/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

********************************************************************//**

\class Theme
\brief Based on ThemeBase, Theme manages image and icon resources.

   Theme is a class which manages theme resources.
   It maps sets of ids to the resources and to names of the resources,
   so that they can be loaded/saved from files.

   Theme adds the Audacity specific images to ThemeBase.

\see \ref Themability

*//*****************************************************************//**

\class ThemeBase
\brief Theme management - Image loading and saving.

   Base for the Theme class. ThemeBase is a generic
   non-Audacity specific class.

\see \ref Themability

*//*****************************************************************//**

\class FlowPacker
\brief Packs rectangular boxes into a rectangle, using simple first fit.

This class is currently used by Theme to pack its images into the image
cache.  Perhaps someday we will improve FlowPacker and make it more flexible,
and use it for toolbar and window layouts too.

*//*****************************************************************//**

\class SourceOutputStream
\brief Allows us to capture output of the Save .png and 'pipe' it into
our own output function which gives a series of numbers.

This class is currently used by Theme to pack its images into the image
cache.  Perhaps someday we will improve FlowPacker and make it more flexible,
and use it for toolbar and window layouts too.

*//*****************************************************************/

#include "Theme.h"

#include <map>

#include <wx/wxprec.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/file.h>
#include <wx/ffile.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/mstream.h>
#include <wx/settings.h>
#include <regex>

#include "AllThemeResources.h"
#include "BasicUI.h"
#include "FileNames.h"
#include "Prefs.h"
#include "ImageManipulation.h"
#include "Internat.h"
#include "MemoryX.h"

// theTheme is a global variable.
THEME_API Theme theTheme;

Theme::Theme(void)
{
}

Theme::~Theme(void)
{
}

namespace {
// Side of a square painted in one of the theme colours in image caches
constexpr int iColSize = 10;

wxString ThemeFilePrefix(teThemeType id)
{
    auto strings = wxSplit(id.GET(), L'-');
    wxString result;
    for (auto& string : strings) {
        result += string.Capitalize();
    }
    return result;
}

//! Has the side-effect of ensuring existence of the directory
FilePath ThemeSubdir(const FilePath& themeDir, Identifier id)
{
    return FileNames::MkDir(
        wxFileName(FileNames::MkDir(themeDir), id.GET()).GetFullPath());
}

//! Has the side-effect of ensuring existence of the directory
FilePath ThemeComponentsDir(const FilePath& themeDir, Identifier id)
{
    return FileNames::MkDir(wxFileName(ThemeSubdir(themeDir, id), wxT("Components")).GetFullPath());
}

constexpr auto ImageCacheFileName = L"ImageCache.png";

constexpr auto ImageMapFileName = L"ImageCache.htm";

constexpr auto ColorFileName = L"Colors.txt";

FilePath ThemeImageDefsAsCee(const FilePath& themeDir)
{
    return wxFileName(themeDir, wxT("ThemeImageDefsAsCee.h")).GetFullPath();
}

constexpr auto ThemeCacheFileName = L"ThemeAsCeeCode.h";

FilePath ThemeComponent(const wxString& dir, const wxString& Str)
{
    return wxFileName(dir, Str, wxT("png")).GetFullPath();
}
}

void Theme::EnsureInitialised()
{
    if (!mpSet || mpSet->bInitialised) {
        return;
    }
    RegisterImagesAndColours();

#ifdef EXPERIMENTAL_EXTRA_THEME_RESOURCES
    extern void RegisterExtraThemeResources();
    RegisterExtraThemeResources();
#endif
}

FilePath ThemeBase::GetFilePath()
{
    if (mThemeDir.empty()) {
        SetFilePath(
            wxFileName(FileNames::DataDir(), wxT("Theme")).GetFullPath());
    }
    return mThemeDir;
}

void ThemeBase::SetFilePath(const FilePath& path)
{
    mThemeDir = path;
}

bool ThemeBase::LoadPreferredTheme()
{
    Identifier theme = GUITheme().Read();
    theTheme.LoadTheme(theme);
    return true;
}

void Theme::RegisterImagesAndColours()
{
    if (!mpSet || mpSet->bInitialised) {
        return;
    }
    mpSet->bInitialised = true;

// This initialises the variables e.g
// RegisterImage( myFlags, bmpRecordButton, some image, wxT("RecordButton"));
    int myFlags = resFlagPaired;
    NameSet allNames;
#define THEME_INITS
#include "AllThemeResources.h"
}

ThemeBase::ThemeBase(void)
{
}

ThemeBase::~ThemeBase(void)
{
}

using ThemeCacheLookup
    =std::map< EnumValueSymbol, const ThemeBase::RegisteredTheme& >;

static ThemeCacheLookup& GetThemeCacheLookup()
{
    static ThemeCacheLookup theMap;
    return theMap;
}

ThemeBase::RegisteredTheme::RegisteredTheme(
    EnumValueSymbol symbol, PreferredSystemAppearance preferredSystemAppearance,
    const std::vector<unsigned char>& data)
    : symbol{symbol}
    , preferredSystemAppearance{preferredSystemAppearance}
    , data{data}
{
    GetThemeCacheLookup().emplace(symbol, *this);
}

ThemeBase::RegisteredTheme::~RegisteredTheme()
{
    GetThemeCacheLookup().erase(symbol);
}

void ThemeBase::SwitchTheme(teThemeType Theme)
{
    // Switch the active theme set
    mpSet = &mSets[Theme.empty() ? GUITheme().Read() : Theme];
    auto& resources = *mpSet;
    EnsureInitialised();

    const bool cbOkIfNotFound = true;

    if (!ReadImageCache(Theme, cbOkIfNotFound)) {
        // THEN get the default set.
        ReadImageCache(GetFallbackThemeType(), !cbOkIfNotFound);

        // JKC: Now we could go on and load the individual images
        // on top of the default images using the commented out
        // code that follows...
        //
        // However, I think it is better to get the user to
        // build a NEW image cache, which they can do easily
        // from the Theme preferences tab.
#if 0
        // and now add any available component images.
        LoadComponents(cbOkIfNotFound);

        // JKC: I'm usure about doing this next step automatically.
        // Suppose the disk is write protected?
        // Is having the image cache created automatically
        // going to confuse users?  Do we need version specific names?
        // and now save the combined image as a cache for later use.
        // We should load the images a little faster in future as a result.
        CreateImageCache();
#endif
    }
}

/// This function is called to load the initial Theme images.
/// It does not though cause the GUI to refresh.
void ThemeBase::LoadTheme(teThemeType Theme)
{
    SwitchTheme(Theme);

    // Post-processing steps after loading of the cache

    // Two always overwritten images
    RotateImageInto(bmpRecordBeside, bmpRecordBelow, false);
    RotateImageInto(bmpRecordBesideDisabled, bmpRecordBelowDisabled, false);

    // Next line is not required as we haven't yet built the GUI
    // when this function is (or should be) called.
    // AColor::ApplyUpdatedImages();

    // The next step doesn't post-process the theme data.  It only modifies
    // system settings.  So it is not necessary to make it conditional on
    // preferences, for avoidance of errors in the use of Theme preferences.
    // (See commit 2020217)
    Publish({ mPreferredSystemAppearance });
}

void ThemeBase::RecolourBitmap(int iIndex, wxColour From, wxColour To)
{
    wxImage Image(Bitmap(iIndex).ConvertToImage());

    std::unique_ptr<wxImage> pResult = ChangeImageColour(
        &Image, From, To);
    ReplaceImage(iIndex, pResult.get());
}

int ThemeBase::ColourDistance(wxColour& From, wxColour& To)
{
    return
        abs(From.Red() - To.Red())
        + abs(From.Green() - To.Green())
        + abs(From.Blue() - To.Blue());
}

wxImage ThemeBase::MaskedImage(char const** pXpm, char const** pMask)
{
    wxBitmap Bmp1(pXpm);
    wxBitmap Bmp2(pMask);

//   wxLogDebug( wxT("Image 1: %i Image 2: %i"),
//      Bmp1.GetDepth(), Bmp2.GetDepth() );

    // We want a 24-bit-depth bitmap if all is working, but on some
    // platforms it might just return -1 (which means best available
    // or not relevant).
    // JKC: \todo check that we're not relying on 24 bit elsewhere.
    wxASSERT(Bmp1.GetDepth() == -1 || Bmp1.GetDepth() == 24);
    wxASSERT(Bmp1.GetDepth() == -1 || Bmp2.GetDepth() == 24);

    int i, nBytes;
    nBytes = Bmp1.GetWidth() * Bmp1.GetHeight();
    wxImage Img1(Bmp1.ConvertToImage());
    wxImage Img2(Bmp2.ConvertToImage());

//   unsigned char *src = Img1.GetData();
    unsigned char* mk = Img2.GetData();
    //wxImage::setAlpha requires memory allocated with malloc, not NEW
    MallocString<unsigned char> alpha{
        static_cast<unsigned char*>(malloc(nBytes)) };

    // Extract alpha channel from second XPM.
    for (i=0; i < nBytes; i++) {
        alpha[i] = mk[0];
        mk+=3;
    }

    Img1.SetAlpha(alpha.release());

    //dmazzoni: the top line does not work on wxGTK
    //wxBitmap Result( Img1, 32 );
    //wxBitmap Result( Img1 );

    return Img1;
}

// Legacy function to allow use of an XPM where no theme image was defined.
// Bit depth and mask needs review.
// Note that XPMs don't offer translucency, so unsuitable for a round shape overlay,
// for example.
void ThemeBase::RegisterImage(NameSet& allNames,
                              int& flags, int& iIndex, char const** pXpm, const wxString& Name)
{
    wxBitmap Bmp(pXpm);
    wxImage Img(Bmp.ConvertToImage());
    // The next line recommended by http://forum.audacityteam.org/viewtopic.php?f=50&t=96765
    Img.SetMaskColour(0xDE, 0xDE, 0xDE);
    Img.InitAlpha();

    //dmazzoni: the top line does not work on wxGTK
    //wxBitmap Bmp2( Img, 32 );
    //wxBitmap Bmp2( Img );

    RegisterImage(allNames, flags, iIndex, Img, Name);
}

void ThemeBase::RegisterImage(NameSet& allNames,
                              int& flags, int& iIndex, const wxImage& Image, const wxString& Name)
{
    auto& resources = *mpSet;
    resources.mImages.push_back(Image);

#ifdef __APPLE__
    // On Mac, bitmaps with alpha don't work.
    // So we convert to a mask and use that.
    // It isn't quite as good, as alpha gives smoother edges.
    //[Does not affect the large control buttons, as for those we do
    // the blending ourselves anyway.]
    wxImage TempImage(Image);
    TempImage.ConvertAlphaToMask();
    resources.mBitmaps.push_back(wxBitmap(TempImage));
#else
    resources.mBitmaps.push_back(wxBitmap(Image));
#endif

    flags &= ~resFlagSkip;
    auto index = resources.mBitmaps.size() - 1;
    if (iIndex == -1) {
        // First time assignment of global variable identifying an image
        iIndex = index;
        mBitmapNames.push_back(Name);
        mBitmapFlags.push_back(flags);
        wxASSERT(allNames.insert(Name).second);
    } else {
        // If revisiting for another theme set,
        // images should be re-done in the same sequence
        wxASSERT(iIndex == index);
        wxASSERT(mBitmapNames[index] == Name);
        wxASSERT(mBitmapFlags[index] == flags);
    }
}

void ThemeBase::RegisterColour(NameSet& allNames,
                               int& iIndex, const wxColour& Clr, const wxString& Name)
{
    auto& resources = *mpSet;
    resources.mColours.push_back(Clr);
    auto index = resources.mColours.size() - 1;
    if (iIndex == -1) {
        // First time assignment of global variable identifying a colour
        iIndex = index;
        mColourNames.push_back(Name);
        wxASSERT(allNames.insert(Name).second);
    } else {
        // If revisiting for another theme set,
        // colours should be re-done in the same sequence
        wxASSERT(iIndex == index);
        wxASSERT(mColourNames[index] == Name);
    }
}

FlowPacker::FlowPacker(int width)
    : mxCacheWidth{width}
{
    SetNewGroup(1);
}

void FlowPacker::SetNewGroup(int iGroupSize)
{
    myPosBase +=myHeight * iImageGroupSize;
    mxPos =0;
    mOldFlags = mFlags;
    iImageGroupSize = iGroupSize;
    iImageGroupIndex = -1;
    mComponentWidth=0;
}

void FlowPacker::SetColourGroup()
{
    myPosBase = 750;
    mxPos =0;
    mOldFlags = mFlags;
    iImageGroupSize = 1;
    iImageGroupIndex = -1;
    mComponentWidth=0;
    myHeight = 11;
}

void FlowPacker::GetNextPosition(int xSize, int ySize)
{
    xSize += 2 * mBorderWidth;
    ySize += 2 * mBorderWidth;
    // if the height has increased, then we are on a NEW group.
    if ((ySize > myHeight) || (((mFlags ^ mOldFlags) & ~resFlagSkip) != 0)) {
        SetNewGroup(((mFlags & resFlagPaired) != 0) ? 2 : 1);
        myHeight = ySize;
//      mFlags &= ~resFlagNewLine;
//      mOldFlags = mFlags;
    }

    iImageGroupIndex++;
    if (iImageGroupIndex == iImageGroupSize) {
        iImageGroupIndex = 0;
        mxPos += mComponentWidth;
    }

    if (mxPos > (mxCacheWidth - xSize)) {
        SetNewGroup(iImageGroupSize);
        iImageGroupIndex++;
        myHeight = ySize;
    }
    myPos = myPosBase + iImageGroupIndex * myHeight;

    mComponentWidth = xSize;
    mComponentHeight = ySize;
}

wxRect FlowPacker::Rect()
{
    return wxRect(mxPos, myPos, mComponentWidth, mComponentHeight);
}

wxRect FlowPacker::RectInner()
{
    return Rect().Deflate(mBorderWidth, mBorderWidth);
}

void FlowPacker::RectMid(int& x, int& y)
{
    x = mxPos + mComponentWidth / 2;
    y = myPos + mComponentHeight / 2;
}

/// \brief Helper class based on wxOutputStream used to get a png file in text format
///
/// The trick used here is that wxWidgets can write a PNG image to a stream.
/// By writing to a custom stream, we get to see each byte of data in turn, convert
/// it to text, put in commas, and then write that out to our own text stream.
class SourceOutputStream final : public wxOutputStream
{
public:
    SourceOutputStream() { }
    int OpenFile(const FilePath& Filename);
    virtual ~SourceOutputStream();

protected:
    size_t OnSysWrite(const void* buffer, size_t bufsize) override;
    wxFile File;
    int nBytes;
};

/// Opens the file and also adds a standard comment at the start of it.
int SourceOutputStream::OpenFile(const FilePath& Filename)
{
    nBytes = 0;
    bool bOk;
    bOk = File.Open(Filename, wxFile::write);
    if (bOk) {
        File.Write(wxString::Format(
                       wxT("///   @file %s\r\n"), wxFileName(Filename).GetFullName()));
        File.Write(wxT("///   @brief This file was Auto-Generated.\r\n"));
        File.Write(wxT("///\r\n"));
        File.Write(wxT("///   It is included by Theme.cpp.\r\n"));
        File.Write(wxT("///   Only check this into Git if you've read and understood the guidelines!\r\n\r\n   "));
    }
    return bOk;
}

/// This is the 'callback' function called with each write of PNG data
/// to the stream.  This is where we conveet to text and add commas.
size_t SourceOutputStream::OnSysWrite(const void* buffer, size_t bufsize)
{
    wxString Temp;
    for (int i=0; i < (int)bufsize; i++) {
        // Write one byte with a comma
        Temp = wxString::Format(wxT("%i,"), (int)(((unsigned char*)buffer)[i]));
        File.Write(Temp);
        nBytes++;
        // New line if more than 20 bytes written since last time.
        if ((nBytes % 20) == 0) {
            File.Write(wxT("\r\n   "));
        }
    }
    return bufsize;
}

/// Destructor.  We close our text stream in here.
SourceOutputStream::~SourceOutputStream()
{
    File.Write(wxT("\r\n"));
    File.Close();
}

// Must be wide enough for bmpAudacityLogo. Use double width + 10.
const int ImageCacheWidth = 440;

const int ImageCacheHeight = 836;

void ThemeBase::CreateImageCache()
{
    ValueRestorer cleanup{ mpSet };
    for (auto&[key, data] : GetThemeCacheLookup()) {
        if (!CreateOneImageCache(key.Internal(), true)) {
            // Some file failed to save, message was given
            return;
        }
    }
    BasicUI::ShowMessageBox(
/* i18n-hint: A theme is a consistent visual style across an application's
graphical user interface, including choices of colors, and similarity of images
such as those on button controls.  Audacity can load and save alternative
themes. */
        XO("Themes written to:\n  %s/*/%s.")
        .Format(GetFilePath(), ImageCacheFileName));
}

bool ThemeBase::CreateOneImageCache(teThemeType id, bool bBinarySave)
{
    SwitchTheme(id);
    auto& resources = *mpSet;

    wxImage ImageCache(ImageCacheWidth, ImageCacheHeight);
    ImageCache.SetRGB(wxRect(0, 0, ImageCacheWidth, ImageCacheHeight), 1, 1, 1);//Not-quite black.

    // Ensure we have an alpha channel...
    if (!ImageCache.HasAlpha()) {
        ImageCache.InitAlpha();
    }

    FlowPacker context{ ImageCacheWidth };

//#define IMAGE_MAP
#ifdef IMAGE_MAP
    wxLogDebug(wxT("<img src=\"ImageCache.png\" usemap=\"#map1\">"));
    wxLogDebug(wxT("<map name=\"map1\">"));
#endif

    // Save the bitmaps
    for (size_t i = 0; i < resources.mImages.size(); ++i) {
        wxImage& SrcImage = resources.mImages[i];
        context.mFlags = mBitmapFlags[i];
        if (!(mBitmapFlags[i] & resFlagInternal)) {
            context.GetNextPosition(SrcImage.GetWidth(), SrcImage.GetHeight());
            ImageCache.SetRGB(context.Rect(), 0xf2, 0xb0, 0x27);
            if (!(context.mFlags & resFlagSkip)) {
                PasteSubImage(&ImageCache, &SrcImage,
                              context.mxPos + context.mBorderWidth,
                              context.myPos + context.mBorderWidth);
            } else {
                ImageCache.SetRGB(context.RectInner(), 1, 1, 1);
            }
#ifdef IMAGE_MAP
            // No href in html.  Uses title not alt.
            wxRect R(context.Rect());
            wxLogDebug(wxT("<area title=\"Bitmap:%s\" shape=rect coords=\"%i,%i,%i,%i\">"),
                       mBitmapNames[i],
                       R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom());
#endif
        }
    }

    // Now save the colours.
    int x, y;

    context.SetColourGroup();
    for (size_t i = 0; i < resources.mColours.size(); ++i) {
        context.GetNextPosition(iColSize, iColSize);
        wxColour c = resources.mColours[i];
        ImageCache.SetRGB(context.Rect(), 0xf2, 0xb0, 0x27);
        ImageCache.SetRGB(context.RectInner(), c.Red(), c.Green(), c.Blue());

        // YUCK!  No function in wxWidgets to set a rectangle of alpha...
        for (x=0; x < iColSize; x++) {
            for (y=0; y < iColSize; y++) {
                ImageCache.SetAlpha(context.mxPos + x, context.myPos + y, 255);
            }
        }
#ifdef IMAGE_MAP
        // No href in html.  Uses title not alt.
        wxRect R(context.Rect());
        wxLogDebug(wxT("<area title=\"Colour:%s\" shape=rect coords=\"%i,%i,%i,%i\">"),
                   mColourNames[i],
                   R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom());
#endif
    }
#if TEST_CARD
    for ( unsigned i = 0; i < ImageCacheWidth; ++i) {
        for ( unsigned j = 0; j < ImageCacheHeight; ++j) {
            int r = j & 0xff;
            int g = i & 0xff;
            int b = (j >> 8) | ((i >> 4) & 0xf0);
            wxRect R(i, j, 1, 1);
            ImageCache.SetRGB(R, r, g, b);
            ImageCache.SetAlpha(i, j, 255);
        }
    }
#endif

#ifdef IMAGE_MAP
    wxLogDebug("</map>");
#endif

    using namespace BasicUI;

    // IF bBinarySave, THEN saving to a normal PNG file.
    if (bBinarySave) {
        auto dir = ThemeSubdir(GetFilePath(), id);
        auto FileName = wxFileName{ dir, ImageCacheFileName }.GetFullPath();

        // Perhaps we should prompt the user if they are overwriting
        // an existing theme cache?
#if 0
        if (wxFileExists(FileName)) {
            auto message
                =//          XO(
//"Theme cache file:\n  %s\nalready exists.\nAre you sure you want to replace it?")
//             .Format( FileName );
                  TranslatableString{ FileName };
            ShowMessageBox(message);
            return false;
        }
#endif
#if 0
        // Deliberate policy to use the fast/cheap blocky pixel-multiplication
        // algorithm, as this introduces no artifacts on repeated scale up/down.
        ImageCache.Rescale(
            ImageCache.GetWidth() * 4,
            ImageCache.GetHeight() * 4,
            wxIMAGE_QUALITY_NEAREST);
#endif
        if (!ImageCache.SaveFile(FileName, wxBITMAP_TYPE_PNG)) {
            ShowMessageBox(
                XO("Audacity could not write file:\n  %s.")
                .Format(FileName));
            return false;
        }
    }
    // ELSE saving to a C code textual version.
    else {
        // Generated header file is not put in the sub-directory for
        // the theme, but is instead distinguished by a prefix on the file name.
        // So the header can simply be copied into the source code tree.
        auto dir = GetFilePath();
        SourceOutputStream OutStream;
        auto name = ThemeFilePrefix(id) + ThemeCacheFileName;
        auto FileName = wxFileName{ dir, name }.GetFullPath();
        if (!OutStream.OpenFile(FileName)) {
            ShowMessageBox(
                XO("Audacity could not open file:\n  %s\nfor writing.")
                .Format(FileName));
            return false;
        }
        if (!ImageCache.SaveFile(OutStream, wxBITMAP_TYPE_PNG)) {
            ShowMessageBox(
                XO("Audacity could not write images to file:\n  %s.")
                .Format(FileName));
            return false;
        }
    }
    return true;
}

void ThemeBase::WriteImageMap()
{
    ValueRestorer cleanup{ mpSet };
    for (auto&[key, data] : GetThemeCacheLookup()) {
        WriteOneImageMap(key.Internal());
    }
}

/// Writes an html file with an image map of the ImageCache
/// Very handy for seeing what each part is for.
void ThemeBase::WriteOneImageMap(teThemeType id)
{
    SwitchTheme(id);
    auto& resources = *mpSet;

    FlowPacker context{ ImageCacheWidth };

    auto dir = ThemeSubdir(GetFilePath(), id);
    auto FileName = wxFileName{ dir, ImageMapFileName }.GetFullPath();
    wxFFile File(FileName, wxT("wb")); // I'll put in NEW lines explicitly.
    if (!File.IsOpened()) {
        return;
    }

    File.Write(wxT("<html>\r\n"));
    File.Write(wxT("<body bgcolor=\"303030\">\r\n"));
    wxString Temp = wxString::Format(wxT("<img src=\"ImageCache.png\" width=\"%i\" usemap=\"#map1\">\r\n"), ImageCacheWidth);
    File.Write(Temp);
    File.Write(wxT("<map name=\"map1\">\r\n"));

    for (size_t i = 0; i < resources.mImages.size(); ++i) {
        wxImage& SrcImage = resources.mImages[i];
        context.mFlags = mBitmapFlags[i];
        if (!(mBitmapFlags[i] & resFlagInternal)) {
            context.GetNextPosition(SrcImage.GetWidth(), SrcImage.GetHeight());
            // No href in html.  Uses title not alt.
            wxRect R(context.RectInner());
            File.Write(wxString::Format(
                           wxT("<area title=\"Bitmap:%s\" shape=rect coords=\"%i,%i,%i,%i\">\r\n"),
                           mBitmapNames[i],
                           R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom()));
        }
    }
    // Now save the colours.
    context.SetColourGroup();
    for (size_t i = 0; i < resources.mColours.size(); ++i) {
        context.GetNextPosition(iColSize, iColSize);
        // No href in html.  Uses title not alt.
        wxRect R(context.RectInner());
        File.Write(wxString::Format(wxT("<area title=\"Colour:%s\" shape=rect coords=\"%i,%i,%i,%i\">\r\n"),
                                    mColourNames[i],
                                    R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom()));
    }
    File.Write(wxT("</map>\r\n"));
    File.Write(wxT("</body>\r\n"));
    File.Write(wxT("</html>\r\n"));
    // File will be closed automatically.
}

/// Writes a series of Macro definitions that can be used in the include file.
void ThemeBase::WriteImageDefs()
{
    // The generated image defs macro calls depend only on sizes of images,
    // not contents, and so there is only one file good across all themes.

    auto& resources = *mpSet;
    EnsureInitialised();

    wxFFile File(ThemeImageDefsAsCee(GetFilePath()), wxT("wb"));
    if (!File.IsOpened()) {
        return;
    }
    teResourceFlags PrevFlags = (teResourceFlags) - 1;
    for (size_t i = 0; i < resources.mImages.size(); ++i) {
        wxImage& SrcImage = resources.mImages[i];
        // No href in html.  Uses title not alt.
        if (PrevFlags != mBitmapFlags[i]) {
            PrevFlags = (teResourceFlags)mBitmapFlags[i];
            int t = (int)PrevFlags;
            wxString Temp;
            if (t == 0) {
                Temp = wxT(" resFlagNone ");
            }
            if (t & resFlagPaired) {
                Temp += wxT(" resFlagPaired ");
            }
            if (t & resFlagCursor) {
                Temp += wxT(" resFlagCursor ");
            }
            if (t & resFlagNewLine) {
                Temp += wxT(" resFlagNewLine ");
            }
            if (t & resFlagInternal) {
                Temp += wxT(" resFlagInternal ");
            }
            Temp.Replace(wxT("  "), wxT(" | "));

            File.Write(wxString::Format(wxT("\r\n   SET_THEME_FLAGS( %s );\r\n"),
                                        Temp));
        }
        File.Write(wxString::Format(
                       wxT("   DEFINE_IMAGE( bmp%s, wxImage( %i, %i ), wxT(\"%s\"));\r\n"),
                       mBitmapNames[i],
                       SrcImage.GetWidth(),
                       SrcImage.GetHeight(),
                       mBitmapNames[i]
                       ));
    }
}

teThemeType ThemeBase::GetFallbackThemeType()
{
// Fallback must be an internally supported type,
// to guarantee it is found.
    return "light";
}

/// Reads an image cache including images, cursors and colours.
/// @param type if empty means read from an external binary file.
///   otherwise the data is taken from a block of memory.
/// @param bOkIfNotFound if true means do not report absent file.
/// @return true iff we loaded the images.
bool ThemeBase::ReadImageCache(teThemeType type, bool bOkIfNotFound)
{
    auto& resources = *mpSet;
    EnsureInitialised();
    wxImage ImageCache;

    // Ensure we have an alpha channel...
//   if( !ImageCache.HasAlpha() )
//   {
//      ImageCache.InitAlpha();
//   }

    using namespace BasicUI;

    if (type.empty() || type == "custom") {
        mPreferredSystemAppearance = PreferredSystemAppearance::Light;

        // Take the image cache file for the theme chosen in preferences
        auto dir = ThemeSubdir(GetFilePath(), GUITheme().Read());
        const auto& FileName
            =wxFileName{ dir, ImageCacheFileName }.GetFullPath();
        if (!wxFileExists(FileName)) {
            if (bOkIfNotFound) {
                return false; // did not load the images, so return false.
            }
            ShowMessageBox(
                XO("Audacity could not find file:\n  %s.\nTheme not loaded.")
                .Format(FileName));
            return false;
        }
        if (!ImageCache.LoadFile(FileName, wxBITMAP_TYPE_PNG)) {
            ShowMessageBox(
                /* i18n-hint: Do not translate png.  It is the name of a file format.*/
                XO("Audacity could not load file:\n  %s.\nBad png format perhaps?")
                .Format(FileName));
            return false;
        }
    }
    // ELSE we are reading from internal storage.
    else {
        size_t ImageSize = 0;
        const unsigned char* pImage = nullptr;
        auto& lookup = GetThemeCacheLookup();
        auto iter = lookup.find({ type, {} });
        if (const auto end = lookup.end(); iter == end) {
            iter = lookup.find({ "classic", {} });
            wxASSERT(iter != end);
        }

        mPreferredSystemAppearance = iter->second.preferredSystemAppearance;

        ImageSize = iter->second.data.size();
        if (ImageSize == 0) {
            // This must be the image compiler
            return true;
        }

        pImage = iter->second.data.data();
        //wxLogDebug("Reading ImageCache %p size %i", pImage, ImageSize );
        wxMemoryInputStream InternalStream(pImage, ImageSize);

        if (!ImageCache.LoadFile(InternalStream, wxBITMAP_TYPE_PNG)) {
            // If we get this message, it means that the data in file
            // was not a valid png image.
            // Most likely someone edited it by mistake,
            // Or some experiment is being tried with NEW formats for it.
            ShowMessageBox(
                XO(
                    "Audacity could not read its default theme.\nPlease report the problem."));
            return false;
        }
        //wxLogDebug("Read %i by %i", ImageCache.GetWidth(), ImageCache.GetHeight() );
    }

    // Resize a large image down.
    if (ImageCache.GetWidth() > ImageCacheWidth) {
        int h = ImageCache.GetHeight() * ((1.0 * ImageCacheWidth) / ImageCache.GetWidth());
        ImageCache.Rescale(ImageCacheWidth, h);
    }
    FlowPacker context{ ImageCacheWidth };
    // Load the bitmaps
    for (size_t i = 0; i < resources.mImages.size(); ++i) {
        wxImage& Image = resources.mImages[i];
        context.mFlags = mBitmapFlags[i];
        if (!(mBitmapFlags[i] & resFlagInternal)) {
            context.GetNextPosition(Image.GetWidth(), Image.GetHeight());
            wxRect R = context.RectInner();
            //wxLogDebug( "[%i, %i, %i, %i, \"%s\"], ", R.x, R.y, R.width, R.height, mBitmapNames[i].c_str() );
            Image = GetSubImageWithAlpha(ImageCache, context.RectInner());
            resources.mBitmaps[i] = wxBitmap(Image);
        }
    }
    if (!ImageCache.HasAlpha()) {
        ImageCache.InitAlpha();
    }

//   return true; //To not load colours..
    // Now load the colours.
    int x, y;
    context.SetColourGroup();
    wxColour TempColour;
    for (size_t i = 0; i < resources.mColours.size(); ++i) {
        context.GetNextPosition(iColSize, iColSize);
        context.RectMid(x, y);
        wxRect R = context.RectInner();
        //wxLogDebug( "[%i, %i, %i, %i, \"%s\"], ", R.x, R.y, R.width, R.height, mColourNames[i].c_str() );
        // Only change the colour if the alpha is opaque.
        // This allows us to add NEW colours more easily.
        if (ImageCache.GetAlpha(x, y) > 128) {
            TempColour = wxColour(
                ImageCache.GetRed(x, y),
                ImageCache.GetGreen(x, y),
                ImageCache.GetBlue(x, y));
            /// \todo revisit this hack which makes adding NEW colours easier
            /// but which prevents a colour of (1,1,1) from being added.
            /// find an alternative way to make adding NEW colours easier.
            /// e.g. initialise the background to translucent, perhaps.
            if (TempColour != wxColour(1, 1, 1)) {
                resources.mColours[i] = TempColour;
            }
        }
    }
    return true;
}

void ThemeBase::LoadThemeComponents(bool bOkIfNotFound)
{
    ValueRestorer cleanup{ mpSet };
    for (auto&[key, data] : GetThemeCacheLookup()) {
        LoadOneThemeComponents(key.Internal(), bOkIfNotFound);
    }
}

void ThemeBase::LoadOneThemeComponents(teThemeType id, bool bOkIfNotFound)
{
    SwitchTheme(id);
    auto& resources = *mpSet;
    // IF directory doesn't exist THEN return early.
    const auto dir = ThemeComponentsDir(GetFilePath(), id);
    if (!wxDirExists(dir)) {
        return;
    }

    using namespace BasicUI;

    int n=0;
    FilePath FileName;
    for (size_t i = 0; i < resources.mImages.size(); ++i) {
        if (!(mBitmapFlags[i] & resFlagInternal)) {
            FileName = ThemeComponent(dir, mBitmapNames[i]);
            if (wxFileExists(FileName)) {
                if (!resources.mImages[i].LoadFile(FileName, wxBITMAP_TYPE_PNG)) {
                    ShowMessageBox(
                        XO(
                            /* i18n-hint: Do not translate png.  It is the name of a file format.*/
                            "Audacity could not load file:\n  %s.\nBad png format perhaps?")
                        .Format(FileName));
                    return;
                }
                /// JKC: \bug (wxWidgets) A png may have been saved with alpha, but when you
                /// load it, it comes back with a mask instead!  (well I guess it is more
                /// efficient).  Anyway, we want alpha and not a mask, so we call InitAlpha,
                /// and that transfers the mask into the alpha channel, and we're done.
                if (!resources.mImages[i].HasAlpha()) {
                    // wxLogDebug( wxT("File %s lacked alpha"), mBitmapNames[i] );
                    resources.mImages[i].InitAlpha();
                }
                resources.mBitmaps[i] = wxBitmap(resources.mImages[i]);
                n++;
            }
        }
    }

    // Now read complete information about the colors from one text file
    {
        const auto fName = wxFileName{ dir, ColorFileName }.GetFullPath();
        wxTextFile file{ fName };
        file.Open();
        if (!file.IsOpened()) {
            ShowMessageBox(XO("Couldn't read from file: %s").Format(fName));
        } else {
            ++n;
            // Scan the line for name and #xxxxxx;
            static const std::wregex expr{
                LR"(^ *([_[:alnum:]]+).*#([0-9a-fA-F]{6});)" };
            const auto begin = mColourNames.begin(),
                       end = mColourNames.end();
            std::unordered_set<wxString> names;
            for (auto str = file.GetFirstLine();
                 !file.Eof(); str = file.GetNextLine()) {
                if (std::wsmatch match;
                    regex_search(str.ToStdWstring(), match, expr)) {
                    const wxString name{ match[1] };
                    if (!names.insert(name).second) {
                        ShowMessageBox(Verbatim("Ignoring duplicate color name: %s")
                                       .Format(name));
                    } else if (const auto iter = std::find(begin, end, name);
                               iter == end) {
                        ShowMessageBox(
                            Verbatim("Unrecognized color name: %s").Format(name));
                    } else {
                        auto rrggbb
                            =static_cast<unsigned>(stoi(match[2], nullptr, 16));
                        unsigned char rr = (rrggbb >> 16) & 0xffu;
                        unsigned char gg = (rrggbb >> 8) & 0xffu;
                        unsigned char bb = rrggbb & 0xffu;
                        resources.mColours[iter - begin] = { rr, gg, bb };
                    }
                }
            }
        }
    }

    if (n == 0) {
        if (bOkIfNotFound) {
            return;
        }
        ShowMessageBox(
            XO(
                "None of the expected theme component files\n were found in:\n  %s.")
            .Format(dir));
    }
}

void ThemeBase::SaveThemeComponents()
{
    ValueRestorer cleanup{ mpSet };
    for (auto&[key, data] : GetThemeCacheLookup()) {
        if (!SaveOneThemeComponents(key.Internal())) {
            // Some file failed to save, message was given
            return;
        }
    }
    BasicUI::ShowMessageBox(
        XO("Themes written to:\n  %s/*/Components/.").Format(GetFilePath()));
}

bool ThemeBase::SaveOneThemeComponents(teThemeType id)
{
    using namespace BasicUI;
    SwitchTheme(id);
    auto& resources = *mpSet;
    // IF directory doesn't exist THEN create it
    const auto dir = ThemeComponentsDir(GetFilePath(), id);
    if (!wxDirExists(dir)) {
        /// \bug 1 in wxWidgets documentation; wxMkDir returns false if
        /// directory didn't exist, even if it successfully creates it.
        /// so we create and then test if it exists instead.
        /// \bug 2 in wxWidgets documentation; wxMkDir has only one argument
        /// under MSW
#ifdef __WXMSW__
        wxMkDir(dir.fn_str());
#else
        wxMkDir(dir.fn_str(), 0700);
#endif
        if (!wxDirExists(dir)) {
            ShowMessageBox(
                XO("Could not create directory:\n  %s")
                .Format(dir));
            return false;
        }
    }

    int n=0;
    FilePath FileName;
    for (size_t i = 0; i < resources.mImages.size(); ++i) {
        if (!(mBitmapFlags[i] & resFlagInternal)) {
            FileName = ThemeComponent(dir, mBitmapNames[i]);
            if (wxFileExists(FileName)) {
                ++n;
                break;
            }
        }
    }

    if (wxFileExists(ThemeComponent(dir, ColorFileName))) {
        ++n;
    }

    using namespace BasicUI;

    if (n > 0) {
        auto result
            =ShowMessageBox(
                  XO(
                      "Some required files in:\n  %s\nwere already present. Overwrite?")
                  .Format(dir),
                  MessageBoxOptions {}
                  .ButtonStyle(Button::YesNo)
                  .DefaultIsNo());
        if (result == MessageBoxResult::No) {
            return false;
        }
    }

    for (size_t i = 0; i < resources.mImages.size(); ++i) {
        if (!(mBitmapFlags[i] & resFlagInternal)) {
            FileName = ThemeComponent(dir, mBitmapNames[i]);
            if (!resources.mImages[i].SaveFile(FileName, wxBITMAP_TYPE_PNG)) {
                ShowMessageBox(
                    XO("Audacity could not save file:\n  %s")
                    .Format(FileName));
                return false;
            }
        }
    }

    // Now write complete information about the colors in one text file
    {
        const auto fName = wxFileName{ dir, ColorFileName }.GetFullPath();
        wxFileOutputStream ffStream{ fName };
        if (!ffStream.IsOk()) {
            ShowMessageBox(XO("Couldn't write to file: %s").Format(fName));
            return false;
        }
        wxTextOutputStream ss{ ffStream };
        // Open with, for instance, ".darkTheme {"
        ss << "." << id.GET() << "Theme {\n";
        for (size_t i = 0; i < resources.mColours.size(); ++i) {
            const auto& colour = resources.mColours[i];
            ss
            // Write names, aligning the colons in a column,
            // followed by #rrggbb;
                << wxString::Format("%30s: #", mColourNames[i])
                << wxString::Format("%02x", colour.Red())
                << wxString::Format("%02x", colour.Green())
                << wxString::Format("%02x", colour.Blue())
                << ";\n";
        }
        ss << "}";
    }

    return true;
}

void ThemeBase::SaveThemeAsCode()
{
    ValueRestorer cleanup{ mpSet };
    for (auto&[key, data] : GetThemeCacheLookup()) {
        // false indicates not using standard binary method.
        if (!CreateOneImageCache(key.Internal(), false)) {
            // Some file failed to save, message was given
            return;
        }
    }
    BasicUI::ShowMessageBox(
        /* i18n-hint "Cee" means the C computer programming language */
        XO("Themes as Cee code written to:\n  %s/*%s.")
        .Format(GetFilePath(), ThemeCacheFileName));
}

wxImage ThemeBase::MakeImageWithAlpha(wxBitmap& Bmp)
{
    // BUG in wxWidgets.  Conversion from BMP to image does not preserve alpha.
    wxImage image(Bmp.ConvertToImage());
    return image;
}

void ThemeBase::DeleteUnusedThemes()
{
    auto iter = mSets.begin(), end = mSets.end();
    while (iter != end) {
        if (mpSet == &iter->second) {
            ++iter;
        } else {
            iter = mSets.erase(iter);
        }
    }
}

wxColour& ThemeBase::Colour(int iIndex)
{
    wxASSERT(iIndex >= 0);
    auto& resources = *mpSet;
    EnsureInitialised();
    return resources.mColours[iIndex];
}

void ThemeBase::SetBrushColour(wxBrush& Brush, int iIndex)
{
    wxASSERT(iIndex >= 0);
    Brush.SetColour(Colour(iIndex));
}

void ThemeBase::SetPenColour(wxPen& Pen, int iIndex)
{
    wxASSERT(iIndex >= 0);
    Pen.SetColour(Colour(iIndex));
}

wxBitmap& ThemeBase::Bitmap(int iIndex)
{
    wxASSERT(iIndex >= 0);
    auto& resources = *mpSet;
    EnsureInitialised();
    return resources.mBitmaps[iIndex];
}

wxImage& ThemeBase::Image(int iIndex)
{
    wxASSERT(iIndex >= 0);
    auto& resources = *mpSet;
    EnsureInitialised();
    return resources.mImages[iIndex];
}

wxSize ThemeBase::ImageSize(int iIndex)
{
    wxASSERT(iIndex >= 0);
    auto& resources = *mpSet;
    EnsureInitialised();
    wxImage& Image = resources.mImages[iIndex];
    return wxSize(Image.GetWidth(), Image.GetHeight());
}

/// Replaces both the image and the bitmap.
void ThemeBase::ReplaceImage(int iIndex, wxImage* pImage)
{
    Image(iIndex) = *pImage;
    Bitmap(iIndex) = wxBitmap(*pImage);
}

void ThemeBase::RotateImageInto(int iTo, int iFrom, bool bClockwise)
{
    wxImage img(theTheme.Bitmap(iFrom).ConvertToImage());
    wxImage img2 = img.Rotate90(bClockwise);
    ReplaceImage(iTo, &img2);
}

ChoiceSetting& GUITheme()
{
    auto symbols = []{
        std::vector<EnumValueSymbol> symbols;

        // Gather registered themes
        for (const auto&[symbol, data] : GetThemeCacheLookup()) {
            symbols.emplace_back(symbol);
        }

        // Sort the names, with built-in themes to the front,
        // conserving the ordering that was used in 3.1.0; otherwise
        // sorting other registered themes alphabetically by identifier
        static const Identifier names[] = {
            "classic", "light", "dark", "high-contrast", "modern"
        };
        static auto index = [](const EnumValueSymbol& symbol){
            auto begin = std::begin(names), end = std::end(names);
            return std::find(begin, end, symbol.Internal()) - begin;
        };
        std::stable_sort(symbols.begin(), symbols.end(),
                         [](auto& a, auto& b){ return index(a) < index(b); });

        // Last, custom
        symbols.emplace_back(
            /* i18n-hint: user defined */
            "custom", XO("Custom")
            );

        return symbols;
    };

    constexpr int defaultTheme = 1; // "light"

    static ChoiceSetting setting {
        wxT("/GUI/Theme"), symbols(), defaultTheme
    };

    return setting;
}
