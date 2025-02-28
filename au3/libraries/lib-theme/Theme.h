/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_THEME__
#define __AUDACITY_THEME__

#include <map>
#include <unordered_set>
#include <vector>
#include <optional>
#include <wx/arrstr.h>
#include <wx/defs.h>
#include <wx/gdicmn.h>
#include "ComponentInterfaceSymbol.h"

#include "Observer.h"
#include "Prefs.h"

//! A choice of theme such as "Light", "Dark", ...
using teThemeType = Identifier;

//! A system theme, that matches selected theme best (only works on macOS with builtin themes).
enum class PreferredSystemAppearance
{
    Light,
    Dark,
    HighContrastDark
};

class wxArrayString;
class wxBitmap;
class wxColour;
class wxImage;
class wxPen;

class ChoiceSetting;

// JKC: will probably change name from 'teBmps' to 'tIndexBmp';
using teBmps = int; /// The index of a bitmap resource in Theme Resources.

enum teResourceType
{
    resTypeColour,
    resTypeBitmap,
    resTypeImage = resTypeBitmap,
};

enum teResourceFlags
{
    resFlagNone   =0x00,
    resFlagPaired =0x01,
    resFlagCursor =0x02,
    resFlagNewLine = 0x04,
    resFlagInternal = 0x08, // For image manipulation.  Don't save or load.
    resFlagSkip = 0x10
};

//! A cursor for iterating the theme bitmap
class THEME_API FlowPacker
{
public:
    explicit FlowPacker(int width);
    ~FlowPacker() {}
    void GetNextPosition(int xSize, int ySize);
    void SetNewGroup(int iGroupSize);
    void SetColourGroup();
    wxRect Rect();
    wxRect RectInner();
    void RectMid(int& x, int& y);

    // These 4 should become private again...
    int mFlags = resFlagPaired;
    int mxPos = 0;
    int myPos = 0;
    int myHeight = 0;
    int mBorderWidth = 1;

private:
    int iImageGroupSize = 1;
    int iImageGroupIndex = -1;
    int mOldFlags = resFlagPaired;
    int myPosBase = 0;
    int mxCacheWidth = 0;

    int mComponentWidth = 0;
    int mComponentHeight = 0;
};

struct ThemeSet
{
    // wxImage, wxBitmap copy cheaply using reference counting
    std::vector<wxImage> mImages;
    std::vector<wxBitmap> mBitmaps;
    std::vector<wxColour> mColours;

    bool bInitialised = false;
};

struct ThemeChangeMessage {
    std::optional<PreferredSystemAppearance> appearance; /*!<
      An enum value when preferred system appearance changes, or nullopt
      for change of the image set */
};

class THEME_API ThemeBase /* not final */ : public Observer::Publisher<ThemeChangeMessage>
{
public:
    ThemeBase(void);
    ThemeBase (const ThemeBase&) = delete;
    ThemeBase& operator =(const ThemeBase&) = delete;
public:
    virtual ~ThemeBase(void);

public:
    virtual void EnsureInitialised() = 0;

    // Get and set the root directory for saving and loading of files
    FilePath GetFilePath();
    void SetFilePath(const FilePath& path);

    // Typically statically constructed:
    struct THEME_API RegisteredTheme {
        RegisteredTheme(EnumValueSymbol symbol, PreferredSystemAppearance preferredSystemAppearance, const std::vector<unsigned char>& data /*!<
            A reference to this vector is stored, not a copy of it! */
                        );
        ~RegisteredTheme();

        const EnumValueSymbol symbol;
        const PreferredSystemAppearance preferredSystemAppearance;
        const std::vector<unsigned char>& data;
    };

    void SwitchTheme(teThemeType Theme);
    void LoadTheme(teThemeType Theme);

    // For checking uniqueness of names during registration
    using NameSet = std::unordered_set<wxString>;

    void RegisterImage(NameSet& allNames, int& flags, int& iIndex, char const** pXpm, const wxString& Name);
    void RegisterImage(NameSet& allNames, int& flags, int& iIndex, const wxImage& Image, const wxString& Name);
    void RegisterColour(NameSet& allNames, int& iIndex, const wxColour& Clr, const wxString& Name);

    teThemeType GetFallbackThemeType();
    void CreateImageCache();
    bool CreateOneImageCache(teThemeType id, bool bBinarySave);
    bool ReadImageCache(teThemeType type = {}, bool bOkIfNotFound=false);
    void LoadThemeComponents(bool bOkIfNotFound =false);
    void LoadOneThemeComponents(teThemeType id, bool bOkIfNotFound = false);
    void SaveThemeComponents();
    bool SaveOneThemeComponents(teThemeType id);
    void SaveThemeAsCode();
    void WriteImageDefs();
    void WriteImageMap();
    void WriteOneImageMap(teThemeType id);
    static bool LoadPreferredTheme();
    void RecolourBitmap(int iIndex, wxColour From, wxColour To);

    int ColourDistance(wxColour& From, wxColour& To);
    wxColour& Colour(int iIndex);
    wxBitmap& Bitmap(int iIndex);
    wxImage& Image(int iIndex);
    wxSize ImageSize(int iIndex);

    void ReplaceImage(int iIndex, wxImage* pImage);
    void RotateImageInto(int iTo, int iFrom, bool bClockwise);

    void SetBrushColour(wxBrush& Brush, int iIndex);
    void SetPenColour(wxPen& Pen, int iIndex);

    // Utility function that combines a bitmap and a mask, both in XPM format.
    wxImage MaskedImage(char const** pXpm, char const** pMask);
    // Utility function that takes a 32 bit bitmap and makes it into an image.
    wxImage MakeImageWithAlpha(wxBitmap& Bmp);

    // Reclaim resources after finished with theme editing
    void DeleteUnusedThemes();

protected:
    FilePath mThemeDir;

    wxArrayString mBitmapNames;
    std::vector<int> mBitmapFlags;
    wxArrayString mColourNames;

    PreferredSystemAppearance mPreferredSystemAppearance { PreferredSystemAppearance::Light };

    std::map<Identifier, ThemeSet> mSets;
    ThemeSet* mpSet = nullptr;
};

class THEME_API Theme final : public ThemeBase
{
    friend class AColor; // So it can publish
public:
    Theme(void);
public:
    ~Theme(void);
public:
    void EnsureInitialised() override;
    void RegisterImagesAndColours();
};

extern THEME_API Theme theTheme;

extern THEME_API ChoiceSetting
& GUITheme()
;

#endif // __AUDACITY_THEME__
