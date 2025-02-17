/**********************************************************************

  Audacity: A Digital Audio Editor

  ImageRoll.h

  Dominic Mazzoni


**********************************************************************/

#ifndef __AUDACITY_IMAGE_ROLL__
#define __AUDACITY_IMAGE_ROLL__

#include <vector>
// #include <wx/dc.h> // for enum wxRasterOperationMode
#include <wx/defs.h>
#include "wxPanelWrapper.h" // to inherit

#if !wxCHECK_VERSION(3, 0, 0)
#define wxRasterOperationMode int
#endif

// wxImage copies cheaply with reference counting
using ImageArray = std::vector<wxImage>;

class AUDACITY_DLL_API ImageRoll
{
public:
    enum RollType {
        Uninitialized,
        FixedImage,
        HorizontalRoll,
        VerticalRoll,
        Frame
    };

    ImageRoll();
    ImageRoll(const wxImage& src);
    ImageRoll(RollType type, const wxImage& src, wxColour magicColor);
    ImageRoll(const ImageRoll&);
    ImageRoll& operator =(const ImageRoll&);
    ~ImageRoll();

    bool Ok() const;

    wxSize GetMinSize() const { return mMinSize; }
    wxSize GetMaxSize() const { return mMaxSize; }

    void Draw(wxDC& dc, wxRect rect, int /* wxRasterOperationMode */ logicalFunc);
    void Draw(wxDC& dc, wxRect rect); // default logicalFunc to wxCOPY

    static ImageArray SplitH(const wxImage& src, wxColour magicColor);
    static ImageArray SplitV(const wxImage& src, wxColour magicColor);

protected:

    void DrawBitmap(wxDC& dc, wxBitmap& bitmap, int x, int y, int /* wxRasterOperationMode */ logicalFunc);

    void Init(RollType type, const wxImage& src, wxColour magicColor);

    RollType mType;
    // wxBitmap copies cheaply with reference counting
    std::vector<wxBitmap> mPieces;
    wxSize mMinSize;
    wxSize mMaxSize;
};

#endif // __AUDACITY_IMAGE_ROLL__
