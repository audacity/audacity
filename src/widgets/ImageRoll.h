/**********************************************************************

  Audacity: A Digital Audio Editor

  ImageRoll.h

  Dominic Mazzoni


**********************************************************************/

#ifndef __AUDACITY_IMAGE_ROLL__
#define __AUDACITY_IMAGE_ROLL__

#include <wx/defs.h>
#include <wx/dynarray.h>
#include <wx/panel.h>

WX_DECLARE_OBJARRAY(wxBitmap, BitmapArray);
WX_DECLARE_OBJARRAY(wxImage, ImageArray);

class ImageRoll
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
   ImageRoll(const wxImage &src);
   ImageRoll(RollType type, const wxImage &src, wxColour magicColor);

   bool Ok() const;

   wxSize GetMinSize() const { return mMinSize; }
   wxSize GetMaxSize() const { return mMaxSize; }

   void Draw(wxDC &dc, wxRect rect,
             int logicalFunc = wxCOPY);

   static ImageArray SplitH(const wxImage &src, wxColour magicColor);
   static ImageArray SplitV(const wxImage &src, wxColour magicColor);

 protected:

   void DrawBitmap(wxDC &dc, wxBitmap &bitmap,
                   int x, int y, int logicalFunc = wxCOPY);

   void Init(RollType type, const wxImage &src, wxColour magicColor);

   RollType     mType;
   BitmapArray  mPieces;
   wxSize       mMinSize;
   wxSize       mMaxSize;
};

// A very simple class that just display an ImageRoll that doesn't
// do anything
class ImageRollPanel : public wxPanel
{
 public:
   DECLARE_DYNAMIC_CLASS(ImageRollPanel);

   ImageRollPanel(wxWindow *parent,
                  wxWindowID id,
                  ImageRoll &imgRoll,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize,
                  long style = wxTAB_TRAVERSAL);

   void SetLogicalFunction(int func);

   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);

 protected:
   ImageRoll mImageRoll;

   int mLogicalFunction;

   DECLARE_EVENT_TABLE();

};

#endif // __AUDACITY_IMAGE_ROLL__
