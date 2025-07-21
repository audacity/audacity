/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/
#pragma once

#include <wx/wx.h>
#include <wx/graphics.h>
#include <wx/dcbuffer.h>

#include "ArrowButton.h"
#include "TranslatableString.h"
#include "GradientButton.h"

struct CarouselSnapshot {
   using Callback = std::function<void()>;

   TranslatableString title;
   wxBitmap bitmap;
   const char* url;
   Callback callback;
   TranslatableString buttonText;
   TranslatableString imageText;

   CarouselSnapshot(
       TranslatableString title,
       wxBitmap bitmap,
       const char* url,
       TranslatableString buttonText = {},
       TranslatableString imageText = {})
       : title(std::move(title))
       , bitmap(std::move(bitmap))
       , url(url)
       , callback(nullptr)
       , buttonText(std::move(buttonText))
       , imageText(std::move(imageText))
   {}

   CarouselSnapshot(
       TranslatableString title,
       wxBitmap bitmap,
       Callback callback,
       TranslatableString buttonText = {},
       TranslatableString imageText = {})
       : title(std::move(title))
       , bitmap(std::move(bitmap))
       , url(nullptr)
       , callback(std::move(callback))
       , buttonText(std::move(buttonText))
       , imageText(std::move(imageText))
   {}

};

class WX_WRAPPERS_API ImageCarousel : public wxPanel {
public:
   ImageCarousel(wxWindow* parent, const std::vector<CarouselSnapshot>& snapshots,
                 wxWindowID winid = wxID_ANY,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize);
   ~ImageCarousel();

private:
   void OnPaint(wxPaintEvent& event);
   void OnResize(wxSizeEvent& event);
   void OnMouseClick(wxMouseEvent& event);

   void OnLeftClicked();
   void OnRightClicked();
   void OnItemActivated();
   void Advance(int direction);

   void DrawTitle(wxDC& dc, const wxSize& size);
   void UpdateButtons();
   void DrawDots(wxDC& dc, const wxSize& size);

   wxWindowID m_id;
   std::vector<CarouselSnapshot> m_snapshots;
   int m_currentIndex = 0;

#if defined (__WXOSX__) || defined(__WXMSW__)
   ArrowButton* m_btnLeft;
   ArrowButton* m_btnRight;
   GradientButton* m_btnMiddle;
#else
   wxButton* m_btnLeft;
   wxButton* m_btnRight;
   wxButton* m_btnMiddle;
#endif

   wxRect m_imageRect;
};
