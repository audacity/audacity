/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "ArrowButton.h"

wxBEGIN_EVENT_TABLE(ArrowButton, wxButton)
    EVT_PAINT(ArrowButton::OnPaint)
    EVT_BUTTON(wxID_ANY, ArrowButton::OnButtonClick)
wxEND_EVENT_TABLE()

constexpr int roundingRadius = 4;
#if defined(__WXMAC__)
constexpr int roundXOffset = 6;
constexpr int roundYOffset = 4;
#else
constexpr int roundXOffset = 0;
constexpr int roundYOffset = 0;
#endif

ArrowButton::ArrowButton(wxWindow* parent, ArrowDirection direction)
    : wxButton(parent, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(48, 48)),
      m_direction(direction)
{
   SetBackgroundStyle(wxBG_STYLE_PAINT);
   SetCursor(wxCursor(wxCURSOR_HAND));
}

void ArrowButton::SetClickHandler(std::function<void()> handler)
{
   m_onClick = std::move(handler);
}

void ArrowButton::OnButtonClick(wxCommandEvent&)
{
   if (m_onClick) {
      m_onClick();
   }
}

void ArrowButton::OnPaint(wxPaintEvent&)
{
   wxSize size = GetSize();
   wxAutoBufferedPaintDC dc(this);
   dc.Clear();

   // button background
   dc.SetBrush(wxBrush(wxColour(207, 217, 239)));
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.DrawRoundedRectangle(roundXOffset, roundYOffset, size.GetWidth(), size.GetHeight(), roundingRadius);

   // draw arrow (triangle)
   wxPoint center = wxPoint(GetClientSize().GetWidth() / 2, GetClientSize().GetHeight() / 2);
   wxPoint points[3];

   if (m_direction == ArrowDirection::Left) {
      points[0] = { center.x + 6, center.y - 6 };
      points[1] = { center.x - 6, center.y };
      points[2] = { center.x + 6, center.y + 6 };
   } else {
      points[0] = { center.x - 6, center.y - 6 };
      points[1] = { center.x + 6, center.y };
      points[2] = { center.x - 6, center.y + 6 };
   }

   dc.SetBrush(*wxBLACK_BRUSH);
   dc.DrawPolygon(3, points);
   
   if (HasFocus()) {
       wxPen dottedPen(*wxBLACK, 1, wxPENSTYLE_DOT);
       dc.SetPen(dottedPen);
       dc.SetBrush(*wxTRANSPARENT_BRUSH);
       dc.DrawRoundedRectangle(roundXOffset + 2, roundYOffset + 2,
          size.GetWidth() - 4, size.GetHeight() - 4,
          roundingRadius);
   }
}
