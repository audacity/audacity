/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "ArrowButton.h"

wxBEGIN_EVENT_TABLE(ArrowButton, wxPanel)
    EVT_PAINT(ArrowButton::OnPaint)
    EVT_LEFT_UP(ArrowButton::OnMouseClick)
wxEND_EVENT_TABLE()

ArrowButton::ArrowButton(wxWindow* parent, ArrowDirection direction)
    : wxPanel(parent, wxID_ANY, wxDefaultPosition, wxSize(48, 48)),
      m_direction(direction)
{
   SetBackgroundStyle(wxBG_STYLE_PAINT);
   SetCursor(wxCursor(wxCURSOR_HAND));
}

void ArrowButton::SetClickHandler(std::function<void()> handler)
{
   m_onClick = std::move(handler);
}

void ArrowButton::OnMouseClick(wxMouseEvent&)
{
   if (m_onClick) {
      m_onClick();
   }
}

void ArrowButton::OnPaint(wxPaintEvent&)
{
   wxAutoBufferedPaintDC dc(this);
   dc.Clear();

   // button background
   dc.SetBrush(wxBrush(wxColour(207, 217, 239)));
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.DrawRoundedRectangle(GetClientRect(), 4);

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
}
