/**********************************************************************

  Audacity: A Digital Audio Editor

  GradientButton.cpp

**********************************************************************/

#include "GradientButton.h"

const wxColour darkBlue(68, 137, 247);
const wxColour startColorPressed(59, 129, 231);
const wxColour startColorNormal(75, 145, 247);
const wxColour endColorPressed(38, 106, 214);
const wxColour endColorNormal(54, 122, 246);

constexpr int roundingRadius = 4;
#if defined(__WXMAC__)
constexpr int roundXOffset = 6;
constexpr int roundYOffset = 4;
#else
constexpr int roundXOffset = 0;
constexpr int roundYOffset = 0;
#endif

GradientButton::GradientButton(wxWindow* parent, wxWindowID id, const wxString& label,
                               const wxPoint& pos, const wxSize& size)
    : wxButton(parent, id, label, pos, size) {
    SetBackgroundStyle(wxBG_STYLE_PAINT);
    Bind(wxEVT_PAINT, &GradientButton::OnPaint, this);
    Bind(wxEVT_LEFT_DOWN, &GradientButton::OnMouseDown, this);
    Bind(wxEVT_LEFT_UP, &GradientButton::OnMouseUp, this);
}

void GradientButton::OnPaint(wxPaintEvent& event) {
    wxAutoBufferedPaintDC dc(this);

    wxSize size = GetSize();
    wxColour outline(darkBlue);
    dc.SetPen(wxPen(outline, 1));

    dc.Clear();
    wxGraphicsContext* gc = wxGraphicsContext::Create(dc);
    if (gc) {
        wxColour startColor = m_isPressed ? startColorPressed : startColorNormal;
        wxColour endColor = m_isPressed ? endColorPressed : endColorNormal;

        gc->SetBrush(gc->CreateLinearGradientBrush(
            0, 0, size.GetWidth(), size.GetHeight(), startColor, endColor));
        gc->DrawRoundedRectangle(roundXOffset, roundYOffset, size.GetWidth(), size.GetHeight(), roundingRadius);
        delete gc;
    }

    wxString label = GetLabel();
    wxFont font = GetFont();
    dc.SetFont(font);

    wxSize textSize = dc.GetTextExtent(label);
    int x = (size.GetWidth() - textSize.GetWidth()) / 2 + roundXOffset;
    int y = (size.GetHeight() - textSize.GetHeight()) / 2 + roundYOffset;

    dc.SetTextForeground(*wxWHITE);
    dc.DrawText(label, x, y);
}

void GradientButton::OnMouseDown(wxMouseEvent& event) {
    m_isPressed = true;
    Refresh();
    event.Skip();
}

void GradientButton::OnMouseUp(wxMouseEvent& event) {
    m_isPressed = false;
    Refresh();
    event.Skip();
}
