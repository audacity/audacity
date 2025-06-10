/**********************************************************************

  Audacity: A Digital Audio Editor

  GradientButton.cpp

**********************************************************************/

#include "GradientButton.h"

static const wxColor darkBlue(68, 137, 247);

// TODO move to theme
static const wxColor defaultNormalColorStart(75, 145, 247);
static const wxColor defaultNormalColorEnd(54, 122, 246);

static const wxColor defaultPressedColorStart(59, 129, 231);
static const wxColor defaultPressedColorEnd(38, 106, 214);

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
    : m_normalColorStart(defaultNormalColorStart), m_normalColorEnd(defaultNormalColorEnd),
      m_pressedColorStart(defaultPressedColorStart), m_pressedColorEnd(defaultPressedColorEnd),
      wxButton(parent, id, label, pos, size) {
    SetBackgroundStyle(wxBG_STYLE_PAINT);
    Bind(wxEVT_PAINT, &GradientButton::OnPaint, this);
    Bind(wxEVT_LEFT_DOWN, &GradientButton::OnMouseDown, this);
    Bind(wxEVT_LEFT_UP, &GradientButton::OnMouseUp, this);
}

void GradientButton::OnPaint(wxPaintEvent& event) {
    wxAutoBufferedPaintDC dc(this);

    wxSize size = GetSize();

    dc.SetPen(*wxTRANSPARENT_PEN);
    dc.SetBrush(GetParent()->GetBackgroundColour());
    dc.DrawRectangle(0, 0, size.GetWidth(), size.GetHeight());

    wxColour outline(darkBlue);
    dc.SetPen(wxPen(outline, 1));
    wxGraphicsContext* gc = wxGraphicsContext::Create(dc);
    if (gc) {
        wxColour startColor = m_isPressed ? m_pressedColorStart : m_normalColorStart;
        wxColour endColor = m_isPressed ? m_pressedColorEnd : m_normalColorEnd;

        gc->SetBrush(gc->CreateLinearGradientBrush(
            0, 0, size.GetWidth(), size.GetHeight(), startColor, endColor));
        gc->DrawRoundedRectangle(roundXOffset, roundYOffset, size.GetWidth(), size.GetHeight(), roundingRadius);
        delete gc;
    }

    if (HasFocus()) {
        wxPen dottedPen(*wxBLACK, 1, wxPENSTYLE_DOT);
        dc.SetPen(dottedPen);
        dc.SetBrush(*wxTRANSPARENT_BRUSH);
        dc.DrawRoundedRectangle(roundXOffset + 2, roundYOffset + 2,
           size.GetWidth() - 4, size.GetHeight() - 4,
           roundingRadius);
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

void GradientButton::SetNormalColor(wxColour start, wxColour end) {
   m_normalColorStart = start;
   m_normalColorEnd = end;
};

void GradientButton::SetPressedColor(wxColour start, wxColour end) {
   m_pressedColorStart = start;
   m_pressedColorEnd = end;
};

void GradientButton::SetNormalColor(wxColour color) {
   m_normalColorStart = color;
   m_normalColorEnd = color;
};

void GradientButton::SetPressedColor(wxColour color) {
   m_pressedColorStart = color;
   m_pressedColorEnd = color;
};

