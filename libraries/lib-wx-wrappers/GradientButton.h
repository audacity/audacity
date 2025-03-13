/**********************************************************************

  Audacity: A Digital Audio Editor

  GradientButton.h

**********************************************************************/
#pragma once

#include <wx/wx.h>
#include <wx/graphics.h>
#include <wx/dcbuffer.h>

class WX_WRAPPERS_API GradientButton : public wxButton {
public:
    GradientButton(wxWindow* parent, wxWindowID id, const wxString& label,
                   const wxPoint& pos = wxDefaultPosition,
                   const wxSize& size = wxDefaultSize);

    void SetNormalColor(wxColor start, wxColor end);
    void SetPressedColor(wxColor start, wxColor end);

    void SetNormalColor(wxColor color);
    void SetPressedColor(wxColor color);

private:
    void OnPaint(wxPaintEvent& event);
    void OnMouseDown(wxMouseEvent& event);
    void OnMouseUp(wxMouseEvent& event);

    bool m_isPressed = false;
    wxColor m_normalColorStart;
    wxColor m_normalColorEnd;
    wxColor m_pressedColorStart;
    wxColor m_pressedColorEnd;
};
