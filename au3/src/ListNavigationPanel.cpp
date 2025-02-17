/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file ListNavigationPanel.cpp

   @author Vitaly Sverchinsky

**********************************************************************/

#include "ListNavigationPanel.h"
#include "AColor.h"
#include <wx/dcbuffer.h>

ListNavigationPanel::ListNavigationPanel(wxWindow* parent,
                                         wxWindowID id,
                                         const wxPoint& pos,
                                         const wxSize& size,
                                         const wxString& name)
{
    Create(parent, id, pos, size, name);
}

void ListNavigationPanel::Create(wxWindow* parent,
                                 wxWindowID id,
                                 const wxPoint& pos,
                                 const wxSize& size,
                                 const wxString& name)
{
    SetBackgroundStyle(wxBG_STYLE_PAINT);
    ListNavigationEnabled<wxWindow>::Create(parent, id, pos, size, wxNO_BORDER | wxWANTS_CHARS, name);
    Bind(wxEVT_PAINT, &ListNavigationPanel::OnPaint, this);
    Bind(wxEVT_SET_FOCUS, &ListNavigationPanel::OnChangeFocus, this);
    Bind(wxEVT_KILL_FOCUS, &ListNavigationPanel::OnChangeFocus, this);
}

void ListNavigationPanel::OnChangeFocus(wxFocusEvent& evt)
{
    Refresh(false);
}

void ListNavigationPanel::OnPaint(wxPaintEvent& evt)
{
    wxBufferedPaintDC dc(this);

    dc.SetPen(*wxTRANSPARENT_PEN);
    dc.SetBrush(GetBackgroundColour());
    dc.Clear();

    if (HasFocus()) {
        AColor::DrawFocus(dc, GetClientRect().Deflate(3, 3));
    }
}
