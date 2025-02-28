/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file ListNavigationPanel.h

   @author Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <wx/window.h>
#include <wx/containr.h>

#if !defined(__FreeBSD__) && !defined(__OpenBSD__)
extern template class WXDLLIMPEXP_CORE wxNavigationEnabled<wxWindow>;
#endif

#include "ListNavigationEnabled.h"

//!Focusable widget container.
//!Has custom background and focus outline painting.
class ListNavigationPanel : public ListNavigationEnabled<wxWindow>
{
public:
    ListNavigationPanel() = default;

    ListNavigationPanel(wxWindow* parent, wxWindowID id, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
                        const wxString& name = wxPanelNameStr);

    void Create(wxWindow* parent, wxWindowID id, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
                const wxString& name = wxPanelNameStr);
private:

    void OnChangeFocus(wxFocusEvent& evt);

    void OnPaint(wxPaintEvent& evt);
};
