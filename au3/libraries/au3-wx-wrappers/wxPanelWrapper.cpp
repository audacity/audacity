//
//  wxPanelWrapper.cpp
//  Audacity
//
//  Created by Paul Licameli on 6/25/16.
//
//

#include "wxPanelWrapper.h"

#include <wx/grid.h>

const TranslatableString wxDirDialogWrapper::DefaultDialogPrompt = XO("Select a directory");

void wxTabTraversalWrapperCharHook(wxKeyEvent& event)
{
//#ifdef __WXMAC__
#if defined(__WXMAC__) || defined(__WXGTK__)
    // Compensate for the regressions in TAB key navigation
    // due to the switch to wxWidgets 3.0.2
    if (event.GetKeyCode() == WXK_TAB) {
        auto focus = wxWindow::FindFocus();
        if (dynamic_cast<wxGrid*>(focus)
            || (focus
                && focus->GetParent()
                && dynamic_cast<wxGrid*>(focus->GetParent()->GetParent()))) {
            // Let wxGrid do its own TAB key handling
            event.Skip();
            return;
        }
        // Apparently, on wxGTK, FindFocus can return NULL
        if (focus) {
            focus->Navigate(
                event.ShiftDown()
                ? wxNavigationKeyEvent::IsBackward
                : wxNavigationKeyEvent::IsForward
                );
            return;
        }
    }
#endif

    event.Skip();
}

void wxPanelWrapper::SetLabel(const TranslatableString& label)
{
    wxPanel::SetLabel(label.Translation());
}

void wxPanelWrapper::SetName(const TranslatableString& name)
{
    wxPanel::SetName(name.Translation());
}

void wxPanelWrapper::SetToolTip(const TranslatableString& toolTip)
{
    wxPanel::SetToolTip(toolTip.Stripped().Translation());
}

void wxPanelWrapper::SetName()
{
    wxPanel::SetName(GetLabel());
}

void wxDialogWrapper::SetTitle(const TranslatableString& title)
{
    wxDialog::SetTitle(title.Translation());
}

void wxDialogWrapper::SetLabel(const TranslatableString& label)
{
    wxDialog::SetLabel(label.Translation());
}

void wxDialogWrapper::SetName(const TranslatableString& name)
{
    wxDialog::SetName(name.Translation());
}

void wxDialogWrapper::SetName()
{
    wxDialog::SetName(wxDialog::GetTitle());
}

AudacityMessageDialog::~AudacityMessageDialog() = default;
