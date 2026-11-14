/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file ListNavigationEnabled.cpp

   @author Vitaly Sverchinsky

**********************************************************************/

#include "ListNavigationEnabled.h"

void ListNavigationEnabled_HandleCharHook(wxWindow* self, wxKeyEvent& evt)
{
    //We want to restore focus to list item once arrow navigation is used
    //on the child item, for this we need a char hook since key/navigation
    //events are sent directly to the focused item
    const auto keyCode = evt.GetKeyCode();
    if ((keyCode == WXK_DOWN || keyCode == WXK_UP)
        && !self->HasFocus()
        && self->IsDescendant(wxWindow::FindFocus())) {
        self->SetFocusFromKbd();
    } else {
        evt.Skip();
    }
}

void ListNavigationEnabled_HandleKeyDown(wxWindow* self, wxKeyEvent& evt)
{
    const auto keyCode = evt.GetKeyCode();
    if (keyCode == WXK_TAB) {
        self->NavigateIn(wxNavigationKeyEvent::FromTab
                         | (evt.ShiftDown() ? wxNavigationKeyEvent::IsBackward : wxNavigationKeyEvent::IsForward));
    } else if (keyCode == WXK_DOWN) {
        self->Navigate(wxNavigationKeyEvent::IsForward);
    } else if (keyCode == WXK_UP) {
        self->Navigate(wxNavigationKeyEvent::IsBackward);
    } else {
        evt.Skip();
    }
}

void ListNavigationEnabled_HandleNavigationKeyEvent(wxWindow* self,
                                                    wxNavigationKeyEvent& evt, bool inTabOrder)
{
    if (evt.GetEventObject() == self->GetParent() && (!evt.IsFromTab() || inTabOrder)) {
        self->SetFocusFromKbd();
    } else if (evt.GetEventObject() == self && evt.GetCurrentFocus() == self && evt.IsFromTab()) {
        //NavigateIn
        wxPropagationDisabler disableProp(evt);
        const auto isForward = evt.GetDirection();
        const auto& children = self->GetChildren();
        auto node = isForward ? children.GetFirst() : children.GetLast();
        while (node)
        {
            auto child = node->GetData();
            if (child->CanAcceptFocusFromKeyboard()) {
                if (!child->GetEventHandler()->ProcessEvent(evt)) {
                    child->SetFocusFromKbd();
                }
                evt.Skip(false);
                return;
            }
            node = isForward ? node->GetNext() : node->GetPrevious();
        }
    } else {
        evt.Skip();
    }
}

void ListNavigationEnabled_HandleDestroy(wxWindow* self)
{
    if (self->IsDescendant(wxWindow::FindFocus())) {
        auto next = self->GetNextSibling();
        if (next != nullptr && next->AcceptsFocus()) {
            next->SetFocus();
        } else {
            auto prev = self->GetPrevSibling();
            if (prev != nullptr && prev->AcceptsFocus()) {
                prev->SetFocus();
            }
        }
    }
}
