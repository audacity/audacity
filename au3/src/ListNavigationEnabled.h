/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file ListNavigationEnabled.h

   @author Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <wx/window.h>
#include <wx/event.h>
#include <wx/containr.h>

/**
 * \brief Changes default arrow navigation to behave more list- or table-like.
 * Instead of searching focusable items among children first, list navigation
 * searches for siblings when arrow key is pressed. Tab behaviour stays same.
 * Requires wxWANT_CHARS style flag to be set
 */
template<class WindowBase>
class ListNavigationEnabled : public wxNavigationEnabled<WindowBase>
{
    friend void ListNavigationEnabled_HandleCharHook(wxWindow* self, wxKeyEvent& evt);
    friend void ListNavigationEnabled_HandleKeyDown(wxWindow* self, wxKeyEvent& evt);
    friend void ListNavigationEnabled_HandleNavigationKeyEvent(wxWindow* self, wxNavigationKeyEvent& evt, bool inTabOrder);
    friend void ListNavigationEnabled_HandleDestroy(wxWindow* self);

public:
    ListNavigationEnabled()
    {
        WindowBase::Bind(wxEVT_NAVIGATION_KEY, &ListNavigationEnabled::OnNavigationKeyEvent, this);
        WindowBase::Bind(wxEVT_KEY_DOWN, &ListNavigationEnabled::OnKeyDown, this);
        WindowBase::Bind(wxEVT_CHAR_HOOK, &ListNavigationEnabled::OnCharHook, this);
    }

    void SetInTabOrder(bool inTabOrder) { mInTabOrder = inTabOrder; }

private:
    void SetFocus() override
    {
        //Prevent attempt to search for a focusable child
        WindowBase::SetFocus();
    }

    void OnCharHook(wxKeyEvent& evt)
    {
        ListNavigationEnabled_HandleCharHook(this, evt);
    }

    void OnKeyDown(wxKeyEvent& evt)
    {
        ListNavigationEnabled_HandleKeyDown(this, evt);
    }

    void OnNavigationKeyEvent(wxNavigationKeyEvent& evt)
    {
        ListNavigationEnabled_HandleNavigationKeyEvent(this, evt, mInTabOrder);
    }

    bool Destroy() override
    {
        ListNavigationEnabled_HandleDestroy(this);
        return wxNavigationEnabled<WindowBase>::Destroy();
    }

    bool mInTabOrder{ true };
};
