/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyboardCapture.h

  Paul Licameli split this from Project.h

**********************************************************************/

#ifndef __AUDACITY_KEYBOARD_CAPTURE__
#define __AUDACITY_KEYBOARD_CAPTURE__

#include <functional>
#include <wx/event.h>

#include "GlobalVariable.h"

////////////////////////////////////////////////////////////
/// Custom events
////////////////////////////////////////////////////////////

/// \brief Type of event that may be sent to a window while it is
/// installed as the handler with KeyboardCapture::Capture; if it does not skip
/// the event, it will receive further events of type wxEVT_KEY_DOWN,
/// and then wxEVT_CHAR (if key-down was not skipped; those further events
/// don't actually come from wxWidgets, but are simulated by Audacity, as
/// translations of the EVT_CHAR_HOOK event); or, wxEVT_KEY_UP (really from
/// wxWidgets).
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_CAPTURE_KEY, -1);

namespace KeyboardCapture {
AUDACITY_DLL_API bool IsHandler(const wxWindow* handler);
AUDACITY_DLL_API wxWindow* GetHandler();
AUDACITY_DLL_API void Capture(wxWindow* handler);
AUDACITY_DLL_API void Release(wxWindow* handler);

//! Pre-filter is called before passing the event to the captured window
/*! If it returns false, then skip the event entirely */
struct AUDACITY_DLL_API PreFilter : GlobalHook<PreFilter,
                                               bool(wxKeyEvent&)
                                               > {};

//! Post-filter is conditionally called after passing the event to the window
/*!
       Post-filter is called if the captured window skips either the
       EVT_CAPTURE_KEY or the following wxKEY_DOWN event (but not if
       it skips only the wxEVT_CHAR or wxEVT_KEY_UP event); it is passed a
       wxKEY_DOWN or a wxKEY_UP event; if it returns false, then the event is
       skipped
    */
struct AUDACITY_DLL_API PostFilter : GlobalHook<PostFilter,
                                                bool(wxKeyEvent&)
                                                > {};

/// \brief a function useful to implement a focus event handler
/// The window releases the keyboard if the event is for killing focus,
/// otherwise the window captures the keyboard; then refresh the window
/// and skip the event
AUDACITY_DLL_API
void OnFocus(wxWindow& window, wxFocusEvent& event);
}

#endif
