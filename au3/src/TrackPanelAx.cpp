/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanelAx.cpp

  Leland Lucius
  and lots of other contributors

******************************************************************//*!

\class TrackPanelAx
\brief Helper to TrackPanel to give accessibility.

*//*******************************************************************/
#include "TrackPanelAx.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/wx.h>
#endif

#include "SyncLock.h"
#include "LabelTrack.h"
#include "NoteTrack.h"
#include "TimeTrack.h"
#include "Viewport.h"

TrackPanelAx::Adapter::~Adapter() = default;

void TrackPanelAx::Adapter::MessageForScreenReader(
    const TranslatableString& message)
{
    if (mwAx) {
        mwAx->MessageForScreenReader(message);
    }
}

void TrackPanelAx::Adapter::BeginChangeFocus()
{
    if (mwAx) {
        mwAx->BeginChangeFocus();
    }
}

void TrackPanelAx::Adapter::EndChangeFocus(
    const std::shared_ptr<Track>& track)
{
    if (mwAx) {
        mwAx->EndChangeFocus(track);
    }
}

void TrackPanelAx::Adapter::UpdateAccessibility()
{
    if (mwAx) {
        mwAx->UpdateAccessibility();
    }
}

TrackPanelAx::TrackPanelAx(std::weak_ptr<Viewport> wViewport,
                           std::weak_ptr<TrackFocus> wFocus, RectangleFinder finder)  :
#if wxUSE_ACCESSIBILITY
    // window pointer must be set after construction
    WindowAccessible(nullptr),
#endif
    mwViewport{move(wViewport)},
    mwFocus{move(wFocus)}
    , mFinder{move(finder)}
{
}

TrackPanelAx::~TrackPanelAx()
{
}

void TrackPanelAx::BeginChangeFocus()
{
    mTrackName = true;
#if wxUSE_ACCESSIBILITY
    auto pFocus = mwFocus.lock();
    if (!pFocus) {
        return;
    }

    auto focusedTrack = pFocus->PeekFocus();
    if (focusedTrack && !focusedTrack->GetSelected()) {
        NotifyEvent(wxACC_EVENT_OBJECT_SELECTIONREMOVE,
                    GetWindow(),
                    wxOBJID_CLIENT,
                    pFocus->TrackNum(focusedTrack));
    }
#endif
}

void TrackPanelAx::EndChangeFocus(const std::shared_ptr<Track>& track)
{
#if wxUSE_ACCESSIBILITY
    auto pFocus = mwFocus.lock();
    if (!pFocus) {
        return;
    }

    if (track) {
        if (GetWindow() == wxWindow::FindFocus()) {
            NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
                        GetWindow(),
                        wxOBJID_CLIENT,
                        pFocus->NumFocusedTrack());
        }

        if (track->GetSelected()) {
            NotifyEvent(wxACC_EVENT_OBJECT_SELECTION,
                        GetWindow(),
                        wxOBJID_CLIENT,
                        pFocus->NumFocusedTrack());
        }
    } else {
        NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
                    GetWindow(),
                    wxOBJID_CLIENT,
                    wxACC_SELF);
    }

#endif
}

void TrackPanelAx::UpdateAccessibility()
{
    Updated();
}

void TrackPanelAx::Updated()
{
#if wxUSE_ACCESSIBILITY
    auto pFocus = mwFocus.lock();
    if (!pFocus) {
        return;
    }

    auto t = pFocus->GetFocus();
    mTrackName = true;

    // The object_focus event is only needed by Window-Eyes
    // and can be removed when we cease to support this screen reader.
    NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
                GetWindow(),
                wxOBJID_CLIENT,
                pFocus->TrackNum(t));

    NotifyEvent(wxACC_EVENT_OBJECT_NAMECHANGE,
                GetWindow(),
                wxOBJID_CLIENT,
                pFocus->TrackNum(t));
#endif
}

void TrackPanelAx::MessageForScreenReader(const TranslatableString& message)
{
#if wxUSE_ACCESSIBILITY
    auto pFocus = mwFocus.lock();
    if (!pFocus) {
        return;
    }

    if (GetWindow() == wxWindow::FindFocus()) {
        auto t = pFocus->GetFocus();
        int childId = t ? pFocus->TrackNum(t) : 0;

        mMessage = message.Translation();

        // append \a alternatively, so that the string is never the same as the previous string.
        // This ensures that screen readers read it.
        if (mMessageCount % 2 == 0) {
            mMessage.Append('\a');
        }
        mMessageCount++;

        mTrackName = false;
        NotifyEvent(wxACC_EVENT_OBJECT_NAMECHANGE,
                    GetWindow(),
                    wxOBJID_CLIENT,
                    childId);
    }

#endif
}

#if wxUSE_ACCESSIBILITY

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus TrackPanelAx::GetChild(int childId, wxAccessible** child)
{
    if (childId == wxACC_SELF) {
        *child = this;
    } else {
        *child = NULL;
    }

    return wxACC_OK;
}

// Gets the number of children.
wxAccStatus TrackPanelAx::GetChildCount(int* childCount)
{
    auto pFocus = mwFocus.lock();
    if (!pFocus) {
        return wxACC_FAIL;
    }

    *childCount = as_const(*pFocus).GetTracks().Any().size();
    return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for a child).
// Return wxACC_OK even if there is no action. actionName is the action, or the empty
// string if there is no action.
// The retrieved string describes the action that is performed on an object,
// not what the object does as a result. For example, a toolbar button that prints
// a document has a default action of "Press" rather than "Prints the current document."
wxAccStatus TrackPanelAx::GetDefaultAction(int WXUNUSED(childId), wxString* actionName)
{
    actionName->clear();

    return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus TrackPanelAx::GetDescription(int WXUNUSED(childId), wxString* description)
{
    description->clear();

    return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus TrackPanelAx::GetHelpText(int WXUNUSED(childId), wxString* helpText)
{
    helpText->clear();

    return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus TrackPanelAx::GetKeyboardShortcut(int WXUNUSED(childId), wxString* shortcut)
{
    shortcut->clear();

    return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus TrackPanelAx::GetLocation(wxRect& rect, int elementId)
{
    auto pFocus = mwFocus.lock();
    if (!pFocus) {
        return wxACC_FAIL;
    }

    wxRect client;

    if (elementId == wxACC_SELF) {
        rect = GetWindow()->GetScreenRect();
    } else {
        auto t = pFocus->FindTrack(elementId);

        if (t == NULL) {
            return wxACC_FAIL;
        }

        rect = mFinder ? mFinder(*t) : wxRect{};
        // Inflate the screen reader's rectangle so it overpaints Audacity's own
        // yellow focus rectangle.
#ifdef __WXMAC__
        const int dx = 2;
#else
        const int dx = 1;
#endif
        rect.Inflate(dx, dx);
        rect.SetPosition(GetWindow()->ClientToScreen(rect.GetPosition()));
    }

    return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus TrackPanelAx::GetName(int childId, wxString* name)
{
#if defined(__WXMSW__) || defined(__WXMAC__)
    if (mTrackName) {
        if (childId == wxACC_SELF) {
            *name = _("TrackView");
        } else {
            auto pFocus = mwFocus.lock();
            if (!pFocus) {
                return wxACC_FAIL;
            }

            auto t = pFocus->FindTrack(childId);

            if (!t) {
                return wxACC_FAIL;
            }

            name->Printf("%d %s", pFocus->TrackNum(t), t->GetName());

            if (dynamic_cast<LabelTrack*>(t.get())) {
                const auto trackNameLower = t->GetName().Lower();
                //Prior to version 3.2 "Label Track" was the default
                //name for label tracks, don't append type part to the
                //text to avoid verbosity.
                if (trackNameLower.Find(wxString(_("Label Track")).Lower()) == wxNOT_FOUND
                    && trackNameLower.Find(LabelTrack::GetDefaultName().Lower()) == wxNOT_FOUND) {
                    /* i18n-hint: This is for screen reader software and indicates that
                       this is a Label track.*/
                    name->Append(wxT(" ") + wxString(_("Label Track")));
                }
            } else if (dynamic_cast<TimeTrack*>(t.get())) {
                if (t->GetName().Lower().Find(TimeTrack::GetDefaultName().Lower()) == wxNOT_FOUND) {
                    /* i18n-hint: This is for screen reader software and indicates that
                       this is a Time track.*/
                    name->Append(wxT(" ") + wxString(_("Time Track")));
                }
            }
#ifdef USE_MIDI
            else if (dynamic_cast<NoteTrack*>(t.get())) {
                /* i18n-hint: This is for screen reader software and indicates that
                   this is a Note track.*/
                name->Append(wxT(" ") + wxString(_("Note Track")));
            }
#endif

            // LLL: Remove these during "refactor"
            auto pt = dynamic_cast<PlayableTrack*>(t.get());
            if (pt && pt->GetMute()) {
                // The following comment also applies to the solo, selected,
                // and synclockselected states.
                // Many of translations of the strings with a leading space omitted
                // the leading space. Therefore a space has been added using wxT(" ").
                // Because screen readers won't be affected by multiple spaces, the
                // leading spaces have not been removed, so that no NEW translations are needed.
                /* i18n-hint: This is for screen reader software and indicates that
                   this track is muted. (The mute button is on.)*/
                name->Append(wxT(" ") + wxString(_(" Muted")));
            }

            if (pt && pt->GetSolo()) {
                /* i18n-hint: This is for screen reader software and indicates that
                   this track is soloed. (The Solo button is on.)*/
                name->Append(wxT(" ") + wxString(_(" Soloed")));
            }
            if (t->GetSelected()) {
                /* i18n-hint: This is for screen reader software and indicates that
                   this track is selected.*/
                name->Append(wxT(" ") + wxString(_(" Selected")));
            }
            if (SyncLock::IsSyncLockSelected(*t)) {
                /* i18n-hint: This is for screen reader software and indicates that
                   this track is shown with a sync-locked icon.*/
                // The absence of a dash between Sync and Locked is deliberate -
                // if present, Jaws reads it as "dash".
                name->Append(wxT(" ") + wxString(_(" Sync Locked")));
            }
        }
    } else {
        *name = mMessage;
    }

    return wxACC_OK;
#endif

#if defined(__WXMAC__)
    return wxACC_NOT_IMPLEMENTED;
#endif
}

// Returns a role constant.
wxAccStatus TrackPanelAx::GetRole(int childId, wxAccRole* role)
{
#if defined(__WXMSW__)
    if (mTrackName) {
        if (childId == wxACC_SELF) {
            *role = wxROLE_SYSTEM_TABLE;
        } else {
            *role = wxROLE_SYSTEM_ROW;
        }
    } else {
        *role = wxROLE_NONE;
    }
#endif

#if defined(__WXMAC__)
    if (childId == wxACC_SELF) {
        *role = wxROLE_SYSTEM_PANE;
    } else {
        *role = wxROLE_SYSTEM_STATICTEXT;
    }
#endif

    return wxACC_OK;
}

// Gets a variant representing the selected children
// of this object.
// Acceptable values:
// - a null variant (IsNull() returns TRUE)
// - a list variant (GetType() == wxT("list"))
// - an integer representing the selected child element,
//   or 0 if this object is selected (GetType() == wxT("long"))
// - a "void*" pointer to a wxAccessible child object
wxAccStatus TrackPanelAx::GetSelections(wxVariant* WXUNUSED(selections))
{
    return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus TrackPanelAx::GetState(int childId, long* state)
{
#if defined(__WXMSW__)
    auto pFocus = mwFocus.lock();
    if (!pFocus) {
        return wxACC_FAIL;
    }

    if (childId > 0) {
        auto t = pFocus->FindTrack(childId);

        *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE;
        if (t) {
            if (t == pFocus->PeekFocus() && GetWindow() == wxWindow::FindFocus()) {
                *state |= wxACC_STATE_SYSTEM_FOCUSED;
            }

            if (t->GetSelected() && mTrackName) {
                *state |= wxACC_STATE_SYSTEM_SELECTED;
            }
        }
    } else { // childId == wxACC_SELF
        // let wxWidgets use a standard accessible object for the state
        return wxACC_NOT_IMPLEMENTED;
    }
#endif

#if defined(__WXMAC__)
    auto pFocus = mwFocus.lock();
    if (!pFocus) {
        return wxACC_FAIL;
    }

    *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE;

    if (childId > 0) {
        auto t = pFocus->FindTrack(childId);

        if (t) {
            if (t == pFocus->PeekFocus()) {
                *state |= wxACC_STATE_SYSTEM_FOCUSED;
            }

            if (t->GetSelected()) {
                *state |= wxACC_STATE_SYSTEM_SELECTED;
            }
        }
    }
#endif

    return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
#if defined(__WXMAC__)
wxAccStatus TrackPanelAx::GetValue(int childId, wxString* strValue)
#else
wxAccStatus TrackPanelAx::GetValue(int WXUNUSED(childId), wxString* WXUNUSED(strValue))
#endif
{
#if defined(__WXMSW__)
    return wxACC_NOT_IMPLEMENTED;
#endif

#if defined(__WXMAC__)
    if (childId == wxACC_SELF) {
        *strValue = _("TrackView");
    } else {
        auto pFocus = mwFocus.lock();
        if (!pFocus) {
            return wxACC_FAIL;
        }

        auto t = pFocus->FindTrack(childId);

        if (t == NULL) {
            return wxACC_FAIL;
        } else {
            /* i18n-hint: The %d is replaced by the number of the track.*/
            strValue->Printf(_("Track %d"), pFocus->TrackNum(t));
            strValue->Append(" " + t->GetName());

            // LLL: Remove these during "refactor"
            auto pt = dynamic_cast<PlayableTrack*>(t.get());
            if (pt && pt->GetMute()) {
                strValue->Append(_(" Mute On"));
            }

            if (pt && pt->GetSolo()) {
                strValue->Append(_(" Solo On"));
            }
            if (t->GetSelected()) {
                strValue->Append(_(" Select On"));
            }
        }
    }
    return wxACC_OK;
#endif
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus TrackPanelAx::GetFocus(int* childId, wxAccessible** child)
{
    auto pFocus = mwFocus.lock();
    if (!pFocus) {
        return wxACC_FAIL;
    }

#if defined(__WXMSW__)

    if (GetWindow() == wxWindow::FindFocus()) {
        auto focusedTrack = pFocus->PeekFocus();
        if (focusedTrack) {
            *childId = pFocus->TrackNum(focusedTrack);
        } else {
            *child = this;
        }
    }

    return wxACC_OK;
#endif

#if defined(__WXMAC__)
    if (GetWindow() == wxWindow::FindFocus()) {
        auto focusedTrack = pFocus->PeekFocus();
        if (focusedTrack) {
            *childId = pFocus->TrackNum(focusedTrack);
        } else {
            *childId = wxACC_SELF;
        }

        return wxACC_OK;
    }

    return wxACC_NOT_IMPLEMENTED;
#endif
}

// Navigates from fromId to toId/toObject
wxAccStatus TrackPanelAx::Navigate(wxNavDir navDir, int fromId, int* toId, wxAccessible** toObject)
{
    int childCount;
    GetChildCount(&childCount);

    if (fromId > childCount) {
        return wxACC_FAIL;
    }

    switch (navDir) {
    case wxNAVDIR_FIRSTCHILD:
        if (fromId == wxACC_SELF && childCount > 0) {
            *toId = 1;
        } else {
            return wxACC_FALSE;
        }
        break;

    case wxNAVDIR_LASTCHILD:
        if (fromId == wxACC_SELF && childCount > 0) {
            *toId = childCount;
        } else {
            return wxACC_FALSE;
        }
        break;

    case wxNAVDIR_NEXT:
    case wxNAVDIR_DOWN:
        if (fromId != wxACC_SELF) {
            *toId = fromId + 1;
            if (*toId > childCount) {
                return wxACC_FALSE;
            }
        } else {
            return wxACC_NOT_IMPLEMENTED;
        }
        break;

    case wxNAVDIR_PREVIOUS:
    case wxNAVDIR_UP:
        if (fromId != wxACC_SELF) {
            *toId = fromId - 1;
            if (*toId < 1) {
                return wxACC_FALSE;
            }
        } else {
            return wxACC_NOT_IMPLEMENTED;
        }
        break;

    case wxNAVDIR_LEFT:
    case wxNAVDIR_RIGHT:
        if (fromId != wxACC_SELF) {
            return wxACC_FALSE;
        } else {
            return wxACC_NOT_IMPLEMENTED;
        }
        break;
    }

    *toObject = nullptr;
    return wxACC_OK;
}

// Modify focus or selection
wxAccStatus TrackPanelAx::Select(int childId, wxAccSelectionFlags selectFlags)
{
    auto pFocus = mwFocus.lock();
    if (!pFocus) {
        return wxACC_FAIL;
    }

    // Only support change of focus
    if (selectFlags != wxACC_SEL_TAKEFOCUS) {
        return wxACC_NOT_IMPLEMENTED;
    }

    if (childId != wxACC_SELF) {
        int childCount;
        GetChildCount(&childCount);
        if (childId > childCount) {
            return wxACC_FAIL;
        }

        Track* t = pFocus->FindTrack(childId).get();
        if (t) {
            pFocus->SetFocus(t->SharedPointer());
            auto pViewport = mwViewport.lock();
            if (pViewport) {
                pViewport->ShowTrack(*t);
            }
        }
    } else {
        return wxACC_NOT_IMPLEMENTED;
    }

    return wxACC_OK;
}

#endif // wxUSE_ACCESSIBILITY
