/**********************************************************************

  Audacity: A Digital Audio Editor

  CellularPanel.cpp

  Dominic Mazzoni
  and lots of other contributors

  Implements CellularPanel.

********************************************************************//*!

\class CellularPanel
\brief

  Formerly part of TrackPanel, this abstract base class has no special knowledge
  of Track objects and is intended for reuse with other windows.

  Manages a division of a panel's area into disjoint rectangles, each with
  an associated Cell object.  Details of that partition and association, and
  the choice of the cell with keyboard focus, are subclass responsibilities.

  Handling of keyboard events is delegated to the focused cell.  The cell under
  the mouse position is queried for hit-test candidate objects, which handle
  click-drag-release (and ESC key abort) sequences.

*//*****************************************************************/

#include "CellularPanel.h"

#include <wx/eventfilter.h>
#include <wx/setup.h> // for wxUSE_* macros
#include "KeyboardCapture.h"
#include "UIHandle.h"
#include "TrackPanelMouseEvent.h"
#include "HitTestResult.h"
#include "RefreshCode.h"
#include "TrackPanelCell.h"

// A singleton class that intercepts escape key presses when some cellular
// panel is dragging
struct CellularPanel::Filter : wxEventFilter
{
    Filter()
    {
        wxEvtHandler::AddFilter(this);
    }

    ~Filter()
    {
        wxEvtHandler::RemoveFilter(this);
    }

    static void Create()
    {
        static Filter instance;
    }

    int FilterEvent(wxEvent& event) override
    {
        const auto type = event.GetEventType();
        if (type == wxEVT_KEY_DOWN
            && static_cast< wxKeyEvent& >(event).GetKeyCode() == WXK_ESCAPE) {
            bool eatEvent = false;
            for (const auto& pPanel: { spClickedPanel, spEnteredPanel }) {
                if (pPanel) {
                    eatEvent = true;
                    // Handle escape either in the clicked panel to abort a drag, or
                    // to switch among hit test candidates before button down in the
                    // entered (but not yet clicked) panel
                    pPanel->HandleEscapeKey(true);
                }
            }
            if (eatEvent) {
                return Event_Processed;
            }
        } else if ((type == wxEVT_LEFT_DOWN
                    || type == wxEVT_RIGHT_DOWN
                    || type == wxEVT_MIDDLE_DOWN)) {
            if (spClickedPanel
                && spClickedPanel != event.GetEventObject()) {
                // Clicking away from the panel doesn't necessarily change wxWidgets
                // focus, so we use this global filter instead
                spClickedPanel->DoKillFocus();
                // Don't eat the event
            }
        }
        return Event_Skip;
    }

    static wxWeakRef< CellularPanel > spClickedPanel;
    static wxWeakRef< CellularPanel > spEnteredPanel;
};

wxWeakRef< CellularPanel > CellularPanel::Filter::spClickedPanel = nullptr;
wxWeakRef< CellularPanel > CellularPanel::Filter::spEnteredPanel = nullptr;

struct CellularPanel::State
{
    UIHandlePtr mUIHandle;

    std::weak_ptr<TrackPanelCell> mLastCell;
    std::vector<UIHandlePtr> mTargets;
    size_t mTarget {};
    unsigned mMouseOverUpdateFlags{};

    int mMouseMostRecentX;
    int mMouseMostRecentY;

    std::weak_ptr<TrackPanelCell> mpClickedCell;

    bool mEnableTab{};
};

BEGIN_EVENT_TABLE(CellularPanel, OverlayPanel)
EVT_MOUSE_EVENTS(CellularPanel::OnMouseEvent)
EVT_MOUSE_CAPTURE_LOST(CellularPanel::OnCaptureLost)
EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, CellularPanel::OnCaptureKey)
EVT_KEY_DOWN(CellularPanel::OnKeyDown)
EVT_KEY_UP(CellularPanel::OnKeyUp)
EVT_CHAR(CellularPanel::OnChar)
EVT_SET_FOCUS(CellularPanel::OnSetFocus)
EVT_KILL_FOCUS(CellularPanel::OnKillFocus)
EVT_CONTEXT_MENU(CellularPanel::OnContextMenu)
END_EVENT_TABLE()

CellularPanel::CellularPanel(
    wxWindow* parent, wxWindowID id,
    const wxPoint& pos, const wxSize& size,
    ViewInfo* viewInfo,
    long style)
    : OverlayPanel(parent, id, pos, size, style)
    , mViewInfo(viewInfo)
    , mState{std::make_unique<State>()}
{
    // Create the global event filter instance for CellularPanels only when the
    // first CellularPanel is created, not sooner, so that the filter will be
    // invoked before that for the application.
    Filter::Create();
}

CellularPanel::~CellularPanel() = default;

void CellularPanel::HandleInterruptedDrag()
{
    auto& state = *mState;
    if (state.mUIHandle
        && state.mUIHandle->StopsOnKeystroke()) {
        // The bogus id isn't used anywhere, but may help with debugging.
        // as this is sending a bogus mouse up.  The mouse button is still actually down
        // and may go up again.
        const int idBogusUp = 2;
        wxMouseEvent evt { wxEVT_LEFT_UP };
        evt.SetId(idBogusUp);
        evt.SetPosition(this->ScreenToClient(::wxGetMousePosition()));
        this->ProcessEvent(evt);
    }
}

void CellularPanel::Uncapture(bool escaping, wxMouseState* pState)
{
    auto state = ::wxGetMouseState();
    if (!pState) {
        // Remap the position
        state.SetPosition(this->ScreenToClient(state.GetPosition()));
        pState = &state;
    }

    if (HasCapture()) {
        ReleaseMouse();
    }
    HandleMotion(*pState);

    if (escaping || !AcceptsFocus()) {
        Filter::spClickedPanel = nullptr;
    }
}

bool CellularPanel::CancelDragging(bool escaping)
{
    auto& state = *mState;
    if (state.mUIHandle) {
        // copy shared_ptr for safety, as in HandleClick
        auto handle = state.mUIHandle;
        // UIHANDLE CANCEL
        UIHandle::Result refreshResult = handle->Cancel(GetProject());
        auto pClickedCell = state.mpClickedCell.lock();
        if (pClickedCell) {
            ProcessUIHandleResult(
                pClickedCell.get(), {},
                refreshResult | state.mMouseOverUpdateFlags);
        }
        state.mpClickedCell.reset();
        state.mUIHandle.reset(), handle.reset(), ClearTargets();
        Uncapture(escaping);
        return true;
    }
    return false;
}

bool CellularPanel::HandleEscapeKey(bool down)
{
    if (!down) {
        return false;
    }

    {
        auto target = Target();
        const auto pProject = GetProject();
        if (target && target->HasEscape(pProject) && target->Escape(pProject)) {
            HandleCursorForPresentMouseState(false);
            return true;
        }
    }

    auto& state = *mState;
    if (state.mUIHandle) {
        CancelDragging(true);
        return true;
    }

    if (ChangeTarget(true, false)) {
        HandleCursorForPresentMouseState(false);
        return true;
    }

    return false;
}

void CellularPanel::UpdateMouseState(const wxMouseState& state)
{
    mLastMouseState = state;

    // Simulate a down button if none, so hit test routines can anticipate
    // which button will be clicked
    if (!state.ButtonIsDown(wxMOUSE_BTN_ANY)) {
#ifdef __WXOSX__
        if (state.RawControlDown()) {
            // On Mac we can distinctly anticipate "right" click (as Control+click)
            mLastMouseState.SetRightDown(true),
            mLastMouseState.SetLeftDown(false);
        } else
#endif
        // Anticipate a left click by default
        mLastMouseState.SetRightDown(false),
        mLastMouseState.SetLeftDown(true);
    }
}

void CellularPanel::HandleModifierKey()
{
    HandleCursorForPresentMouseState();
}

void CellularPanel::HandleCursorForPresentMouseState(bool doHit)
{
    // Come here on modifier key or mouse button transitions,
    // or on starting or stopping of play or record,
    // or change of toolbar button,
    // and change the cursor appropriately.

    // Get the button and key states
    auto state = ::wxGetMouseState();
    // Remap the position
    state.SetPosition(this->ScreenToClient(state.GetPosition()));

    HandleMotion(state, doHit);
}

///  CellularPanel::HandleMotion( ) sets the cursor drawn at the mouse location,
///  and updates the status bar message.
///  We treat certain other changes of mouse button and key state as "motions"
///  too, and also starting and stopping of playback or recording, all of which
///  may cause the appropriate cursor and message to change.
///  As this procedure checks which region the mouse is over, it is
///  appropriate to establish the message in the status bar.
void CellularPanel::HandleMotion(wxMouseState& inState, bool doHit)
{
    UpdateMouseState(inState);

    const auto foundCell = FindCell(inState.m_x, inState.m_y);
    auto& rect = foundCell.rect;
    auto& pCell = foundCell.pCell;
    const TrackPanelMouseState tpmState{ mLastMouseState, rect, pCell };
    HandleMotion(tpmState, doHit);
}

void CellularPanel::HandleMotion
    (const TrackPanelMouseState& tpmState, bool doHit)
{
    auto& state = *mState;
    auto handle = state.mUIHandle;

    auto newCell = tpmState.pCell;
    auto oldCell = state.mLastCell.lock();
    auto oldHandle = Target();

    TranslatableString status, tooltip;
    wxCursor* pCursor{};
    unsigned refreshCode = 0;
    if (!doHit) {
        // Dragging or not
        handle = Target();

        // Assume cell does not change but target does
        refreshCode = state.mMouseOverUpdateFlags;
        state.mMouseOverUpdateFlags = 0;
    } else if (!state.mUIHandle) {
        // Not yet dragging.

        unsigned updateFlags = state.mMouseOverUpdateFlags;

        // First check whether crossing cell to cell
        if (newCell == oldCell) {
            oldCell.reset();
        } else {
            // Forget old targets
            ClearTargets();
            // Re-draw any highlighting
            if (oldCell) {
                ProcessUIHandleResult(
                    oldCell.get(), oldCell.get(), updateFlags);
            }
        }

        auto oldPosition = state.mTarget;

        // Now do the
        // UIHANDLE HIT TEST !
        state.mTargets.clear();
        if (newCell) {
            state.mTargets = newCell->HitTest(tpmState, GetProject());
        }
        state.mTarget = 0;

        // Find the old target's NEW place if we can
        if (oldHandle) {
            auto begin = state.mTargets.begin(), end = state.mTargets.end(),
                 iter = std::find(begin, end, oldHandle);
            if (iter != end) {
                size_t newPosition = iter - begin;
                if (newPosition <= oldPosition) {
                    state.mTarget = newPosition;
                }
                // else, some NEW hit at this position takes priority
            }
        }

        handle = Target();

        state.mLastCell = newCell;

        // These lines caused P2 Bug 2617, repeated refreshing using all CPU.
        // Disabling them might be causing something to not refresh,
        // but so far I have not found a downside to disabling them.  JKC

        // VS: https://github.com/audacity/audacity/issues/1363
        // Extensive refresh request fixed by using std::move on
        // new envelope handle instance
        if (!oldCell && oldHandle != handle) {
            // Did not move cell to cell, but did change the target
            refreshCode = updateFlags;
        }

        if (handle && handle != oldHandle) {
            handle->Enter(true, GetProject());
        }

        if (oldHandle == handle) {
            oldHandle.reset();
        }
    }

    // UIHANDLE PREVIEW
    // Update status message and cursor, whether dragging or not
    if (handle) {
        auto preview = handle->Preview(tpmState, GetProject());
        status = preview.message;
        tooltip = preview.tooltip;
        pCursor = preview.cursor;
        auto code = handle->GetChangeHighlight();
        handle->SetChangeHighlight(RefreshCode::RefreshNone);
        refreshCode |= code;
        state.mMouseOverUpdateFlags |= code;
    }
    if (newCell
        && (!pCursor || status.empty() || tooltip.empty())) {
        // Defaulting of cursor, tooltip, and status if there is no handle,
        // or if the handle does not specify them
        const auto preview = newCell->DefaultPreview(tpmState, GetProject());
        if (!pCursor) {
            pCursor = preview.cursor;
        }
        if (status.empty()) {
            status = preview.message;
        }
        if (tooltip.empty()) {
            tooltip = preview.tooltip;
        }
    }
    if (!pCursor) {
        // Ultimate default cursor
        static wxCursor defaultCursor{ wxCURSOR_DEFAULT };
        pCursor = &defaultCursor;
    }

    // Update status, tooltip, and cursor only if we're dragging, or the mouse
    // was in one of our cells and nobody else is dragging
    if (handle || (newCell && !wxWindow::GetCapture())) {
        UpdateStatusMessage(status);

#if wxUSE_TOOLTIPS
        if (tooltip.Translation() != GetToolTipText()) {
            // Unset first, by analogy with AButton
            UnsetToolTip();
            if (handle != oldHandle) {
                SetToolTip(tooltip);
            }
        }
#endif

        if (pCursor) {
            SetCursor(*pCursor);
        }
    } else if (oldCell || oldHandle) {
        // Leaving a cell or hit test target with no replacement
        UpdateStatusMessage({});
    }

    if (newCell) {
        ProcessUIHandleResult(newCell.get(), newCell.get(), refreshCode);
    }
}

void CellularPanel::Leave()
{
    // Make transition into an empty CellularPanel state
    auto state = ::wxGetMouseState();
    const wxRect rect;
    std::shared_ptr<TrackPanelCell> pCell;
    TrackPanelMouseState tpmState{ state, rect, pCell };
    HandleMotion(tpmState);
}

bool CellularPanel::HasRotation()
{
    auto& state = *mState;
    // Is there a nontrivial TAB key rotation?
    if (state.mTargets.size() > 1) {
        return true;
    }
    auto target = Target();
    return target && target->HasRotation();
}

bool CellularPanel::HasEscape()
{
    if (IsMouseCaptured()) {
        return true;
    }

    auto& state = *mState;
    if (state.mTarget + 1 == state.mTargets.size()
        && Target()
        && !Target()->HasEscape(GetProject())) {
        return false;
    }

    return state.mTargets.size() > 0;
}

bool CellularPanel::ChangeTarget(bool forward, bool cycle)
{
    auto& state = *mState;
    auto size = state.mTargets.size();

    auto target = Target();
    if (target && target->HasRotation()) {
        if (target->Rotate(forward)) {
            return true;
        } else if (cycle && (size == 1 || IsMouseCaptured())) {
            // Rotate through the states of this target only.
            target->Enter(forward, GetProject());
            return true;
        }
    }

    if (!cycle
        && ((forward && state.mTarget + 1 == size)
            || (!forward && state.mTarget == 0))) {
        return false;
    }

    if (size > 1) {
        if (forward) {
            ++state.mTarget;
        } else {
            state.mTarget += size - 1;
        }
        state.mTarget %= size;
        if (Target()) {
            Target()->Enter(forward, GetProject());
        }
        return true;
    }

    return false;
}

/// Determines if a modal tool is active
bool CellularPanel::IsMouseCaptured()
{
    auto& state = *mState;
    return state.mUIHandle != NULL;
}

void CellularPanel::OnContextMenu(wxContextMenuEvent& WXUNUSED(event))
{
    DoContextMenu({});
}

/// Handle mouse wheel rotation (for zoom in/out, vertical and horizontal scrolling)
void CellularPanel::HandleWheelRotation(TrackPanelMouseEvent& tpmEvent)
{
    auto pCell = tpmEvent.pCell;
    if (!pCell) {
        return;
    }

    auto& event = tpmEvent.event;
    double steps {};
#if defined(__WXMAC__) && defined(EVT_MAGNIFY)
    // PRL:
    // Pinch and spread implemented in wxWidgets 3.1.0, or cherry-picked from
    // the future in custom build of 3.0.2
    if (event.Magnify()) {
        event.SetControlDown(true);
        steps = 2 * event.GetMagnification();
    } else
#endif
    {
        steps = event.m_wheelRotation
                / (event.m_wheelDelta > 0 ? (double)event.m_wheelDelta : 120.0);
    }

    if (event.GetWheelAxis() == wxMOUSE_WHEEL_HORIZONTAL) {
        // Two-fingered horizontal swipe on mac is treated like shift-mousewheel
        event.SetShiftDown(true);
        // This makes the wave move in the same direction as the fingers, and the scrollbar
        // thumb moves oppositely
        steps *= -1;
    }

    tpmEvent.steps = steps;

    if (!event.HasAnyModifiers()) {
        // We will later un-skip if we do anything, but if we don't,
        // propagate the event up for the sake of the scrubber
        event.Skip();
        event.ResumePropagation(wxEVENT_PROPAGATE_MAX);
    }

    unsigned result
        =pCell->HandleWheelRotation(tpmEvent, GetProject());
    ProcessUIHandleResult(
        pCell.get(), pCell.get(), result);
}

void CellularPanel::OnCaptureKey(wxCommandEvent& event)
{
    auto& state = *mState;
    state.mEnableTab = false;
    wxKeyEvent* kevent = static_cast<wxKeyEvent*>(event.GetEventObject());
    const auto code = kevent->GetKeyCode();
    if (WXK_ESCAPE != code) {
        HandleInterruptedDrag();
    }

    // Give focused cell precedence
    const auto t = GetFocusedCell();
    if (t) {
        const unsigned refreshResult
            =t->CaptureKey(*kevent, *mViewInfo, this, GetProject());
        ProcessUIHandleResult(t.get(), t.get(), refreshResult);
        event.Skip(kevent->GetSkipped());
    }

#if 0
    // Special TAB key handling, but only if the cell didn't capture it
    if (!(t && !kevent->GetSkipped())
        && WXK_TAB == code && HasRotation()) {
        // Override TAB navigation in wxWidgets, by not skipping
        event.Skip(false);
        mEnableTab = true;
        return;
    } else
#endif
    if (!t) {
        event.Skip();
    }
}

void CellularPanel::OnKeyDown(wxKeyEvent& event)
{
    switch (event.GetKeyCode()) {
    case WXK_ESCAPE:
        // This switch case is now redundant with the global filter
        if (HandleEscapeKey(true)) {
            // Don't skip the event, eat it so that
            // AudacityApp does not also stop any playback.
            return;
        } else {
            break;
        }

    case WXK_ALT:
    case WXK_SHIFT:
    case WXK_CONTROL:
#ifdef __WXOSX__
    case WXK_RAW_CONTROL:
#endif
        HandleModifierKey();
        break;

#if 0
    case WXK_TAB:
        if (mEnableTab && HasRotation()) {
            ChangeTarget(!event.ShiftDown(), true);
            HandleCursorForPresentMouseState(false);
            return;
        } else {
            break;
        }
#endif
    }

    const auto t = GetFocusedCell();
    if (t) {
        const unsigned refreshResult
            =t->KeyDown(event, *mViewInfo, this, GetProject());
        ProcessUIHandleResult(t.get(), t.get(), refreshResult);
    } else {
        event.Skip();
    }
}

void CellularPanel::OnChar(wxKeyEvent& event)
{
    switch (event.GetKeyCode()) {
    case WXK_ESCAPE:
    case WXK_ALT:
    case WXK_SHIFT:
    case WXK_CONTROL:
    case WXK_PAGEUP:
    case WXK_PAGEDOWN:
        return;
    }

    const auto t = GetFocusedCell();
    if (t) {
        const unsigned refreshResult
            =t->Char(event, *mViewInfo, this, GetProject());
        ProcessUIHandleResult(t.get(), t.get(), refreshResult);
    } else {
        event.Skip();
    }
}

void CellularPanel::OnKeyUp(wxKeyEvent& event)
{
    bool didSomething = false;
    switch (event.GetKeyCode()) {
    case WXK_ESCAPE:
        didSomething = HandleEscapeKey(false);
        break;

    case WXK_ALT:
    case WXK_SHIFT:
    case WXK_CONTROL:
#ifdef __WXOSX__
    case WXK_RAW_CONTROL:
#endif
        HandleModifierKey();
        break;
    }

    if (didSomething) {
        return;
    }

    const auto t = GetFocusedCell();
    if (t) {
        const unsigned refreshResult
            =t->KeyUp(event, *mViewInfo, this, GetProject());
        ProcessUIHandleResult(t.get(), t.get(), refreshResult);
        return;
    }

    event.Skip();
}

/// Should handle the case when the mouse capture is lost. (MSW only)
void CellularPanel::OnCaptureLost(wxMouseCaptureLostEvent& WXUNUSED(event))
{
    auto& state = *mState;
    state.mUIHandle.reset();
    Leave();

    // This is bad.  We are lying abou the event by saying it is a mouse up.
    wxMouseEvent e(wxEVT_LEFT_UP);
    e.SetId(kCaptureLostEventId);

    e.m_x = state.mMouseMostRecentX;
    e.m_y = state.mMouseMostRecentY;

    OnMouseEvent(e);
}

/// This handles just generic mouse events.  Then, based
/// on our current state, we forward the mouse events to
/// various interested parties.
void CellularPanel::OnMouseEvent(wxMouseEvent& event)
try
{
    const auto foundCell = FindCell(event.m_x, event.m_y);
    auto& rect = foundCell.rect;
    auto& pCell = foundCell.pCell;

    const auto size = GetSize();
    TrackPanelMouseEvent tpmEvent{ event, rect, size, pCell };

#if defined(__WXMAC__) && defined(EVT_MAGNIFY)
    // PRL:
    // Pinch and spread implemented in wxWidgets 3.1.0, or cherry-picked from
    // the future in custom build of 3.0.2
    if (event.Magnify()) {
        HandleWheelRotation(tpmEvent);
    }
#endif

    // If a mouse event originates from a keyboard context menu event then
    // event.GetPosition() == wxDefaultPosition. wxContextMenu events are handled in
    // CellularPanel::OnContextMenu(), and therefore associated mouse events are ignored here.
    // Not ignoring them was causing bug 613: the mouse events were interpreted as clicking
    // outside the tracks.
    if (event.GetPosition() == wxDefaultPosition && (event.RightDown() || event.RightUp())) {
        event.Skip();
        return;
    }

    if (event.m_wheelRotation != 0) {
        HandleWheelRotation(tpmEvent);
    }

    if (event.LeftDown() || event.LeftIsDown() || event.Moving()) {
        // Skip, even if we do something, so that the left click or drag
        // may have an additional effect in the scrubber.
        event.Skip();
        event.ResumePropagation(wxEVENT_PROPAGATE_MAX);
    }

    auto& state = *mState;
    state.mMouseMostRecentX = event.m_x;
    state.mMouseMostRecentY = event.m_y;

    if (event.LeftDown()) {
        // The activate event is used to make the
        // parent window 'come alive' if it didn't have focus.
        wxActivateEvent e;
        GetParent()->GetEventHandler()->ProcessEvent(e);
    }

    if (event.Entering()) {
        Filter::spEnteredPanel = this;
    } else if (event.Leaving()) {
        if (Filter::spEnteredPanel == this) {
            Filter::spEnteredPanel = nullptr;
        }
        Leave();

        auto buttons
            =// Bug 1325: button state in Leaving events is unreliable on Mac.
             // Poll the global state instead.
             // event.ButtonIsDown(wxMOUSE_BTN_ANY);
              ::wxGetMouseState().ButtonIsDown(wxMOUSE_BTN_ANY);

        if (!buttons) {
            CancelDragging(false);

#if defined(__WXMAC__)

            // We must install the cursor ourselves since the window under
            // the mouse is no longer this one and wx2.8.12 makes that check.
            // Should re-evaluate with wx3.
            wxSTANDARD_CURSOR->MacInstall();
#endif
        }
    }

    if (state.mUIHandle) {
        auto pClickedCell = state.mpClickedCell.lock();
        if (event.Dragging()) {
            // UIHANDLE DRAG
            // copy shared_ptr for safety, as in HandleClick
            auto handle = state.mUIHandle;
            const UIHandle::Result refreshResult
                =handle->Drag(tpmEvent, GetProject());
            ProcessUIHandleResult
                (pClickedCell.get(), pCell.get(), refreshResult);
            state.mMouseOverUpdateFlags |= refreshResult;
            if (refreshResult & RefreshCode::Cancelled) {
                // Drag decided to abort itself
                state.mUIHandle.reset(), handle.reset(), ClearTargets();
                state.mpClickedCell.reset();
                Uncapture(false, &event);
            } else {
                UpdateMouseState(event);
                TrackPanelMouseState tpmState{ mLastMouseState, rect, pCell };
                HandleMotion(tpmState);
            }
        } else if (event.ButtonUp()) {
            // UIHANDLE RELEASE
            // copy shared_ptr for safety, as in HandleClick
            auto handle = state.mUIHandle;
            unsigned moreFlags = state.mMouseOverUpdateFlags;
            UIHandle::Result refreshResult
                =handle->Release(tpmEvent, GetProject(), this);
            ProcessUIHandleResult
                (pClickedCell.get(), pCell.get(),
                refreshResult | moreFlags);
            state.mUIHandle.reset(), handle.reset(), ClearTargets();
            state.mpClickedCell.reset();
            // will also Uncapture() below
        }
    } else if (event.GetEventType() == wxEVT_MOTION) {
        // Update status message and cursor, not during drag
        // consider it not a drag, even if button is down during motion, if
        // mUIHandle is null, as it becomes during interrupted drag
        // (e.g. by hitting space to play while dragging an envelope point)
        HandleMotion(event);
    } else if (event.ButtonDown() || event.ButtonDClick()) {
        HandleClick(tpmEvent);
    }

    if (event.ButtonDown() && IsMouseCaptured()) {
        if (!HasCapture()) {
            CaptureMouse();
        }
    }

    if (event.ButtonUp()) {
        Uncapture(false);
    }
}
catch (...)
{
    // Abort any dragging, as if by hitting Esc
    if (CancelDragging(true)) {
    } else {
        Uncapture(true);
        Refresh(false);
    }
    throw;
}

namespace {
class DefaultRightButtonHandler : public UIHandle
{
public:
    explicit DefaultRightButtonHandler(
        const std::shared_ptr<TrackPanelCell>& pCell)
        : mwCell{pCell}
    {}

    ~DefaultRightButtonHandler() override;

    std::shared_ptr<const Track> FindTrack() const override
    { return nullptr; }

    virtual Result Click
        (const TrackPanelMouseEvent& event, AudacityProject* pProject) override
    {
        return RefreshCode::RefreshNone;
    }

    virtual Result Drag
        (const TrackPanelMouseEvent& event, AudacityProject* pProject) override
    {
        return RefreshCode::RefreshNone;
    }

    virtual HitTestPreview Preview
        (const TrackPanelMouseState& state, AudacityProject* pProject) override
    {
        return {};
    }

    virtual Result Release
        (const TrackPanelMouseEvent& event, AudacityProject* pProject,
        wxWindow* pParent) override
    {
        if (auto pCell = mwCell.lock()) {
            auto point = event.event.GetPosition();
            return pCell->DoContextMenu(event.rect, pParent, &point, pProject);
        }
        return RefreshCode::RefreshNone;
    }

    virtual Result Cancel(AudacityProject* pProject) override
    {
        return RefreshCode::RefreshNone;
    }

private:
    std::weak_ptr<TrackPanelCell> mwCell;
};

DefaultRightButtonHandler::~DefaultRightButtonHandler()
{
}
}

void CellularPanel::HandleClick(const TrackPanelMouseEvent& tpmEvent)
{
    auto pCell = tpmEvent.pCell;
    // Do hit test once more, in case the button really pressed was not the
    // one "anticipated."
    {
        TrackPanelMouseState tpmState{
            tpmEvent.event,
            tpmEvent.rect,
            tpmEvent.pCell
        };
        HandleMotion(tpmState);
    }

    auto& state = *mState;
    state.mUIHandle = Target();
    if (tpmEvent.event.RightDown()
        && !(state.mUIHandle && state.mUIHandle->HandlesRightClick())) {
        if (auto pCell = state.mLastCell.lock()) {
            state.mUIHandle = std::make_shared<DefaultRightButtonHandler>(pCell);
        }
    }

    if (state.mUIHandle) {
        // UIHANDLE CLICK
        // Make another shared pointer to the handle, in case recursive
        // event dispatching otherwise tries to delete the handle.
        auto handle = state.mUIHandle;
        UIHandle::Result refreshResult
            =handle->Click(tpmEvent, GetProject());
        if (refreshResult & RefreshCode::Cancelled) {
            state.mUIHandle.reset(), handle.reset(), ClearTargets();
        } else {
            Filter::spClickedPanel = this;

#if wxUSE_TOOLTIPS
            // Remove any outstanding tooltip
            UnsetToolTip();
#endif

            if (!HasFocus() && AcceptsFocus()) {
                SetFocusIgnoringChildren();
            }

            state.mpClickedCell = pCell;

            // Perhaps the clicked handle wants to update cursor and state message
            // after a click.
            TrackPanelMouseState tpmState{
                tpmEvent.event,
                tpmEvent.rect,
                tpmEvent.pCell
            };
            HandleMotion(tpmState);
        }
        ProcessUIHandleResult(
            pCell.get(), pCell.get(), refreshResult);
        state.mMouseOverUpdateFlags |= refreshResult;
    }
}

void CellularPanel::DoContextMenu(std::shared_ptr<TrackPanelCell> pCell)
{
    if (!pCell) {
        pCell = GetFocusedCell();
        if (!pCell) {
            return;
        }
    }

    std::weak_ptr<TrackPanelCell> wCell = pCell;

    const auto delegate = pCell->ContextMenuDelegate();
    if (!delegate) {
        return;
    }

    auto rect = FindRect(*delegate);
    const UIHandle::Result refreshResult
        =delegate->DoContextMenu(rect, this, nullptr, GetProject());
    pCell.reset();

    ProcessUIHandleResult(wCell.lock().get(), wCell.lock().get(), refreshResult);
}

void CellularPanel::OnSetFocus(wxFocusEvent& event)
{
    SetFocusedCell();
    Refresh(false);
}

void CellularPanel::DoKillFocus()
{
    if (auto pCell = GetFocusedCell()) {
        auto refreshResult = pCell->LoseFocus(GetProject());
        auto& state = *mState;
        auto pClickedCell = state.mpClickedCell.lock();
        if (pClickedCell) {
            ProcessUIHandleResult(pClickedCell.get(), {}, refreshResult);
        }
    }
    Refresh(false);
}

void CellularPanel::OnKillFocus(wxFocusEvent& WXUNUSED(event))
{
    DoKillFocus();
    if (KeyboardCapture::IsHandler(this)) {
        KeyboardCapture::Release(this);
    }
}

// Empty out-of-line default functions to fill Visitor's vtable
CellularPanel::Visitor::~Visitor() {}
void CellularPanel::Visitor::VisitCell(
    const wxRect& rect, TrackPanelCell& cell) {}
void CellularPanel::Visitor::BeginGroup(
    const wxRect& rect, TrackPanelGroup& group) {}
void CellularPanel::Visitor::EndGroup(
    const wxRect& rect, TrackPanelGroup& group) {}

// Public, top-level entry for generalized Visit
void CellularPanel::Visit(Visitor& visitor)
{
    Visit(GetClientRect(), Root(), visitor);
}

// Common utility class for the functions that follow
namespace {
struct Adaptor : CellularPanel::Visitor {
    using SimpleCellVisitor = CellularPanel::SimpleCellVisitor;
    using SimpleNodeVisitor = CellularPanel::SimpleNodeVisitor;

    // Visit cells only
    Adaptor(const SimpleCellVisitor& function_)
        : function{[&](const wxRect& rect, TrackPanelNode& cell) {
            return function_(rect, static_cast<TrackPanelCell&>(cell));
        }}
    {}

    // Visit cells and groups, each once only, choosing pre- or post- ordering
    // for the groups
    Adaptor(const SimpleNodeVisitor& function_, bool pre_)
        : function{function_}, pre{pre_}, post{!pre_} {}

    void VisitCell(const wxRect& rect, TrackPanelCell& cell) override
    { return function(rect, cell); }
    void BeginGroup(const wxRect& rect, TrackPanelGroup& group) override
    {
        if (pre) {
            return function(rect, group);
        }
    }

    void EndGroup(const wxRect& rect, TrackPanelGroup& group) override
    {
        if (post) {
            return function(rect, group);
        }
    }

    SimpleNodeVisitor function;
    const bool pre{ false }, post{ false };
};
}

// Simplified entry points for visits that don't need all the generality of
// CellularPanel::Visitor
void CellularPanel::VisitCells(const SimpleCellVisitor& visitor)
{
    Adaptor adaptor{ visitor };
    Visit(adaptor);
}

void CellularPanel::VisitPreorder(const SimpleNodeVisitor& visitor)
{
    Adaptor adaptor{ visitor, true };
    Visit(adaptor);
}

void CellularPanel::VisitPostorder(const SimpleNodeVisitor& visitor)
{
    Adaptor adaptor{ visitor, false };
    Visit(adaptor);
}

namespace {
wxRect Subdivide(
    const wxRect& rect, bool divideX,
    const TrackPanelGroup::Refinement& children,
    const TrackPanelGroup::Refinement::const_iterator iter)
{
    const auto next = iter + 1;
    const auto end = children.end();
    wxCoord nextCoord;
    if (next == end) {
        nextCoord = std::max(iter->first,
                             divideX ? rect.GetRight() : rect.GetBottom());
    } else {
        nextCoord = next->first - 1;
    }

    auto lesser = iter->first;
    auto greater = nextCoord;

    auto result = rect;
    if (divideX) {
        result.SetLeft(lesser), result.SetRight(greater);
    } else {
        result.SetTop(lesser), result.SetBottom(greater);
    }

    return result;
}
}

// Private, recursive implementation function of Visit
void CellularPanel::Visit(
    const wxRect& rect, const std::shared_ptr<TrackPanelNode>& node,
    Visitor& visitor)
{
    if (auto pCell = dynamic_cast<TrackPanelCell*>(node.get())) {
        visitor.VisitCell(rect, *pCell);
    } else if (auto pGroup = dynamic_cast<TrackPanelGroup*>(node.get())) {
        visitor.BeginGroup(rect, *pGroup);

        // Recur on children
        const auto results = pGroup->Children(rect);
        const bool divideX = results.first == TrackPanelGroup::Axis::X;
        const auto& children = results.second;
        const auto begin = children.begin(), end = children.end();
        for (auto iter = begin; iter != end; ++iter) {
            Visit(
                Subdivide(rect, divideX, children, iter), iter->second, visitor);
        }

        visitor.EndGroup(rect, *pGroup);
    } else {
        return;
    }
}

auto CellularPanel::FindCell(int mouseX, int mouseY) -> FoundCell
{
    auto rect = this->GetClientRect();
    auto node = Root();
    while (node) {
        if (auto pCell = std::dynamic_pointer_cast< TrackPanelCell >(node)) {
            // Found the bottom of the hierarchy
            return { pCell, rect }
        } else if (auto pGroup = dynamic_cast< TrackPanelGroup* >(node.get())) {
            // Ask node for its subdivision
            const auto results = pGroup->Children(rect);
            const bool divideX = results.first == TrackPanelGroup::Axis::X;
            const auto& children = results.second;

            // Find the correct child
            const auto begin = children.begin(), end = children.end();
            auto iter = std::upper_bound(begin, end,
                                         (divideX ? mouseX : mouseY),
                                         [&]( wxCoord coord, const TrackPanelGroup::Child& child ) {
                return coord < child.first;
            }
                                         );
            if (iter == begin) {
                break;
            }
            --iter;

            // Descend the hierarchy of nodes
            rect = Subdivide(rect, divideX, children, iter);
            node = iter->second;
        } else {
            // Nulls in the array of children are allowed, to define a void with
            // no cell
            break;
        }
    }

    return { {}, {} };
}

wxRect CellularPanel::FindRect(const TrackPanelCell& cell)
{
    wxRect result;

    struct Stop {};
    try {
        VisitCells([&]( const wxRect& rect, TrackPanelCell& visited ) {
            if (&visited == &cell) {
                result = rect, throw Stop {}
            }
        });
    }
    catch (const Stop&) {}

    return result;
}

wxRect CellularPanel::FindRect(
    const std::function< bool(TrackPanelNode&) >& pred)
{
    wxRect result;

    struct Stop {};
    try {
        VisitPreorder([&]( const wxRect& rect, TrackPanelNode& visited ) {
            if (pred(visited)) {
                result = rect, throw Stop {}
            }
        });
    }
    catch (const Stop&) {}

    return result;
}

UIHandlePtr CellularPanel::Target()
{
    auto& state = *mState;
    if (state.mTargets.size()) {
        return state.mTargets[state.mTarget];
    } else {
        return {}
    }
}

wxCoord CellularPanel::MostRecentXCoord() const
{
    auto& state = *mState;
    return state.mMouseMostRecentX;
}

void CellularPanel::ClearTargets()
{
    auto& state = *mState;
    // Forget the rotation of hit test candidates when the mouse moves from
    // cell to cell or outside of the panel entirely.
    state.mLastCell.reset();
    state.mTargets.clear();
    state.mTarget = 0;
    state.mMouseOverUpdateFlags = 0;
}

std::shared_ptr<TrackPanelCell> CellularPanel::LastCell() const
{
    auto& state = *mState;
    return state.mLastCell.lock();
}

void CellularPanel::Draw(TrackPanelDrawingContext& context, unsigned nPasses)
{
    const auto panelRect = GetClientRect();
    auto lastCell = LastCell();
    for ( unsigned iPass = 0; iPass < nPasses; ++iPass ) {
        VisitPostorder([&]( const wxRect& rect, TrackPanelNode& node ) {
            // Draw the node
            const auto newRect = node.DrawingArea(
                context, rect, panelRect, iPass);
            if (newRect.Intersects(panelRect)) {
                node.Draw(context, newRect, iPass);
            }

            // Draw the current handle if it is associated with the node
            if (&node == lastCell.get()) {
                auto target = Target();
                if (target) {
                    const auto targetRect
                        =target->DrawingArea(context, rect, panelRect, iPass);
                    if (targetRect.Intersects(panelRect)) {
                        target->Draw(context, targetRect, iPass);
                    }
                }
            }
        }); // nodes
    } // passes
}
