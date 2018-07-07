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

#include "Audacity.h"
#include "CellularPanel.h"
#include "Project.h"
#include "UIHandle.h"
#include "TrackPanelMouseEvent.h"
#include "HitTestResult.h"
#include "RefreshCode.h"

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
   wxWindow * parent, wxWindowID id,
   const wxPoint & pos, const wxSize & size,
   ViewInfo *viewInfo,
   long style)
: OverlayPanel(parent, id, pos, size, style)
, mViewInfo( viewInfo )
, mState{ std::make_unique<State>() }
{
}

CellularPanel::~CellularPanel() = default;

void CellularPanel::HandleInterruptedDrag()
{
   auto &state = *mState;
   if (state.mUIHandle &&
       state.mUIHandle->StopsOnKeystroke() ) {
      // The bogus id isn't used anywhere, but may help with debugging.
      // as this is sending a bogus mouse up.  The mouse button is still actually down
      // and may go up again.
      const int idBogusUp = 2;
      wxMouseEvent evt { wxEVT_LEFT_UP };
      evt.SetId( idBogusUp );
      evt.SetPosition(this->ScreenToClient(::wxGetMousePosition()));
      this->ProcessEvent(evt);
   }
}

void CellularPanel::Uncapture(wxMouseState *pState)
{
   auto state = ::wxGetMouseState();
   if (!pState) {
      // Remap the position
      state.SetPosition(this->ScreenToClient(state.GetPosition()));
      pState = &state;
   }

   if (HasCapture())
      ReleaseMouse();
   HandleMotion( *pState );
}

bool CellularPanel::CancelDragging()
{
   auto &state = *mState;
   if (state.mUIHandle) {
      // copy shared_ptr for safety, as in HandleClick
      auto handle = state.mUIHandle;
      // UIHANDLE CANCEL
      UIHandle::Result refreshResult = handle->Cancel(GetProject());
      auto pClickedCell = state.mpClickedCell.lock();
      if (pClickedCell)
         ProcessUIHandleResult(
            pClickedCell.get(), {},
            refreshResult | state.mMouseOverUpdateFlags );
      state.mpClickedCell.reset();
      state.mUIHandle.reset(), handle.reset(), ClearTargets();
      Uncapture();
      return true;
   }
   return false;
}

bool CellularPanel::HandleEscapeKey(bool down)
{
   if (!down)
      return false;

   {
      auto target = Target();
      if (target && target->HasEscape() && target->Escape()) {
         HandleCursorForPresentMouseState(false);
         return true;
      }
   }

   auto &state = *mState;
   if (state.mUIHandle) {
      CancelDragging();
      return true;
   }

   if (ChangeTarget(true, false)) {
      HandleCursorForPresentMouseState(false);
      return true;
   }

   return false;
}

void CellularPanel::UpdateMouseState(const wxMouseState &state)
{
   mLastMouseState = state;

   // Simulate a down button if none, so hit test routines can anticipate
   // which button will be clicked
   if (!state.ButtonIsDown(wxMOUSE_BTN_ANY)) {
#ifdef __WXOSX__
      if (state.RawControlDown())
         // On Mac we can distinctly anticipate "right" click (as Control+click)
         mLastMouseState.SetRightDown( true ),
         mLastMouseState.SetLeftDown( false );
      else
#endif
         // Anticipate a left click by default
         mLastMouseState.SetRightDown( false ),
         mLastMouseState.SetLeftDown( true );
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

   HandleMotion( state, doHit );
}

///  CellularPanel::HandleMotion( ) sets the cursor drawn at the mouse location,
///  and updates the status bar message.
///  We treat certain other changes of mouse button and key state as "motions"
///  too, and also starting and stopping of playback or recording, all of which
///  may cause the appropriate cursor and message to change.
///  As this procedure checks which region the mouse is over, it is
///  appropriate to establish the message in the status bar.
void CellularPanel::HandleMotion( wxMouseState &inState, bool doHit )
{
   UpdateMouseState( inState );

   const auto foundCell = FindCell( inState.m_x, inState.m_y );
   auto &rect = foundCell.rect;
   auto &pCell = foundCell.pCell;
   const TrackPanelMouseState tpmState{ mLastMouseState, rect, pCell };
   HandleMotion( tpmState, doHit );
}

void CellularPanel::HandleMotion
( const TrackPanelMouseState &tpmState, bool doHit )
{
   auto &state = *mState;
   auto handle = state.mUIHandle;

   auto newCell = tpmState.pCell;

   wxString status{}, tooltip{};
   wxCursor *pCursor{};
   unsigned refreshCode = 0;

   if ( ! doHit ) {
      // Dragging or not
      handle = Target();

      // Assume cell does not change but target does
      refreshCode = state.mMouseOverUpdateFlags;
      state.mMouseOverUpdateFlags = 0;
   }
   else if ( !state.mUIHandle ) {
      // Not yet dragging.

      auto oldCell = state.mLastCell.lock();

      unsigned updateFlags = state.mMouseOverUpdateFlags;

      // First check whether crossing cell to cell
      if ( newCell == oldCell )
         oldCell.reset();
      else {
         // Forget old targets
         ClearTargets();
         // Re-draw any highlighting
         if (oldCell) {
            ProcessUIHandleResult(
               oldCell.get(), oldCell.get(), updateFlags);
         }
      }

      auto oldHandle = Target();
      auto oldPosition = state.mTarget;

      // Now do the
      // UIHANDLE HIT TEST !
      state.mTargets = newCell->HitTest(tpmState, GetProject());

      state.mTarget = 0;

      // Find the old target's NEW place if we can
      if (oldHandle) {
         auto begin = state.mTargets.begin(), end = state.mTargets.end(),
            iter = std::find(begin, end, oldHandle);
         if (iter != end) {
            size_t newPosition = iter - begin;
            if (newPosition <= oldPosition)
               state.mTarget = newPosition;
            // else, some NEW hit and this position takes priority
         }
      }

      handle = Target();

      state.mLastCell = newCell;

      if (!oldCell && oldHandle != handle)
         // Did not move cell to cell, but did change the target
         refreshCode = updateFlags;

      if (handle && handle != oldHandle)
         handle->Enter(true);
   }

   // UIHANDLE PREVIEW
   // Update status message and cursor, whether dragging or not
   if (handle) {
      auto preview = handle->Preview( tpmState, GetProject() );
      status = preview.message;
      tooltip = preview.tooltip;
      pCursor = preview.cursor;
      auto code = handle->GetChangeHighlight();
      handle->SetChangeHighlight(RefreshCode::RefreshNone);
      refreshCode |= code;
      state.mMouseOverUpdateFlags |= code;
   }
   if (newCell &&
       (!pCursor || status.empty() || tooltip.empty())) {
      // Defaulting of cursor, tooltip, and status if there is no handle,
      // or if the handle does not specify them
      const auto preview = newCell->DefaultPreview( tpmState, GetProject() );
      if (!pCursor)
         pCursor = preview.cursor;
      if (status.empty())
         status = preview.message;
      if (tooltip.empty())
         tooltip = preview.tooltip;
   }
   if (!pCursor) {
      // Ultimate default cursor
      static wxCursor defaultCursor{ wxCURSOR_DEFAULT };
      pCursor = &defaultCursor;
   }

   UpdateStatusMessage(status);

#if wxUSE_TOOLTIPS
   if (tooltip != GetToolTipText()) {
      // Unset first, by analogy with AButton
      UnsetToolTip();
      SetToolTip(tooltip);
   }
#endif

   if (pCursor)
      SetCursor( *pCursor );

   ProcessUIHandleResult(
      newCell.get(), newCell.get(), refreshCode);
}

bool CellularPanel::HasRotation()
{
   auto &state = *mState;
   // Is there a nontrivial TAB key rotation?
   if ( state.mTargets.size() > 1 )
      return true;
   auto target = Target();
   return target && target->HasRotation();
}

bool CellularPanel::HasEscape()
{
   if (IsMouseCaptured())
      return true;

   auto &state = *mState;
  if (state.mTarget + 1 == state.mTargets.size() &&
       Target() &&
       !Target()->HasEscape())
       return false;

   return state.mTargets.size() > 0;
}

bool CellularPanel::ChangeTarget(bool forward, bool cycle)
{
   auto &state = *mState;
   auto size = state.mTargets.size();

   auto target = Target();
   if (target && target->HasRotation()) {
      if(target->Rotate(forward))
         return true;
      else if (cycle && (size == 1 || IsMouseCaptured())) {
         // Rotate through the states of this target only.
         target->Enter(forward);
         return true;
      }
   }

   if (!cycle &&
       ((forward && state.mTarget + 1 == size) ||
        (!forward && state.mTarget == 0)))
      return false;

   if (size > 1) {
      if (forward)
         ++state.mTarget;
      else
         state.mTarget += size - 1;
      state.mTarget %= size;
      if (Target())
         Target()->Enter(forward);
      return true;
   }

   return false;
}

/// Determines if a modal tool is active
bool CellularPanel::IsMouseCaptured()
{
   auto &state = *mState;
   return state.mUIHandle != NULL;
}

void CellularPanel::OnContextMenu(wxContextMenuEvent & WXUNUSED(event))
{
   DoContextMenu();
}

/// Handle mouse wheel rotation (for zoom in/out, vertical and horizontal scrolling)
void CellularPanel::HandleWheelRotation( TrackPanelMouseEvent &tpmEvent )
{
   auto pCell = tpmEvent.pCell;
   if (!pCell)
      return;

   auto &event = tpmEvent.event;
   double steps {};
#if defined(__WXMAC__) && defined(EVT_MAGNIFY)
   // PRL:
   // Pinch and spread implemented in wxWidgets 3.1.0, or cherry-picked from
   // the future in custom build of 3.0.2
   if (event.Magnify()) {
      event.SetControlDown(true);
      steps = 2 * event.GetMagnification();
   }
   else
#endif
   {
      steps = event.m_wheelRotation /
         (event.m_wheelDelta > 0 ? (double)event.m_wheelDelta : 120.0);
   }

   if(event.GetWheelAxis() == wxMOUSE_WHEEL_HORIZONTAL) {
      // Two-fingered horizontal swipe on mac is treated like shift-mousewheel
      event.SetShiftDown(true);
      // This makes the wave move in the same direction as the fingers, and the scrollbar
      // thumb moves oppositely
      steps *= -1;
   }

   tpmEvent.steps = steps;

   if(!event.HasAnyModifiers()) {
      // We will later un-skip if we do anything, but if we don't,
      // propagate the event up for the sake of the scrubber
      event.Skip();
      event.ResumePropagation(wxEVENT_PROPAGATE_MAX);
   }

   unsigned result =
      pCell->HandleWheelRotation( tpmEvent, GetProject() );
   ProcessUIHandleResult(
      pCell.get(), pCell.get(), result);
}

void CellularPanel::OnCaptureKey(wxCommandEvent & event)
{
   auto &state = *mState;
   state.mEnableTab = false;
   wxKeyEvent *kevent = static_cast<wxKeyEvent *>(event.GetEventObject());
   const auto code = kevent->GetKeyCode();
   if ( WXK_ESCAPE != code )
      HandleInterruptedDrag();

   // Give focused cell precedence
   const auto t = GetFocusedCell();
   if (t) {
      const unsigned refreshResult =
         t->CaptureKey(*kevent, *mViewInfo, this);
      ProcessUIHandleResult(t, t, refreshResult);
      event.Skip(kevent->GetSkipped());
   }

#if 0
   // Special TAB key handling, but only if the cell didn't capture it
   if ( !(t && !kevent->GetSkipped()) &&
        WXK_TAB == code && HasRotation() ) {
      // Override TAB navigation in wxWidgets, by not skipping
      event.Skip(false);
      mEnableTab = true;
      return;
   }
   else
#endif
   if (!t)
      event.Skip();
}

void CellularPanel::OnKeyDown(wxKeyEvent & event)
{
   switch (event.GetKeyCode())
   {
   case WXK_ESCAPE:
      if(HandleEscapeKey(true))
         // Don't skip the event, eat it so that
         // AudacityApp does not also stop any playback.
         return;
      else
         break;

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
      if ( mEnableTab && HasRotation() ) {
         ChangeTarget( !event.ShiftDown(), true );
         HandleCursorForPresentMouseState(false);
         return;
      }
      else
         break;
#endif
   }

   const auto t = GetFocusedCell();

   if (t) {
      const unsigned refreshResult =
         t->KeyDown(event, *mViewInfo, this);
      ProcessUIHandleResult(t, t, refreshResult);
   }
   else
      event.Skip();
}

void CellularPanel::OnChar(wxKeyEvent & event)
{
   switch (event.GetKeyCode())
   {
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
      const unsigned refreshResult =
         t->Char(event, *mViewInfo, this);
      ProcessUIHandleResult(t, t, refreshResult);
   }
   else
      event.Skip();
}

void CellularPanel::OnKeyUp(wxKeyEvent & event)
{
   bool didSomething = false;
   switch (event.GetKeyCode())
   {
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

   if (didSomething)
      return;

   const auto t = GetFocusedCell();
   if (t) {
      const unsigned refreshResult =
         t->KeyUp(event, *mViewInfo, this);
      ProcessUIHandleResult(t, t, refreshResult);
      return;
   }

   event.Skip();
}

/// Should handle the case when the mouse capture is lost.
void CellularPanel::OnCaptureLost(wxMouseCaptureLostEvent & WXUNUSED(event))
{
   ClearTargets();

   // This is bad.  We are lying abou the event by saying it is a mouse up.
   wxMouseEvent e(wxEVT_LEFT_UP);
   e.SetId( kCaptureLostEventId );

   auto &state = *mState;
   e.m_x = state.mMouseMostRecentX;
   e.m_y = state.mMouseMostRecentY;

   OnMouseEvent(e);
}

/// This handles just generic mouse events.  Then, based
/// on our current state, we forward the mouse events to
/// various interested parties.
void CellularPanel::OnMouseEvent(wxMouseEvent & event)
try
{
   const auto foundCell = FindCell( event.m_x, event.m_y );
   auto &rect = foundCell.rect;
   auto &pCell = foundCell.pCell;

   const auto size = GetSize();
   TrackPanelMouseEvent tpmEvent{ event, rect, size, pCell };

#if defined(__WXMAC__) && defined(EVT_MAGNIFY)
   // PRL:
   // Pinch and spread implemented in wxWidgets 3.1.0, or cherry-picked from
   // the future in custom build of 3.0.2
   if (event.Magnify()) {
      HandleWheelRotation( tpmEvent );
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

   if (event.m_wheelRotation != 0)
      HandleWheelRotation( tpmEvent );

   if (event.LeftDown() || event.LeftIsDown() || event.Moving()) {
      // Skip, even if we do something, so that the left click or drag
      // may have an additional effect in the scrubber.
      event.Skip();
      event.ResumePropagation(wxEVENT_PROPAGATE_MAX);
   }

   auto &state = *mState;
   state.mMouseMostRecentX = event.m_x;
   state.mMouseMostRecentY = event.m_y;

   if (event.LeftDown()) {
      // The activate event is used to make the
      // parent window 'come alive' if it didn't have focus.
      wxActivateEvent e;
      GetParent()->GetEventHandler()->ProcessEvent(e);
   }

   if (event.ButtonDown()) {
      SetFocus();
   }

   if (event.Leaving())
   {
      if ( !state.mUIHandle )
         ClearTargets();

      auto buttons =
         // Bug 1325: button state in Leaving events is unreliable on Mac.
         // Poll the global state instead.
         // event.ButtonIsDown(wxMOUSE_BTN_ANY);
         ::wxGetMouseState().ButtonIsDown(wxMOUSE_BTN_ANY);

      if(!buttons) {
         CancelDragging();

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
         const UIHandle::Result refreshResult =
            handle->Drag( tpmEvent, GetProject() );
         ProcessUIHandleResult
            (pClickedCell.get(), pCell.get(), refreshResult);
         state.mMouseOverUpdateFlags |= refreshResult;
         if (refreshResult & RefreshCode::Cancelled) {
            // Drag decided to abort itself
            state.mUIHandle.reset(), handle.reset(), ClearTargets();
            state.mpClickedCell.reset();
            Uncapture( &event );
         }
         else {
            UpdateMouseState(event);
            TrackPanelMouseState tpmState{ mLastMouseState, rect, pCell };
            HandleMotion( tpmState );
         }
      }
      else if (event.ButtonUp()) {
         // UIHANDLE RELEASE
         unsigned moreFlags = state.mMouseOverUpdateFlags;
         UIHandle::Result refreshResult =
            state.mUIHandle->Release( tpmEvent, GetProject(), this );
         ProcessUIHandleResult
            (pClickedCell.get(), pCell.get(),
             refreshResult | moreFlags);
         state.mUIHandle.reset(), ClearTargets();
         state.mpClickedCell.reset();
         // will also Uncapture() below
      }
   }
   else if ( event.GetEventType() == wxEVT_MOTION )
      // Update status message and cursor, not during drag
      // consider it not a drag, even if button is down during motion, if
      // mUIHandle is null, as it becomes during interrupted drag
      // (e.g. by hitting space to play while dragging an envelope point)
      HandleMotion( event );
   else if ( event.ButtonDown() || event.ButtonDClick() )
      HandleClick( tpmEvent );

   if (event.ButtonDown() && IsMouseCaptured()) {
      if (!HasCapture())
         CaptureMouse();
   }

   if (event.ButtonUp())
      Uncapture();
}
catch( ... )
{
   // Abort any dragging, as if by hitting Esc
   if ( CancelDragging() )
      ;
   else {
      Uncapture();
      Refresh(false);
   }
   throw;
}

void CellularPanel::HandleClick( const TrackPanelMouseEvent &tpmEvent )
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
      HandleMotion( tpmState );
   }

   auto &state = *mState;
   state.mUIHandle = Target();

   if (state.mUIHandle) {
      // UIHANDLE CLICK
      // Make another shared pointer to the handle, in case recursive
      // event dispatching otherwise tries to delete the handle.
      auto handle = state.mUIHandle;
      UIHandle::Result refreshResult =
         handle->Click( tpmEvent, GetProject() );
      if (refreshResult & RefreshCode::Cancelled)
         state.mUIHandle.reset(), handle.reset(), ClearTargets();
      else {
         state.mpClickedCell = pCell;

         // Perhaps the clicked handle wants to update cursor and state message
         // after a click.
         TrackPanelMouseState tpmState{
            tpmEvent.event,
            tpmEvent.rect,
            tpmEvent.pCell
         };
         HandleMotion( tpmState );
      }
      ProcessUIHandleResult(
         pCell.get(), pCell.get(), refreshResult);
      state.mMouseOverUpdateFlags |= refreshResult;
   }
}

void CellularPanel::DoContextMenu( TrackPanelCell *pCell )
{
   if( !pCell ) {
      pCell = GetFocusedCell();
      if( !pCell )
         return;
   }

   const auto delegate = pCell->ContextMenuDelegate();
   if (!delegate)
      return;

   auto rect = FindRect( *delegate );
   const UIHandle::Result refreshResult =
      delegate->DoContextMenu(rect, this, NULL);

   // To do: use safer shared_ptr to pCell
   ProcessUIHandleResult(pCell, pCell, refreshResult);
}

void CellularPanel::OnSetFocus(wxFocusEvent & WXUNUSED(event))
{
   SetFocusedCell();
}

void CellularPanel::OnKillFocus(wxFocusEvent & WXUNUSED(event))
{
   if (AudacityProject::HasKeyboardCapture(this))
   {
      AudacityProject::ReleaseKeyboard(this);
   }
   Refresh( false);
}

UIHandlePtr CellularPanel::Target()
{
   auto &state = *mState;
   if (state.mTargets.size())
      return state.mTargets[state.mTarget];
   else
      return {};
}

wxCoord CellularPanel::MostRecentXCoord() const
{
   auto &state = *mState;
   return state.mMouseMostRecentX;
}

void CellularPanel::ClearTargets()
{
   auto &state = *mState;
   // Forget the rotation of hit test candidates when the mouse moves from
   // cell to cell or outside of the panel entirely.
   state.mLastCell.reset();
   state.mTargets.clear();
   state.mTarget = 0;
   state.mMouseOverUpdateFlags = 0;
}

std::shared_ptr<TrackPanelCell> CellularPanel::LastCell() const
{
   auto &state = *mState;
   return state.mLastCell.lock();
}
