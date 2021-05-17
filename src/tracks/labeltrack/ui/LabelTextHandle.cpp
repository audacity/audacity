/**********************************************************************

Audacity: A Digital Audio Editor

LabelTextHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "LabelTextHandle.h"

#include "LabelTrackView.h"

#include "../../../HitTestResult.h"
#include "../../../LabelTrack.h"
#include "../../../ProjectAudioIO.h"
#include "../../../ProjectHistory.h"
#include "../../../RefreshCode.h"
#include "../../../SelectionState.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../ViewInfo.h"
#include "../../../../images/Cursors.h"

#include <wx/clipbrd.h>

LabelTextHandle::LabelTextHandle
( const std::shared_ptr<LabelTrack> &pLT, int labelNum )
   : mpLT{ pLT }
   , mLabelNum{ labelNum }
{
}

void LabelTextHandle::Enter(bool, AudacityProject *)
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

HitTestPreview LabelTextHandle::HitPreview()
{
   static auto ibeamCursor =
      ::MakeCursor(wxCURSOR_IBEAM, IBeamCursorXpm, 17, 16);
   return {
      XO("Click to edit label text"),
      ibeamCursor.get()
   };
}

UIHandlePtr LabelTextHandle::HitTest
(std::weak_ptr<LabelTextHandle> &holder,
 const wxMouseState &state, const std::shared_ptr<LabelTrack> &pLT)
{
   // If Control is down, let the select handle be hit instead
   int labelNum;
   if (!state.ControlDown() &&
       (labelNum =
          LabelTrackView::OverATextBox(*pLT, state.m_x, state.m_y) ) >= 0) {
      auto result = std::make_shared<LabelTextHandle>( pLT, labelNum );
      result = AssignUIHandlePtr(holder, result);
      return result;
   }

   return {};
}

LabelTextHandle::~LabelTextHandle()
{
}

void LabelTextHandle::HandleTextClick(AudacityProject &
#if defined(__WXGTK__) && (HAVE_GTK)
                                                       project
#endif
                                                              ,
   const wxMouseEvent & evt,
   const wxRect & r, const ZoomInfo &zoomInfo,
   NotifyingSelectedRegion &newSel)
{
   auto pTrack = mpLT.lock();
   if (!pTrack)
      return;

   auto &view = LabelTrackView::Get( *pTrack );
   static_cast<void>(r);//compiler food.
   static_cast<void>(zoomInfo);//compiler food.
   if (evt.ButtonDown())
   {
      const auto selIndex = LabelTrackView::OverATextBox( *pTrack, evt.m_x, evt.m_y );
      view.SetSelectedIndex( selIndex );
      if ( selIndex != -1 ) {
         const auto &mLabels = pTrack->GetLabels();
         const auto &labelStruct = mLabels[ selIndex ];
         newSel = labelStruct.selectedRegion;

         if (evt.LeftDown()) {
            // Find the NEW drag end
            auto position = view.FindCursorPosition( evt.m_x );

            // Anchor shift-drag at the farther end of the previous highlight
            // that is farther from the click, on Mac, for consistency with
            // its text editors, but on the others, re-use the previous
            // anchor.
            auto initial = view.GetInitialCursorPosition();
            if (evt.ShiftDown()) {
#ifdef __WXMAC__
               // Set the drag anchor at the end of the previous selection
               // that is farther from the NEW drag end
               const auto current = view.GetCurrentCursorPosition();
               if ( abs( position - current ) > abs( position - initial ) )
                  initial = current;
#else
               // initial position remains as before
#endif
            }
            else
               initial = position;

            view.SetTextHighlight( initial, position );
            mRightDragging = false;
         }
         else
            // Actually this might be right or middle down
            mRightDragging = true;

         // Middle click on GTK: paste from primary selection
#if defined(__WXGTK__) && (HAVE_GTK)
         if (evt.MiddleDown()) {
            // Check for a click outside of the selected label's text box; in this
            // case PasteSelectedText() will start a NEW label at the click
            // location
            if (!LabelTrackView::OverTextBox(&labelStruct, evt.m_x, evt.m_y))
               view.SetSelectedIndex( -1 );
            double t = zoomInfo.PositionToTime(evt.m_x, r.x);
            newSel = SelectedRegion(t, t);
         }
#endif
      }
#if defined(__WXGTK__) && (HAVE_GTK)
      if (evt.MiddleDown()) {
         // Paste text, making a NEW label if none is selected.
         wxTheClipboard->UsePrimarySelection(true);
         view.PasteSelectedText(project, newSel.t0(), newSel.t1());
         wxTheClipboard->UsePrimarySelection(false);
      }
#endif
   }
}

UIHandle::Result LabelTextHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto pLT = mpLT.lock();
   if (!pLT)
      return RefreshCode::Cancelled;

   auto result = LabelDefaultClickHandle::Click( evt, pProject );

   auto &selectionState = SelectionState::Get( *pProject );
   auto &tracks = TrackList::Get( *pProject );
   mChanger =
      std::make_shared< SelectionStateChanger >( selectionState, tracks );

   const wxMouseEvent &event = evt.event;
   auto &viewInfo = ViewInfo::Get( *pProject );

   mSelectedRegion = viewInfo.selectedRegion;
   HandleTextClick( *pProject,
      event, evt.rect, viewInfo, viewInfo.selectedRegion );

   {
      // IF the user clicked a label, THEN select all other tracks by Label

      //do nothing if at least one other track is selected
      bool done = tracks.Selected().any_of(
         [&](const Track *pTrack){ return pTrack != pLT.get(); }
      );

      if (!done) {
         //otherwise, select all tracks
         for (auto t : tracks.Any())
            selectionState.SelectTrack( *t, true, true );
      }

      // Do this after, for its effect on TrackPanel's memory of last selected
      // track (which affects shift-click actions)
      selectionState.SelectTrack( *pLT, true, true );
   }

   // PRL: bug1659 -- make selection change undo correctly
   const bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
   if (!unsafe)
      ProjectHistory::Get( *pProject ).ModifyState(false);

   return result | RefreshCode::RefreshCell;
}

void LabelTextHandle::HandleTextDragRelease(
   AudacityProject &project, const wxMouseEvent & evt)
{
   auto pTrack = mpLT.lock();
   if (!pTrack)
      return;
   auto &view = LabelTrackView::Get( *pTrack );

   if(evt.LeftUp())
   {
#if 0
      // AWD: Due to wxWidgets bug #7491 (fix not ported to 2.8 branch) we
      // should never write the primary selection. We can enable this block
      // when we move to the 3.0 branch (or if a fixed 2.8 version is released
      // and we can do a runtime version check)
#if defined (__WXGTK__) && defined (HAVE_GTK)
      // On GTK, if we just dragged out a text selection, set the primary
      // selection
      if (mInitialCursorPos != mCurrentCursorPos) {
         wxTheClipboard->UsePrimarySelection(true);
         CopySelectedText();
         wxTheClipboard->UsePrimarySelection(false);
      }
#endif
#endif

      return;
   }

   if(evt.Dragging())
   {
      if (!mRightDragging)
         // Update drag end
         view.SetCurrentCursorPosition(
            view.FindCursorPosition( evt.m_x ) );

      return;
   }

   if (evt.RightUp()) {
      const auto selIndex = view.GetSelectedIndex( project );
      if ( selIndex != -1 &&
         LabelTrackView::OverTextBox(
            pTrack->GetLabel( selIndex ), evt.m_x, evt.m_y ) ) {
         // popup menu for editing
         // TODO: handle context menus via CellularPanel?
         view.ShowContextMenu( project );
      }
   }

   return;
}

UIHandle::Result LabelTextHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto &project = *pProject;
   using namespace RefreshCode;
   auto result = LabelDefaultClickHandle::Drag( evt, pProject );

   const wxMouseEvent &event = evt.event;
   auto pLT = TrackList::Get( *pProject ).Lock(mpLT);
   if(pLT)
      HandleTextDragRelease( project, event );

   // locate the initial mouse position
   if (event.LeftIsDown()) {
      if (mLabelTrackStartXPos == -1) {
         mLabelTrackStartXPos = event.m_x;
         mLabelTrackStartYPos = event.m_y;

         auto pView = pLT ? &LabelTrackView::Get( *pLT ) : nullptr;
         if (pLT &&
            (pView->GetSelectedIndex( project ) != -1) &&
             LabelTrackView::OverTextBox(
               pLT->GetLabel(pView->GetSelectedIndex( project )),
               mLabelTrackStartXPos,
               mLabelTrackStartYPos))
            mLabelTrackStartYPos = -1;
      }
      // if initial mouse position in the text box
      // then only drag text
      if (mLabelTrackStartYPos == -1)
         result |= RefreshCell;
   }

   return result;
}

HitTestPreview LabelTextHandle::Preview
(const TrackPanelMouseState &, AudacityProject *)
{
   return HitPreview();
}

UIHandle::Result LabelTextHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   auto result = LabelDefaultClickHandle::Release( evt, pProject, pParent );

   // Only selected a part of a text string and changed track selectedness.
   // No undoable effects.

   if (mChanger) {
      mChanger->Commit();
      mChanger.reset();
   }

   const wxMouseEvent &event = evt.event;
   auto pLT = TrackList::Get( *pProject ).Lock(mpLT);
   if (pLT)
      HandleTextDragRelease( *pProject, event );

   // handle mouse left button up
   if (event.LeftUp())
      mLabelTrackStartXPos = -1;

   return result | RefreshCode::RefreshNone;
}

UIHandle::Result LabelTextHandle::Cancel( AudacityProject *pProject )
{
   // Restore the selection states of tracks
   // Note that we are also relying on LabelDefaultClickHandle::Cancel
   // to restore the selection state of the labels in the tracks.
   auto &viewInfo = ViewInfo::Get( *pProject );
   viewInfo.selectedRegion = mSelectedRegion;
   auto result = LabelDefaultClickHandle::Cancel( pProject );
   return result | RefreshCode::RefreshAll;
}
