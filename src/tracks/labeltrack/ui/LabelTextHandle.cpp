/**********************************************************************

Audacity: A Digital Audio Editor

LabelTextHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "LabelTextHandle.h"

#include "../../../Experimental.h"

#include "../../../HitTestResult.h"
#include "../../../LabelTrack.h"
#include "../../../Project.h"
#include "../../../RefreshCode.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../ViewInfo.h"
#include "../../../images/Cursors.h"

LabelTextHandle::LabelTextHandle
( const std::shared_ptr<LabelTrack> &pLT, int labelNum )
   : mpLT{ pLT }
   , mLabelNum{ labelNum }
{
}

void LabelTextHandle::Enter(bool)
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
      _("Click to edit label text"),
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
       (labelNum = pLT->OverATextBox(state.m_x, state.m_y) ) >= 0) {
      auto result = std::make_shared<LabelTextHandle>( pLT, labelNum );
      result = AssignUIHandlePtr(holder, result);
      return result;
   }

   return {};
}

LabelTextHandle::~LabelTextHandle()
{
}

UIHandle::Result LabelTextHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto pLT = mpLT.lock();
   if (!pLT)
      return RefreshCode::Cancelled;

   auto result = LabelDefaultClickHandle::Click( evt, pProject );

   auto &selectionState = pProject->GetSelectionState();
   auto &tracks = TrackList::Get( *pProject );
   mChanger =
      std::make_shared< SelectionStateChanger >( selectionState, tracks );

   const wxMouseEvent &event = evt.event;
   auto &viewInfo = ViewInfo::Get( *pProject );

   mSelectedRegion = viewInfo.selectedRegion;
   pLT->HandleTextClick( event, evt.rect, viewInfo, &viewInfo.selectedRegion );
   wxASSERT(pLT->HasSelection());

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
   const bool unsafe = pProject->IsAudioActive();
   if (!unsafe)
      pProject->ModifyState(false);

   return result | RefreshCode::RefreshCell | RefreshCode::UpdateSelection;
}

UIHandle::Result LabelTextHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   auto result = LabelDefaultClickHandle::Drag( evt, pProject );

   const wxMouseEvent &event = evt.event;
   auto pLT = TrackList::Get( *pProject ).Lock(mpLT);
   if(pLT)
      pLT->HandleTextDragRelease(event);

   // locate the initial mouse position
   if (event.LeftIsDown()) {
      if (mLabelTrackStartXPos == -1) {
         mLabelTrackStartXPos = event.m_x;
         mLabelTrackStartYPos = event.m_y;

         if (pLT &&
            (pLT->getSelectedIndex() != -1) &&
             pLT->OverTextBox(
               pLT->GetLabel(pLT->getSelectedIndex()),
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
(const TrackPanelMouseState &, const AudacityProject *)
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
      pLT->HandleTextDragRelease(event);

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
