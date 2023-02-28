/**********************************************************************

Audacity: A Digital Audio Editor

LabelGlyphHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "LabelGlyphHandle.h"

#include "LabelTrackView.h"
#include "../../../HitTestResult.h"
#include "LabelLayout.h"
#include "LabelTrack.h"
#include "ProjectHistory.h"
#include "../../../RefreshCode.h"
#include "../../../TrackPanelMouseEvent.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "SelectionState.h"
#include "ProjectAudioIO.h"
#include "../../../../images/Cursors.h"

#include <wx/cursor.h>
#include <wx/event.h>
#include <wx/translation.h>

#include <cassert>

LabelTrackHit::LabelTrackHit( const std::shared_ptr<LabelTrack> &pLT )
   : mpLT{ pLT }
{
   mSubscription = pLT->Subscribe( *this, &LabelTrackHit::OnLabelPermuted );
}

LabelTrackHit::~LabelTrackHit()
{
}

void LabelTrackHit::OnLabelPermuted( const LabelTrackEvent &e )
{
   if ( e.mpTrack.lock() != mpLT )
      return;
   if ( e.type != LabelTrackEvent::Permutation )
      return;

   auto former = e.mFormerPosition;
   auto present = e.mPresentPosition;

   auto update = [=]( int &index ){
      if ( index == former )
         index = present;
      else if ( former < index && index <= present )
         -- index;
      else if ( former > index && index >= present )
         ++ index;
   };
   
   update( mMouseOverLabelLeft );
   update( mMouseOverLabelRight );
   update( mMouseOverLabel );
}

LabelGlyphHandle::LabelGlyphHandle
(const std::shared_ptr<LabelTrack> &pLT,
 const wxRect &rect, const std::shared_ptr<LabelTrackHit> &pHit)
   : mpHit{ pHit }
   , mpLT{ pLT }
   , mRect{ rect }
{
}

void LabelGlyphHandle::Enter(bool, AudacityProject *)
{
   mChangeHighlight = RefreshCode::RefreshCell;
}

UIHandle::Result LabelGlyphHandle::NeedChangeHighlight
(const LabelGlyphHandle &oldState, const LabelGlyphHandle &newState)
{
   if (oldState.mpHit->mEdge != newState.mpHit->mEdge)
      // pointer moves between the circle and the chevron
      return RefreshCode::RefreshCell;
   return 0;
}

UIHandlePtr LabelGlyphHandle::HitTest
(std::weak_ptr<LabelGlyphHandle> &holder,
 const wxMouseState &state,
 const std::shared_ptr<LabelTrack> &pLT, const wxRect &rect)
{
   // Allocate on heap because there are pointers to it when it is bound as
   // an event sink, therefore it's not copyable; make it shared so
   // LabelGlyphHandle can be copyable:
   auto pHit = std::make_shared<LabelTrackHit>( pLT );

   LabelTrackView::OverGlyph(*pLT, *pHit, state.m_x, state.m_y);

   // IF edge!=0 THEN we've set the cursor and we're done.
   // signal this by setting the tip.
   if ( pHit->mEdge & 3 )
   {
      auto result = std::make_shared<LabelGlyphHandle>( pLT, rect, pHit );
      result = AssignUIHandlePtr(holder, result);
      return result;
   }

   return {};
}

LabelGlyphHandle::~LabelGlyphHandle()
{
}

void LabelGlyphHandle::HandleGlyphClick
(LabelTrackHit &hit, const wxMouseEvent & evt,
 const wxRect & r, const ZoomInfo &zoomInfo,
 NotifyingSelectedRegion &newSel)
{
   if (evt.ButtonDown())
   {
      //OverGlyph sets mMouseOverLabel to be the chosen label.
      const auto pTrack = mpLT;
      LabelTrackView::OverGlyph(*pTrack, hit, evt.m_x, evt.m_y);

      hit.mIsAdjustingLabel = evt.Button(wxMOUSE_BTN_LEFT) &&
         ( hit.mEdge & 3 ) != 0;

      if (hit.mIsAdjustingLabel)
      {
         auto& view = LabelTrackView::Get(*pTrack);
         view.ResetTextSelection();

         double t = 0.0;
         
         // When we start dragging the label(s) we don't want them to jump.
         // so we calculate the displacement of the mouse from the drag center
         // and use that in subsequent dragging calculations.  The mouse stays
         // at the same relative displacement throughout dragging.

         // However, if two label's edges are being dragged
         // then the displacement is relative to the initial average
         // position of them, and in that case there can be a jump of at most
         // a few pixels to bring the two label boundaries to exactly the same
         // position when we start dragging.

         // Dragging of three label edges at the same time is not supported (yet).

         const auto &mLabels = pTrack->GetLabels();
         if( ( hit.mMouseOverLabelRight >= 0 ) &&
             ( hit.mMouseOverLabelLeft >= 0 )
           )
         {
            t = (mLabels[ hit.mMouseOverLabelRight ].getT1() +
                 mLabels[ hit.mMouseOverLabelLeft ].getT0()) / 2.0f;

            // If we're moving two edges of same label then it's a move 
            // (label is shrunk to zero and size of zero is preserved)
            // If we're on a boundary between two different labels, 
            // then it's an adjust.
            // In both cases the two points coalesce.
            // 
            // NOTE: seems that it's not necessary that hitting the both
            // left and right handles mean that we're dealing with a point, 
            // but the range will be turned into a point on click
            bool isPointLabel = hit.mMouseOverLabelLeft == hit.mMouseOverLabelRight;
            // Except!  We don't coalesce if both ends are from the same label and
            // we have deliberately chosen to preserve length, by holding shift down.
            if (!(isPointLabel && evt.ShiftDown()))
            {
               MayAdjustLabel(hit, hit.mMouseOverLabelLeft, -1, false, t);
               MayAdjustLabel(hit, hit.mMouseOverLabelRight, 1, false, t);
               wxASSERT(mLabels[hit.mMouseOverLabelRight].getT1() ==
                  mLabels[hit.mMouseOverLabelLeft].getT0());
            }
         }
         else if( hit.mMouseOverLabelRight >=0)
         {
            t = mLabels[ hit.mMouseOverLabelRight ].getT1();
         }
         else if( hit.mMouseOverLabelLeft >=0)
         {
            t = mLabels[ hit.mMouseOverLabelLeft ].getT0();
         }
         else if (hit.mMouseOverLabel >= 0)
         {
            t = mLabels[hit.mMouseOverLabel].getT0();
         }
         mxMouseDisplacement = zoomInfo.TimeToPosition(t, r.x) - evt.m_x;
      }
   }
}

UIHandle::Result LabelGlyphHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto result = LabelDefaultClickHandle::Click( evt, pProject );

   const wxMouseEvent &event = evt.event;
   auto& selectionState = SelectionState::Get(*pProject);
   auto& tracks = TrackList::Get(*pProject);

   auto &viewInfo = ViewInfo::Get( *pProject );
   HandleGlyphClick(
      *mpHit, event, mRect, viewInfo, viewInfo.selectedRegion);

   if (! mpHit->mIsAdjustingLabel )
   {
      // The positive hit test should have ensured otherwise
      //wxASSERT(false);
      result |= RefreshCode::Cancelled;
   }
   else
      // redraw the track.
      result |= RefreshCode::RefreshCell;

   return result;
}

/// If the index is for a real label, adjust its left or right boundary.
/// @iLabel - index of label, -1 for none.
/// @iEdge - which edge is requested to move, -1 for left +1 for right.
/// @bAllowSwapping - if we can switch which edge is being dragged.
/// fNewTime - the NEW time for this edge of the label.
void LabelGlyphHandle::MayAdjustLabel
( LabelTrackHit &hit, int iLabel, int iEdge, bool bAllowSwapping, double fNewTime)
{
   if( iLabel < 0 )
      return;

   const auto pTrack = mpLT;
   const auto &mLabels = pTrack->GetLabels();
   auto labelStruct = mLabels[ iLabel ];

   // Adjust the requested edge.
   bool flipped = LabelLayout::AdjustEdge(labelStruct, iEdge, fNewTime);
   // If the edges did not swap, then we are done.
   if( ! flipped ) {
      pTrack->SetLabel( iLabel, labelStruct );
      return;
   }

   // If swapping's not allowed we must also move the edge
   // we didn't move.  Then we're done.
   if( !bAllowSwapping )
   {
      LabelLayout::AdjustEdge(labelStruct, -iEdge, fNewTime);
      pTrack->SetLabel( iLabel, labelStruct );
      return;
   }

   pTrack->SetLabel( iLabel, labelStruct );

   // Swap our record of what we are dragging.
   std::swap( hit.mMouseOverLabelLeft, hit.mMouseOverLabelRight );
}

// If the index is for a real label, adjust its left and right boundary.
void LabelGlyphHandle::MayMoveLabel( int iLabel, int iEdge, double fNewTime)
{
   if( iLabel < 0 )
      return;

   const auto pTrack = mpLT;
   const auto &mLabels = pTrack->GetLabels();
   auto labelStruct = mLabels[ iLabel ];
   LabelLayout::MoveLabel(labelStruct, iEdge, fNewTime);
   pTrack->SetLabel( iLabel, labelStruct );
}

// Constrain function, as in processing/arduino.
// returned value will be between min and max (inclusive).
static int Constrain( int value, int min, int max )
{
   wxASSERT( min <= max );
   int result=value;
   if( result < min )
      result=min;
   if( result > max )
      result=max;
   return result;
}

bool LabelGlyphHandle::HandleGlyphDragRelease
(AudacityProject &project,
 LabelTrackHit &hit, const wxMouseEvent & evt,
 wxRect & r, const ZoomInfo &zoomInfo,
 NotifyingSelectedRegion &newSel)
{
   const auto pTrack = mpLT;
   const auto &mLabels = pTrack->GetLabels();
   if(evt.LeftUp())
   {
      bool updated = false;
      if( hit.mMouseOverLabelLeft >= 0 ) {
         auto labelStruct = mLabels[ hit.mMouseOverLabelLeft ];
         auto &u = LabelLayout::Get(labelStruct).updated;
         updated = u;
         u = false;
         pTrack->SetLabel( hit.mMouseOverLabelLeft, labelStruct );
      }
      if( hit.mMouseOverLabelRight >= 0 ) {
         auto labelStruct = mLabels[ hit.mMouseOverLabelRight ];
         auto &u = LabelLayout::Get(labelStruct).updated;
         updated = updated || u;
         u = false;
         pTrack->SetLabel( hit.mMouseOverLabelRight, labelStruct );
      }

      if (hit.mMouseOverLabel >= 0)
      {
          auto labelStruct = mLabels[hit.mMouseOverLabel];
          auto &layout = LabelLayout::Get(labelStruct);
          if (!layout.updated)
          {
              //happens on click over bar between handles (without moving a cursor)
              newSel = labelStruct.selectedRegion;

              // IF the user clicked a label, THEN select all other tracks by Label
              // do nothing if at least one other track is selected
              auto& selectionState = SelectionState::Get(project);
              auto& tracks = TrackList::Get(project);

              bool done = tracks.Selected().any_of(
                  [&](const Track* track) { return track != static_cast<Track*>(pTrack.get()); }
              );

              if (!done) {
                  //otherwise, select all tracks
                  for (auto t : tracks)
                      selectionState.SelectTrack(*t, true, true);
              }

              // Do this after, for its effect on TrackPanel's memory of last selected
              // track (which affects shift-click actions)
              assert(pTrack->IsLeader()); // It's a label track
              selectionState.SelectTrack(*pTrack, true, true);

              // PRL: bug1659 -- make selection change undo correctly
              updated = !ProjectAudioIO::Get(project).IsAudioActive();
              
              auto& view = LabelTrackView::Get(*pTrack);
              view.SetNavigationIndex(hit.mMouseOverLabel);
          }
          else
          {
              layout.updated = false;
              pTrack->SetLabel(hit.mMouseOverLabel, labelStruct);
              updated = true;
          }
      }

      hit.mIsAdjustingLabel = false;
      hit.mMouseOverLabelLeft  = -1;
      hit.mMouseOverLabelRight = -1;
      hit.mMouseOverLabel = -1;
      return updated;
   }

   if(evt.Dragging())
   {
      //If we are currently adjusting a label,
      //just reset its value and redraw.
      // LL:  Constrain to inside track rectangle for now.  Should be changed
      //      to allow scrolling while dragging labels
      int x = Constrain( evt.m_x + mxMouseDisplacement - r.x, 0, r.width);

      double fNewX = zoomInfo.PositionToTime(x, 0);
      // Moving the whole ranged label(s)
      if (hit.mMouseOverLabel != -1)
      {
         if (evt.ShiftDown())
         {
            auto dt = fNewX - mLabels[hit.mMouseOverLabel].getT0();
            for (auto i = 0, count = static_cast<int>(mLabels.size()); i < count; ++i)
               MayMoveLabel(i, -1, mLabels[i].getT0() + dt);
         }
         else
            MayMoveLabel(hit.mMouseOverLabel, -1, fNewX);
      }
      // If we're on the 'dot' and nowe're moving,
      // Though shift-down inverts that.
      // and if both edges the same, then we're always moving the label.
      else if((hit.mMouseOverLabelLeft == hit.mMouseOverLabelRight) || evt.ShiftDown())
      {
         MayMoveLabel( hit.mMouseOverLabelLeft,  -1, fNewX );
         MayMoveLabel( hit.mMouseOverLabelRight, +1, fNewX );
      }
      else
      {
         // If exactly one edge is selected we allow swapping
         bool bAllowSwapping =
            (hit.mMouseOverLabelLeft >= 0) !=
            (hit.mMouseOverLabelRight >= 0);
         MayAdjustLabel( hit, hit.mMouseOverLabelLeft,  -1, bAllowSwapping, fNewX );
         MayAdjustLabel( hit, hit.mMouseOverLabelRight, +1, bAllowSwapping, fNewX );
      }

      const auto &view = LabelTrackView::Get( *pTrack );
      auto navigationIndex = view.GetNavigationIndex(project);
      if(navigationIndex != -1 &&
          (navigationIndex == hit.mMouseOverLabel ||
              navigationIndex == hit.mMouseOverLabelLeft ||
              navigationIndex == hit.mMouseOverLabelRight))
      {
         //Set the selection region to be equal to
         //the NEW size of the label.
         newSel = mLabels[navigationIndex].selectedRegion;
      }
      pTrack->SortLabels();
   }

   return false;
}

UIHandle::Result LabelGlyphHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto result = LabelDefaultClickHandle::Drag( evt, pProject );

   const wxMouseEvent &event = evt.event;
   auto &viewInfo = ViewInfo::Get( *pProject );
   HandleGlyphDragRelease(
      *pProject, *mpHit, event, mRect, viewInfo, viewInfo.selectedRegion);

   // Refresh all so that the change of selection is redrawn in all tracks
   return result | RefreshCode::RefreshAll | RefreshCode::DrawOverlays;
}

HitTestPreview LabelGlyphHandle::Preview
(const TrackPanelMouseState &, AudacityProject *)
{
   static wxCursor arrowCursor{ wxCURSOR_ARROW };
   static auto handOpenCursor =
      MakeCursor(wxCURSOR_HAND, RearrangeCursorXpm, 16, 16);
   static auto handClosedCursor =
      MakeCursor(wxCURSOR_HAND, RearrangingCursorXpm, 16, 16);

   if (mpHit->mMouseOverLabel != -1)
   {
      return {
         XO("Drag label. Hold shift and drag to move all labels on the same track."),
         mpHit->mIsAdjustingLabel ? &*handClosedCursor : &*handOpenCursor
      };
   }
   else if ((mpHit->mEdge & 4) != 0)
      return { XO("Drag one or more label boundaries."), &arrowCursor };
   else
      return { XO("Drag label boundary."), &arrowCursor };
}

UIHandle::Result LabelGlyphHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   auto result = LabelDefaultClickHandle::Release( evt, pProject, pParent );

   const wxMouseEvent &event = evt.event;
   auto &viewInfo = ViewInfo::Get( *pProject );
   if (HandleGlyphDragRelease(
         *pProject, *mpHit, event, mRect, viewInfo, viewInfo.selectedRegion)) {
      ProjectHistory::Get( *pProject ).PushState(XO("Modified Label"),
         XO("Label Edit"),
         UndoPush::CONSOLIDATE);
   }

   // Refresh all so that the change of selection is redrawn in all tracks
   return result | RefreshCode::RefreshAll | RefreshCode::DrawOverlays;
}

UIHandle::Result LabelGlyphHandle::Cancel(AudacityProject *pProject)
{
   ProjectHistory::Get( *pProject ).RollbackState();
   auto result = LabelDefaultClickHandle::Cancel( pProject );
   return result | RefreshCode::RefreshAll;
}
