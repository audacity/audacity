/**********************************************************************

Audacity: A Digital Audio Editor

LabelGlyphHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "LabelGlyphHandle.h"

#include "../../../HitTestResult.h"
#include "../../../LabelTrack.h"
#include "../../../ProjectHistory.h"
#include "../../../RefreshCode.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../UndoManager.h"
#include "../../../ViewInfo.h"

#include <wx/cursor.h>
#include <wx/translation.h>

LabelTrackHit::LabelTrackHit( const std::shared_ptr<LabelTrack> &pLT )
   : mpLT{ pLT }
{
   pLT->Bind(
      EVT_LABELTRACK_PERMUTED, &LabelTrackHit::OnLabelPermuted, this );
}

LabelTrackHit::~LabelTrackHit()
{
   // Must do this because this sink isn't wxEvtHandler
   mpLT->Unbind(
      EVT_LABELTRACK_PERMUTED, &LabelTrackHit::OnLabelPermuted, this );
}

void LabelTrackHit::OnLabelPermuted( LabelTrackEvent &e )
{
   e.Skip();
   if ( e.mpTrack.lock() != mpLT )
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
}

LabelGlyphHandle::LabelGlyphHandle
(const std::shared_ptr<LabelTrack> &pLT,
 const wxRect &rect, const std::shared_ptr<LabelTrackHit> &pHit)
   : mpHit{ pHit }
   , mpLT{ pLT }
   , mRect{ rect }
{
}

void LabelGlyphHandle::Enter(bool)
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

HitTestPreview LabelGlyphHandle::HitPreview(bool hitCenter)
{
   static wxCursor arrowCursor{ wxCURSOR_ARROW };
   return {
      (hitCenter
         ? _("Drag one or more label boundaries.")
         : _("Drag label boundary.")),
      &arrowCursor
   };
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

   pLT->OverGlyph(*pHit, state.m_x, state.m_y);

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

UIHandle::Result LabelGlyphHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto result = LabelDefaultClickHandle::Click( evt, pProject );

   const wxMouseEvent &event = evt.event;

   auto &viewInfo = ViewInfo::Get( *pProject );
   mpLT->HandleGlyphClick
      (*mpHit, event, mRect, viewInfo, &viewInfo.selectedRegion);

   if (! mpHit->mIsAdjustingLabel )
   {
      // The positive hit test should have ensured otherwise
      //wxASSERT(false);
      result |= RefreshCode::Cancelled;
   }
   else
      // redraw the track.
      result |= RefreshCode::RefreshCell;

   // handle shift+ctrl down
   /*if (event.ShiftDown()) { // && event.ControlDown()) {
      lTrack->SetHighlightedByKey(true);
      Refresh(false);
      return;
   }*/

   return result;
}

UIHandle::Result LabelGlyphHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto result = LabelDefaultClickHandle::Drag( evt, pProject );

   const wxMouseEvent &event = evt.event;
   auto &viewInfo = ViewInfo::Get( *pProject );
   mpLT->HandleGlyphDragRelease
      (*mpHit, event, mRect, viewInfo, &viewInfo.selectedRegion);

   // Refresh all so that the change of selection is redrawn in all tracks
   return result | RefreshCode::RefreshAll | RefreshCode::DrawOverlays;
}

HitTestPreview LabelGlyphHandle::Preview
(const TrackPanelMouseState &, const AudacityProject *)
{
   return HitPreview( (mpHit->mEdge & 4 )!=0);
}

UIHandle::Result LabelGlyphHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   auto result = LabelDefaultClickHandle::Release( evt, pProject, pParent );

   const wxMouseEvent &event = evt.event;
   auto &viewInfo = ViewInfo::Get( *pProject );
   if (mpLT->HandleGlyphDragRelease
          (*mpHit, event, mRect, viewInfo, &viewInfo.selectedRegion)) {
      ProjectHistory::Get( *pProject ).PushState(_("Modified Label"),
         _("Label Edit"),
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
