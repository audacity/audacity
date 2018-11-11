/**********************************************************************

Audacity: A Digital Audio Editor

LabelGlyphHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "LabelGlyphHandle.h"

#include "../../../HitTestResult.h"
#include "../../../LabelTrack.h"
#include "../../../Project.h"
#include "../../../RefreshCode.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../UndoManager.h"
#include "../../../ViewInfo.h"

#include "../../../MemoryX.h"

#include <wx/cursor.h>
#include <wx/translation.h>

LabelGlyphHandle::LabelGlyphHandle
(const std::shared_ptr<LabelTrack> &pLT,
 const wxRect &rect, const LabelTrackHit &hit)
   : mHit{ hit }
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
   if (oldState.mHit.mEdge != newState.mHit.mEdge)
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
   LabelTrackHit hit{};
   pLT->OverGlyph(hit, state.m_x, state.m_y);

   // IF edge!=0 THEN we've set the cursor and we're done.
   // signal this by setting the tip.
   if ( hit.mEdge & 3 )
   {
      auto result = std::make_shared<LabelGlyphHandle>( pLT, rect, hit );
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

   ViewInfo &viewInfo = pProject->GetViewInfo();
   mpLT->HandleGlyphClick
      (mHit, event, mRect, viewInfo, &viewInfo.selectedRegion);

   if (! mHit.mIsAdjustingLabel )
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
   ViewInfo &viewInfo = pProject->GetViewInfo();
   mpLT->HandleGlyphDragRelease
      (mHit, event, mRect, viewInfo, &viewInfo.selectedRegion);

   // Refresh all so that the change of selection is redrawn in all tracks
   return result | RefreshCode::RefreshAll | RefreshCode::DrawOverlays;
}

HitTestPreview LabelGlyphHandle::Preview
(const TrackPanelMouseState &, const AudacityProject *)
{
   return HitPreview( (mHit.mEdge & 4 )!=0);
}

UIHandle::Result LabelGlyphHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   auto result = LabelDefaultClickHandle::Release( evt, pProject, pParent );

   const wxMouseEvent &event = evt.event;
   ViewInfo &viewInfo = pProject->GetViewInfo();
   if (mpLT->HandleGlyphDragRelease
          (mHit, event, mRect, viewInfo, &viewInfo.selectedRegion)) {
      pProject->PushState(_("Modified Label"),
         _("Label Edit"),
         UndoPush::CONSOLIDATE);
   }

   // Refresh all so that the change of selection is redrawn in all tracks
   return result | RefreshCode::RefreshAll | RefreshCode::DrawOverlays;
}

UIHandle::Result LabelGlyphHandle::Cancel(AudacityProject *pProject)
{
   pProject->RollbackState();
   auto result = LabelDefaultClickHandle::Cancel( pProject );
   return result | RefreshCode::RefreshAll;
}
