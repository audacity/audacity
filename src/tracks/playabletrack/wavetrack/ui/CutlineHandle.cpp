/**********************************************************************

Audacity: A Digital Audio Editor

CutlineHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "CutlineHandle.h"

#include "../../../../Experimental.h"

#include "../../../../MemoryX.h"

#include "../../../../HitTestResult.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../Snap.h" // for kPixelTolerance
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../UndoManager.h"
#include "../../../../WaveTrack.h"
#include "../../../../WaveTrackLocation.h"
#include "../../../../../images/Cursors.h"

CutlineHandle::CutlineHandle
( const std::shared_ptr<WaveTrack> &pTrack, WaveTrackLocation location )
   : mpTrack{ pTrack }
   , mLocation{ location }
{
}

void CutlineHandle::Enter(bool)
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

HitTestPreview CutlineHandle::HitPreview(bool cutline, bool unsafe)
{
   static auto disabledCursor =
      ::MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
   static wxCursor arrowCursor{ wxCURSOR_ARROW };
   return {
      (cutline
       ? _("Left-Click to expand, Right-Click to remove")
       : _("Left-Click to merge clips")),
      (unsafe
       ? &*disabledCursor
       : &arrowCursor)
   };
}
namespace
{
   int FindMergeLine(WaveTrack *track, double time)
   {
      const double tolerance = 0.5 / track->GetRate();
      int ii = 0;
      for (const auto loc: track->GetCachedLocations()) {
         if (loc.typ == WaveTrackLocation::locationMergePoint &&
             fabs(time - loc.pos) < tolerance)
            return ii;
         ++ii;
      }
      return -1;
   }
   
   bool IsOverCutline
      (const ViewInfo &viewInfo, WaveTrack * track,
       const wxRect &rect, const wxMouseState &state,
       WaveTrackLocation *pmLocation)
   {
      for (auto loc: track->GetCachedLocations())
      {
         const double x = viewInfo.TimeToPosition(loc.pos);
         if (x >= 0 && x < rect.width)
         {
            wxRect locRect;
            locRect.width = 2 * kPixelTolerance - 1;
            locRect.x = (int)(rect.x + x) - locRect.width / 2;
            locRect.y = rect.y;
            locRect.height = rect.height;
            if (locRect.Contains(state.m_x, state.m_y))
            {
               if (pmLocation)
                  *pmLocation = loc;
               return true;
            }
         }
      }

      return false;
   }
}

UIHandlePtr CutlineHandle::HitTest
(std::weak_ptr<CutlineHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const AudacityProject *pProject,
 const std::shared_ptr<WaveTrack> &pTrack)
{
   const ViewInfo &viewInfo = pProject->GetViewInfo();
   /// method that tells us if the mouse event landed on an
   /// editable Cutline

   WaveTrackLocation location;
   if (!IsOverCutline(viewInfo, pTrack.get(), rect, state, &location))
      return {};

   auto result = std::make_shared<CutlineHandle>( pTrack, location );
   result = AssignUIHandlePtr( holder, result );
   return result;
}

CutlineHandle::~CutlineHandle()
{
}

UIHandle::Result CutlineHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const bool unsafe = pProject->IsAudioActive();
   if ( unsafe )
      return Cancelled;

   const wxMouseEvent &event = evt.event;
   ViewInfo &viewInfo = pProject->GetViewInfo();

   // Can affect the track by merging clips, expanding a cutline, or
   // deleting a cutline.
   // All the change is done at button-down.  Button-up just commits the undo item.

   /// Someone has just clicked the mouse.  What do we do?

   // FIXME: Disable this and return true when CutLines aren't showing?
   // (Don't use gPrefs-> for the fix as registry access is slow).

   // Cutline data changed on either branch, so refresh the track display.
   UIHandle::Result result = RefreshCell;

   if (event.LeftDown())
   {
      if (mLocation.typ == WaveTrackLocation::locationCutLine)
      {
         mOperation = Expand;
         mStartTime = viewInfo.selectedRegion.t0();
         mEndTime = viewInfo.selectedRegion.t1();

         // When user presses left button on cut line, expand the line again
         double cutlineStart = 0, cutlineEnd = 0;
         double *pCutlineStart = &cutlineStart, *pCutlineEnd = &cutlineEnd;

         for (auto channel :
              TrackList::Channels(mpTrack.get())) {
            channel->ExpandCutLine(
               mLocation.pos, pCutlineStart, pCutlineEnd);
            if ( channel == mpTrack.get() )
               pCutlineStart = pCutlineEnd = nullptr;
         }

         viewInfo.selectedRegion.setTimes(cutlineStart, cutlineEnd);
         result |= UpdateSelection;
      }
      else if (mLocation.typ == WaveTrackLocation::locationMergePoint) {
         const double pos = mLocation.pos;
         for (auto channel :
              TrackList::Channels(mpTrack.get())) {
            // Don't assume correspondence of merge points across channels!
            int idx = FindMergeLine(channel, pos);
            if (idx >= 0) {
               WaveTrack::Location location =
                  channel->GetCachedLocations()[idx];
               channel->MergeClips(
                  location.clipidx1, location.clipidx2);
            }
         }

         mOperation = Merge;
      }
   }
   else if (event.RightDown())
   {
      bool removed = false;
      for (auto channel :
           TrackList::Channels(mpTrack.get()))
         removed = channel->RemoveCutLine(mLocation.pos) || removed;

      if (!removed)
         // Nothing happened, make no Undo item
         return Cancelled;

      mOperation = Remove;
   }
   else
      result = RefreshNone;

   return result;
}

UIHandle::Result CutlineHandle::Drag
(const TrackPanelMouseEvent &, AudacityProject *)
{
   return RefreshCode::RefreshNone;
}

HitTestPreview CutlineHandle::Preview
(const TrackPanelMouseState &, const AudacityProject *pProject)
{
   const bool unsafe = pProject->IsAudioActive();
   auto bCutline = (mLocation.typ == WaveTrackLocation::locationCutLine);
   return HitPreview( bCutline, unsafe );
}

UIHandle::Result CutlineHandle::Release
(const TrackPanelMouseEvent &, AudacityProject *pProject, wxWindow *)
{
   UIHandle::Result result = RefreshCode::RefreshNone;

   // Only now commit the result to the undo stack
   AudacityProject *const project = pProject;
   switch (mOperation) {
   default:
      wxASSERT(false);
   case Merge:
      project->PushState(_("Merged Clips"), _("Merge"), UndoPush::CONSOLIDATE);
      break;
   case Expand:
      project->PushState(_("Expanded Cut Line"), _("Expand"));
      result |= RefreshCode::UpdateSelection;
      break;
   case Remove:
      project->PushState(_("Removed Cut Line"), _("Remove"));
      break;
   }

   // Nothing to do for the display
   return result;
}

UIHandle::Result CutlineHandle::Cancel(AudacityProject *pProject)
{
   using namespace RefreshCode;
   UIHandle::Result result = RefreshCell;
   pProject->RollbackState();
   if (mOperation == Expand) {
      AudacityProject *const project = pProject;
      auto &selectedRegion = project->GetViewInfo().selectedRegion;
      selectedRegion.setTimes( mStartTime, mEndTime );
      result |= UpdateSelection;
   }
   return result;
}
