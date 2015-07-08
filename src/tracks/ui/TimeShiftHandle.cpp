/**********************************************************************

Audacity: A Digital Audio Editor

TimeShiftHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "TimeShiftHandle.h"

#include "TrackControls.h"
#include "../../AColor.h"
#include "../../HitTestResult.h"
#include "../../Project.h"
#include "../../RefreshCode.h"
#include "../../Snap.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../toolbars/ToolsToolBar.h"
#include "../../UndoManager.h"
#include "../../WaveTrack.h"
#include "../../../images/Cursors.h"

TimeShiftHandle::TimeShiftHandle()
{
}

TimeShiftHandle &TimeShiftHandle::Instance()
{
   static TimeShiftHandle instance;
   return instance;
}

HitTestPreview TimeShiftHandle::HitPreview
(const AudacityProject *pProject, bool unsafe)
{
   static auto disabledCursor =
      ::MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
   static auto slideCursor =
      MakeCursor(wxCURSOR_SIZEWE, TimeCursorXpm, 16, 16);
   const ToolsToolBar *const ttb = pProject->GetToolsToolBar();
   return {
      ttb->GetMessageForTool(slideTool),
      (unsafe
       ? &*disabledCursor
       : &*slideCursor)
   };
}

HitTestResult TimeShiftHandle::HitAnywhere(const AudacityProject *pProject)
{
   // After all that, it still may be unsafe to drag.
   // Even if so, make an informative cursor change from default to "banned."
   const bool unsafe = pProject->IsAudioActive();
   return {
      HitPreview(pProject, unsafe),
      (unsafe
       ? NULL
       : &Instance())
   };
}

HitTestResult TimeShiftHandle::HitTest
   (const wxMouseEvent & event, const wxRect &rect, const AudacityProject *pProject)
{
   /// method that tells us if the mouse event landed on a
   /// time-slider that allows us to time shift the sequence.
   /// (Those are the two "grips" drawn at left and right edges for multi tool mode.)

   // Perhaps we should delegate this to TrackArtist as only TrackArtist
   // knows what the real sizes are??

   // The drag Handle width includes border, width and a little extra margin.
   const int adjustedDragHandleWidth = 14;
   // The hotspot for the cursor isn't at its centre.  Adjust for this.
   const int hotspotOffset = 5;

   // We are doing an approximate test here - is the mouse in the right or left border?
   if (!(event.m_x + hotspotOffset < rect.x + adjustedDragHandleWidth ||
       event.m_x + hotspotOffset >= rect.x + rect.width - adjustedDragHandleWidth))
      return {};

   return HitAnywhere(pProject);
}

TimeShiftHandle::~TimeShiftHandle()
{
}

namespace {
   // Don't count right channels.
   WaveTrack *NthAudioTrack(TrackList &list, int nn)
   {
      if (nn >= 0) {
         TrackListOfKindIterator iter(Track::Wave, &list);
         Track *pTrack = iter.First();
         while (pTrack && nn--)
            pTrack = iter.Next(true);
         return static_cast<WaveTrack*>(pTrack);
      }

      return NULL;
   }

   // Don't count right channels.
   int TrackPosition(TrackList &list, Track *pFindTrack)
   {
      Track *const partner = pFindTrack->GetLink();
      TrackListOfKindIterator iter(Track::Wave, &list);
      int nn = 0;
      for (Track *pTrack = iter.First(); pTrack; pTrack = iter.Next(true), ++nn) {
         if (pTrack == pFindTrack ||
             pTrack == partner)
            return nn;
      }
      return -1;
   }
}

UIHandle::Result TimeShiftHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   const ViewInfo &viewInfo = pProject->GetViewInfo();

   Track *const pTrack = static_cast<Track*>(evt.pCell);

   using namespace RefreshCode;

   const bool unsafe = pProject->IsAudioActive();
   if (unsafe)
      return Cancelled;

   TrackList *const trackList = pProject->GetTracks();

   mClipMoveState.clear();
   mDidSlideVertically = false;

   ToolsToolBar *const ttb = pProject->GetToolsToolBar();
   const bool multiToolModeActive = (ttb && ttb->IsDown(multiTool));

   const double clickTime =
      viewInfo.PositionToTime(event.m_x, rect.x);
   mClipMoveState.capturedClipIsSelection =
      (pTrack->GetSelected() &&
       clickTime >= viewInfo.selectedRegion.t0() &&
       clickTime < viewInfo.selectedRegion.t1());

   WaveTrack *wt = pTrack->GetKind() == Track::Wave
      ? static_cast<WaveTrack*>(pTrack) : nullptr;

   if ((wt
#ifdef USE_MIDI
      || pTrack->GetKind() == Track::Note
#endif
      ) && !event.ShiftDown())
   {
#ifdef USE_MIDI
      if (!wt)
         mClipMoveState.capturedClip = nullptr;
      else
#endif
      {
         mClipMoveState.capturedClip = wt->GetClipAtX(event.m_x);
         if (mClipMoveState.capturedClip == NULL)
            return Cancelled;
      }

      TrackPanel::CreateListOfCapturedClips
         ( mClipMoveState, viewInfo, *pTrack, *trackList,
           pProject->IsSyncLocked(), clickTime );
   }
   else {
      // Shift was down, or track was not Wave or Note
      mClipMoveState.capturedClip = NULL;
      mClipMoveState.capturedClipArray.clear();
   }

   mSlideUpDownOnly = event.CmdDown() && !multiToolModeActive;
   mCapturedTrack = pTrack;
   mRect = rect;
   mMouseClickX = event.m_x;
   const double selStart = viewInfo.PositionToTime(event.m_x, mRect.x);
   mSnapManager = std::make_unique<SnapManager>(trackList,
                                  &viewInfo,
                                  &mClipMoveState.capturedClipArray,
                                  &mClipMoveState.trackExclusions,
                                  true); // don't snap to time
   mClipMoveState.snapLeft = -1;
   mClipMoveState.snapRight = -1;
   mSnapPreferRightEdge =
      mClipMoveState.capturedClip &&
      (fabs(selStart - mClipMoveState.capturedClip->GetEndTime()) <
       fabs(selStart - mClipMoveState.capturedClip->GetStartTime()));

   return RefreshNone;
}

UIHandle::Result TimeShiftHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   const wxMouseEvent &event = evt.event;
   ViewInfo &viewInfo = pProject->GetViewInfo();

   // We may switch pTrack to its stereo partner below
   Track *pTrack = dynamic_cast<Track*>(evt.pCell);

   // Uncommenting this permits drag to continue to work even over the controls area
   /*
   pTrack = static_cast<CommonTrackPanelCell*>(evt.pCell)->FindTrack();
   */

   if (!pTrack) {
      // Allow sliding if the pointer is not over any track, but only if x is
      // within the bounds of the tracks area.
      if (event.m_x >= mRect.GetX() &&
         event.m_x < mRect.GetX() + mRect.GetWidth())
          pTrack = mCapturedTrack;
   }

   if (!pTrack)
      return RefreshCode::RefreshNone;

   using namespace RefreshCode;
   const bool unsafe = pProject->IsAudioActive();
   if (unsafe) {
      this->Cancel(pProject);
      return RefreshAll | Cancelled;
   }

   TrackList *const trackList = pProject->GetTracks();

   // GM: DoSlide now implementing snap-to
   // samples functionality based on sample rate.

   // Start by undoing the current slide amount; everything
   // happens relative to the original horizontal position of
   // each clip...
#ifdef USE_MIDI
   if (mClipMoveState.capturedClipArray.size())
#else
   if (mClipMoveState.capturedClip)
#endif
   {
      for (unsigned ii = 0; ii < mClipMoveState.capturedClipArray.size(); ++ii) {
         if (mClipMoveState.capturedClipArray[ii].clip)
            mClipMoveState.capturedClipArray[ii].clip->Offset
               ( -mClipMoveState.hSlideAmount );
         else
            mClipMoveState.capturedClipArray[ii].track->Offset
               ( -mClipMoveState.hSlideAmount );
      }
   }
   else {
      // Was a shift-click
      mCapturedTrack->Offset( -mClipMoveState.hSlideAmount );
      Track *const link = mCapturedTrack->GetLink();
      if (link)
         link->Offset( -mClipMoveState.hSlideAmount );
   }

   if ( mClipMoveState.capturedClipIsSelection ) {
      // Slide the selection, too
      viewInfo.selectedRegion.move( -mClipMoveState.hSlideAmount );
   }
   mClipMoveState.hSlideAmount = 0.0;

   double desiredSlideAmount;
   if (mSlideUpDownOnly) {
      desiredSlideAmount = 0.0;
   }
   else {
      desiredSlideAmount =
         viewInfo.PositionToTime(event.m_x) -
         viewInfo.PositionToTime(mMouseClickX);
      bool trySnap = false;
      double clipLeft = 0, clipRight = 0;
#ifdef USE_MIDI
      if (pTrack->GetKind() == Track::Wave) {
         WaveTrack *const mtw = static_cast<WaveTrack*>(pTrack);
         const double rate = mtw->GetRate();
         // set it to a sample point
         desiredSlideAmount = rint(desiredSlideAmount * rate) / rate;
      }

      // Adjust desiredSlideAmount using SnapManager
      if (mSnapManager.get() && mClipMoveState.capturedClipArray.size()) {
         trySnap = true;
         if (mClipMoveState.capturedClip) {
            clipLeft = mClipMoveState.capturedClip->GetStartTime()
               + desiredSlideAmount;
            clipRight = mClipMoveState.capturedClip->GetEndTime()
               + desiredSlideAmount;
         }
         else {
            clipLeft = mCapturedTrack->GetStartTime() + desiredSlideAmount;
            clipRight = mCapturedTrack->GetEndTime() + desiredSlideAmount;
         }
      }
#else
      {
         trySnap = true;
         if (pTrack->GetKind() == Track::Wave) {
            auto wt = static_cast<const WaveTrack *>(pTrack);
            const double rate = wt->GetRate();
            // set it to a sample point
            desiredSlideAmount = rint(desiredSlideAmount * rate) / rate;
            if (mSnapManager && mClipMoveState.capturedClip) {
               clipLeft = mClipMoveState.capturedClip->GetStartTime()
                  + desiredSlideAmount;
               clipRight = mClipMoveState.capturedClip->GetEndTime()
                 + desiredSlideAmount;
            }
         }
      }
#endif
      if (trySnap)
      {
         double newClipLeft = clipLeft;
         double newClipRight = clipRight;

         bool dummy1, dummy2;
         mSnapManager->Snap(mCapturedTrack, clipLeft, false, &newClipLeft,
            &dummy1, &dummy2);
         mSnapManager->Snap(mCapturedTrack, clipRight, false, &newClipRight,
            &dummy1, &dummy2);

         // Only one of them is allowed to snap
         if (newClipLeft != clipLeft && newClipRight != clipRight) {
            // Un-snap the un-preferred edge
            if (mSnapPreferRightEdge)
               newClipLeft = clipLeft;
            else
               newClipRight = clipRight;
         }

         // Take whichever one snapped (if any) and compute the NEW desiredSlideAmount
         mClipMoveState.snapLeft = -1;
         mClipMoveState.snapRight = -1;
         if (newClipLeft != clipLeft) {
            const double difference = (newClipLeft - clipLeft);
            desiredSlideAmount += difference;
            mClipMoveState.snapLeft =
               viewInfo.TimeToPosition(newClipLeft, mRect.x);
         }
         else if (newClipRight != clipRight) {
            const double difference = (newClipRight - clipRight);
            desiredSlideAmount += difference;
            mClipMoveState.snapRight =
               viewInfo.TimeToPosition(newClipRight, mRect.x);
         }
      }
   }

   // Scroll during vertical drag.
   // EnsureVisible(pTrack); //vvv Gale says this has problems on Linux, per bug 393 thread. Revert for 2.0.2.
   bool slidVertically = false;

   // If the mouse is over a track that isn't the captured track,
   // decide which tracks the captured clips should go to.
   if (mClipMoveState.capturedClip &&
       pTrack != mCapturedTrack &&
       pTrack->GetKind() == Track::Wave
       /* && !mCapturedClipIsSelection*/)
   {
      const int diff =
         TrackPosition(*trackList, pTrack) -
         TrackPosition(*trackList, mCapturedTrack);
      for ( unsigned ii = 0, nn = mClipMoveState.capturedClipArray.size();
            ii < nn; ++ii ) {
         TrackClip &trackClip = mClipMoveState.capturedClipArray[ii];
         if (trackClip.clip) {
            // Move all clips up or down by an equal count of audio tracks.
            Track *const pSrcTrack = trackClip.track;
            auto pDstTrack = NthAudioTrack(*trackList,
               diff + TrackPosition(*trackList, pSrcTrack));
            // Can only move mono to mono, or left to left, or right to right
            // And that must be so for each captured clip
            bool stereo = (pSrcTrack->GetLink() != 0);
            if (pDstTrack && stereo && !pSrcTrack->GetLinked())
               // Assume linked track is wave or null
               pDstTrack = static_cast<WaveTrack*>(pDstTrack->GetLink());
            bool ok = pDstTrack &&
            (stereo == (pDstTrack->GetLink() != 0)) &&
            (!stereo || (pSrcTrack->GetLinked() == pDstTrack->GetLinked()));
            if (ok)
               trackClip.dstTrack = pDstTrack;
            else
               return RefreshAll;
         }
      }

      // Having passed that test, remove clips temporarily from their
      // tracks, so moving clips don't interfere with each other
      // when we call CanInsertClip()
      for ( unsigned ii = 0, nn = mClipMoveState.capturedClipArray.size();
            ii < nn;  ++ii ) {
         TrackClip &trackClip = mClipMoveState.capturedClipArray[ii];
         WaveClip *const pSrcClip = trackClip.clip;
         if (pSrcClip)
            trackClip.holder =
               // Assume track is wave because it has a clip
               static_cast<WaveTrack*>(trackClip.track)->
                  RemoveAndReturnClip(pSrcClip);
      }

      // Now check that the move is possible
      bool ok = true;
      for ( unsigned ii = 0, nn = mClipMoveState.capturedClipArray.size();
            ok && ii < nn; ++ii) {
         TrackClip &trackClip = mClipMoveState.capturedClipArray[ii];
         WaveClip *const pSrcClip = trackClip.clip;
         if (pSrcClip)
            ok = trackClip.dstTrack->CanInsertClip(pSrcClip);
      }

      if (!ok) {
         // Failure -- put clips back where they were
         for ( unsigned ii = 0, nn = mClipMoveState.capturedClipArray.size();
               ii < nn;  ++ii) {
            TrackClip &trackClip = mClipMoveState.capturedClipArray[ii];
            WaveClip *const pSrcClip = trackClip.clip;
            if (pSrcClip)
               // Assume track is wave because it has a clip
                  static_cast<WaveTrack*>(trackClip.track)->
                     AddClip(std::move(trackClip.holder));
         }
         return RefreshAll;
      }
      else {
         // Do the vertical moves of clips
         for ( unsigned ii = 0, nn = mClipMoveState.capturedClipArray.size();
               ii < nn; ++ii ) {
            TrackClip &trackClip = mClipMoveState.capturedClipArray[ii];
            WaveClip *const pSrcClip = trackClip.clip;
            if (pSrcClip) {
               const auto dstTrack = trackClip.dstTrack;
               dstTrack->AddClip(std::move(trackClip.holder));
               trackClip.track = dstTrack;
            }
         }

         mCapturedTrack = pTrack;
         mDidSlideVertically = true;

         // Make the offset permanent; start from a "clean slate"
         mMouseClickX = event.m_x;
      }

      // Not done yet, check for horizontal movement.
      slidVertically = true;
   }

   if (desiredSlideAmount == 0.0)
      return RefreshAll;

   mClipMoveState.hSlideAmount = desiredSlideAmount;

   TrackPanel::DoSlideHorizontal( mClipMoveState, *trackList, *mCapturedTrack );

   if (mClipMoveState.capturedClipIsSelection) {
      // Slide the selection, too
      viewInfo.selectedRegion.move( mClipMoveState.hSlideAmount );
   }


   if (slidVertically) {
      // NEW origin
      mClipMoveState.hSlideAmount = 0;
   }

   return RefreshAll;
}

HitTestPreview TimeShiftHandle::Preview
(const TrackPanelMouseEvent &, const AudacityProject *pProject)
{
   return HitPreview(pProject, false);
}

UIHandle::Result TimeShiftHandle::Release
(const TrackPanelMouseEvent &, AudacityProject *pProject,
 wxWindow *)
{
   using namespace RefreshCode;
   const bool unsafe = pProject->IsAudioActive();
   if (unsafe)
      return this->Cancel(pProject);

   Result result = RefreshNone;

   mCapturedTrack = NULL;
   mSnapManager.reset(NULL);
   mClipMoveState.capturedClipArray.clear();

   // Do not draw yellow lines
   if ( mClipMoveState.snapLeft != -1 || mClipMoveState.snapRight != -1) {
      mClipMoveState.snapLeft = mClipMoveState.snapRight = -1;
      result |= RefreshAll;
   }

   if ( !mDidSlideVertically && mClipMoveState.hSlideAmount == 0 )
      return result;
   
   for ( size_t ii = 0; ii < mClipMoveState.capturedClipArray.size(); ++ii )
   {
      TrackClip &trackClip = mClipMoveState.capturedClipArray[ii];
      WaveClip* pWaveClip = trackClip.clip;
      // Note that per AddClipsToCaptured(Track *t, double t0, double t1),
      // in the non-WaveTrack case, the code adds a NULL clip to mCapturedClipArray,
      // so we have to check for that any time we're going to deref it.
      // Previous code did not check it here, and that caused bug 367 crash.
      if (pWaveClip &&
         trackClip.track != trackClip.origTrack)
      {
         // Now that user has dropped the clip into a different track,
         // make sure the sample rate matches the destination track (mCapturedTrack).
         // Assume the clip was dropped in a wave track
         pWaveClip->Resample
            (static_cast<WaveTrack*>(trackClip.track)->GetRate());
         pWaveClip->MarkChanged();
      }
   }

   wxString msg;
   bool consolidate;
   if (mDidSlideVertically) {
      msg.Printf(_("Moved clips to another track"));
      consolidate = false;
   }
   else {
      wxString direction = mClipMoveState.hSlideAmount > 0 ?
         /* i18n-hint: a direction as in left or right.*/
         _("right") :
         /* i18n-hint: a direction as in left or right.*/
         _("left");
      /* i18n-hint: %s is a direction like left or right */
      msg.Printf(_("Time shifted tracks/clips %s %.02f seconds"),
         direction.c_str(), fabs( mClipMoveState.hSlideAmount ));
      consolidate = true;
   }
   pProject->PushState(msg, _("Time-Shift"),
      consolidate ? (UndoPush::CONSOLIDATE) : (UndoPush::AUTOSAVE));

   return result | FixScrollbars;
}

UIHandle::Result TimeShiftHandle::Cancel(AudacityProject *pProject)
{
   pProject->RollbackState();
   mCapturedTrack = nullptr;
   mSnapManager.reset();
   mClipMoveState.clear();
   return RefreshCode::RefreshAll;
}

void TimeShiftHandle::DrawExtras
(DrawingPass pass,
 wxDC * dc, const wxRegion &, const wxRect &)
{
   if (pass == Panel) {
      // Draw snap guidelines if we have any
      if ( mSnapManager )
         mSnapManager->Draw
            ( dc, mClipMoveState.snapLeft, mClipMoveState.snapRight );
   }
}
