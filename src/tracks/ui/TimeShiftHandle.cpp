/**********************************************************************

Audacity: A Digital Audio Editor

TimeShiftHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h" // for USE_* macros
#include "TimeShiftHandle.h"

#include "../../Experimental.h"

#include "TrackControls.h"
#include "../../AColor.h"
#include "../../HitTestResult.h"
#include "../../NoteTrack.h"
#include "../../Project.h"
#include "../../RefreshCode.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../toolbars/ToolsToolBar.h"
#include "../../UndoManager.h"
#include "../../WaveClip.h"
#include "../../WaveTrack.h"
#include "../../../images/Cursors.h"

TimeShiftHandle::TimeShiftHandle
( const std::shared_ptr<Track> &pTrack, bool gripHit )
   : mCapturedTrack{ pTrack }
   , mGripHit{ gripHit }
{
}

void TimeShiftHandle::Enter(bool)
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

HitTestPreview TimeShiftHandle::HitPreview
(const AudacityProject *WXUNUSED(pProject), bool unsafe)
{
   static auto disabledCursor =
      ::MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
   static auto slideCursor =
      MakeCursor(wxCURSOR_SIZEWE, TimeCursorXpm, 16, 16);
   // TODO: Should it say "track or clip" ?  Non-wave tracks can move, or clips in a wave track.
   // TODO: mention effects of shift (move all clips of selected wave track) and ctrl (move vertically only) ?
   //  -- but not all of that is available in multi tool.
   auto message = _("Click and drag to move a track in time");

   return {
      message,
      (unsafe
       ? &*disabledCursor
       : &*slideCursor)
   };
}

UIHandlePtr TimeShiftHandle::HitAnywhere
(std::weak_ptr<TimeShiftHandle> &holder,
 const std::shared_ptr<Track> &pTrack, bool gripHit)
{
   auto result = std::make_shared<TimeShiftHandle>( pTrack, gripHit );
   result = AssignUIHandlePtr(holder, result);
   return result;
}

UIHandlePtr TimeShiftHandle::HitTest
(std::weak_ptr<TimeShiftHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const std::shared_ptr<Track> &pTrack)
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
   if (!(state.m_x + hotspotOffset < rect.x + adjustedDragHandleWidth ||
       state.m_x + hotspotOffset >= rect.x + rect.width - adjustedDragHandleWidth))
      return {};

   return HitAnywhere( holder, pTrack, true );
}

TimeShiftHandle::~TimeShiftHandle()
{
}

namespace
{
   // Adds a track's clips to state.capturedClipArray within a specified time
   void AddClipsToCaptured
      ( ClipMoveState &state, Track *t, double t0, double t1 )
   {
      bool exclude = true;
      auto &clips = state.capturedClipArray;
      t->TypeSwitch(
         [&](WaveTrack *wt) {
            exclude = false;
            for(const auto &clip: wt->GetClips())
               if ( ! clip->AfterClip(t0) && ! clip->BeforeClip(t1) &&
                  // Avoid getting clips that were already captured
                    ! std::any_of( clips.begin(), clips.end(),
                       [&](const TrackClip &c) { return c.clip == clip.get(); } ) )
                  clips.emplace_back( t, clip.get() );
         },
#ifdef USE_MIDI
         [&](NoteTrack *, const Track::Fallthrough &fallthrough){
            // do not add NoteTrack if the data is outside of time bounds
            if (t->GetEndTime() < t0 || t->GetStartTime() > t1)
               return;
            else
               fallthrough();
         },
#endif
         [&](Track *t) {
            // This handles label tracks rather heavy-handedly --
            // it would be nice to
            // treat individual labels like clips

            // Avoid adding a track twice
            if( !std::any_of( clips.begin(), clips.end(),
               [&](const TrackClip &c) { return c.track == t; } ) ) {
               clips.emplace_back( t, nullptr );
            }
         }
      );
      if (exclude)
         state.trackExclusions.push_back(t);
   }

   // Helper for the above, adds a track's clips to capturedClipArray (eliminates
   // duplication of this logic)
   void AddClipsToCaptured
      ( ClipMoveState &state, const ViewInfo &viewInfo,
        Track *t, bool withinSelection )
   {
      if (withinSelection)
         AddClipsToCaptured( state, t, viewInfo.selectedRegion.t0(),
                            viewInfo.selectedRegion.t1() );
      else
         AddClipsToCaptured( state, t, t->GetStartTime(), t->GetEndTime() );
   }

   WaveTrack *NthChannel(WaveTrack &leader, int nn)
   {
      if (nn < 0)
         return nullptr;
      return *TrackList::Channels( &leader ).begin().advance(nn);
   }

   int ChannelPosition(const Track *pChannel)
   {
      return static_cast<int>(
         TrackList::Channels( pChannel )
            .EndingAfter( pChannel ).size()
      ) - 1;
   }

   // Don't count right channels.
   WaveTrack *NthAudioTrack(TrackList &list, int nn)
   {
      if (nn >= 0) {
         for ( auto pTrack : list.Leaders< WaveTrack >() )
            if (nn -- == 0)
               return pTrack;
      }

      return NULL;
   }

   // Don't count right channels.
   int TrackPosition(TrackList &list, const Track *pFindTrack)
   {
      pFindTrack = *list.FindLeader(pFindTrack);
      int nn = 0;
      for ( auto pTrack : list.Leaders< const WaveTrack >() ) {
         if (pTrack == pFindTrack)
            return nn;
         ++nn;
      }
      return -1;
   }

   WaveClip *FindClipAtTime(WaveTrack *pTrack, double time)
   {
      if (pTrack) {
         // WaveClip::GetClipAtX doesn't work unless the clip is on the screen and can return bad info otherwise
         // instead calculate the time manually
         double rate = pTrack->GetRate();
         auto s0 = (sampleCount)(time * rate + 0.5);

         if (s0 >= 0)
            return pTrack->GetClipAtSample(s0);
      }

      return 0;
   }

   void DoOffset( ClipMoveState &state, Track *pTrack, double offset,
                  WaveClip *pExcludedClip = nullptr )
   {
      auto &clips = state.capturedClipArray;
      if ( !clips.empty() ) {
         for (auto &clip : clips) {
            if (clip.clip) {
               if (clip.clip != pExcludedClip)
                  clip.clip->Offset( offset );
            }
            else
               clip.track->Offset( offset );
         }
      }
      else if ( pTrack )
         // Was a shift-click
         for (auto channel : TrackList::Channels( pTrack ))
            channel->Offset( offset );
   }
}

void TimeShiftHandle::CreateListOfCapturedClips
   ( ClipMoveState &state, const ViewInfo &viewInfo, Track &capturedTrack,
     TrackList &trackList, bool syncLocked, double clickTime )
{
// The captured clip is the focus, but we need to create a list
   // of all clips that have to move, also...

   state.capturedClipArray.clear();

   // First, if click was in selection, capture selected clips; otherwise
   // just the clicked-on clip
   if ( state.capturedClipIsSelection )
      for (auto t : trackList.Selected())
         AddClipsToCaptured( state, viewInfo, t, true );
   else {
      state.capturedClipArray.push_back
         (TrackClip( &capturedTrack, state.capturedClip ));

      if (state.capturedClip) {
         // Check for other channels
         auto wt = static_cast<WaveTrack*>(&capturedTrack);
         for ( auto channel : TrackList::Channels( wt ).Excluding( wt ) )
            if (WaveClip *const clip = FindClipAtTime(channel, clickTime))
               state.capturedClipArray.push_back(TrackClip(channel, clip));
      }
   }

   // Now, if sync-lock is enabled, capture any clip that's linked to a
   // captured clip.
   if ( syncLocked ) {
      // AWD: capturedClipArray expands as the loop runs, so newly-added
      // clips are considered (the effect is like recursion and terminates
      // because AddClipsToCaptured doesn't add duplicate clips); to remove
      // this behavior just store the array size beforehand.
      for (unsigned int i = 0; i < state.capturedClipArray.size(); ++i) {
        {
            auto &trackClip = state.capturedClipArray[i];
            // Capture based on tracks that have clips -- that means we
            // don't capture based on links to label tracks for now (until
            // we can treat individual labels as clips)
            if ( trackClip.clip ) {
               // Iterate over sync-lock group tracks.
               for (auto t : TrackList::SyncLockGroup( trackClip.track ))
                  AddClipsToCaptured(state, t,
                        trackClip.clip->GetStartTime(),
                        trackClip.clip->GetEndTime() );
            }
         }
#ifdef USE_MIDI
         {
            // Beware relocation of array contents!  Bind trackClip again.
            auto &trackClip = state.capturedClipArray[i];
            // Capture additional clips from NoteTracks
            trackClip.track->TypeSwitch( [&](NoteTrack *nt) {
               // Iterate over sync-lock group tracks.
               for (auto t : TrackList::SyncLockGroup(nt))
                  AddClipsToCaptured
                     ( state, t, nt->GetStartTime(), nt->GetEndTime() );
            });
         }
#endif
      }
   }
}

void TimeShiftHandle::DoSlideHorizontal
   ( ClipMoveState &state, TrackList &trackList, Track &capturedTrack )
{
   // Given a signed slide distance, move clips, but subject to constraint of
   // non-overlapping with other clips, so the distance may be adjusted toward
   // zero.
   if ( state.capturedClipArray.size() )
   {
      double allowed;
      double initialAllowed;
      double safeBigDistance = 1000 + 2.0 * ( trackList.GetEndTime() -
                                              trackList.GetStartTime() );

      do { // loop to compute allowed, does not actually move anything yet
         initialAllowed = state.hSlideAmount;

         for ( auto &trackClip : state.capturedClipArray ) {
            if (const auto clip = trackClip.clip) {
               // only audio clips are used to compute allowed
               const auto track = static_cast<WaveTrack *>( trackClip.track );

               // Move all other selected clips totally out of the way
               // temporarily because they're all moving together and
               // we want to find out if OTHER clips are in the way,
               // not one of the moving ones
               DoOffset( state, nullptr, -safeBigDistance, clip );
               auto cleanup = finally( [&]
                  { DoOffset( state, nullptr, safeBigDistance, clip ); } );

               if ( track->CanOffsetClip(clip, state.hSlideAmount, &allowed) ) {
                  if ( state.hSlideAmount != allowed ) {
                     state.hSlideAmount = allowed;
                     state.snapLeft = state.snapRight = -1; // see bug 1067
                  }
               }
               else {
                  state.hSlideAmount = 0.0;
                  state.snapLeft = state.snapRight = -1; // see bug 1067
               }
            }
         }
      } while ( state.hSlideAmount != initialAllowed );

      // finally, here is where clips are moved
      if ( state.hSlideAmount != 0.0 )
         DoOffset( state, nullptr, state.hSlideAmount );
   }
   else
      // For Shift key down, or
      // For non wavetracks, specifically label tracks ...
      DoOffset( state, &capturedTrack, state.hSlideAmount );
}

UIHandle::Result TimeShiftHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const bool unsafe = pProject->IsAudioActive();
   if ( unsafe )
      return Cancelled;

   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   const ViewInfo &viewInfo = pProject->GetViewInfo();

   const auto pTrack = std::static_pointer_cast<Track>(evt.pCell);
   if (!pTrack)
      return RefreshCode::Cancelled;

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

   mClipMoveState.capturedClip = NULL;
   mClipMoveState.capturedClipArray.clear();

   bool ok = true;
   bool captureClips = false;

   if (!event.ShiftDown())
      pTrack->TypeSwitch(
         [&](WaveTrack *wt) {
            if (nullptr ==
               (mClipMoveState.capturedClip = wt->GetClipAtX(event.m_x)))
               ok = false;
            else
               captureClips = true;
         },
#ifdef USE_MIDI
         [&](NoteTrack *) {
            captureClips = true;
         }
#endif
      );

   if ( ! ok )
      return Cancelled;
   else if ( captureClips )
      CreateListOfCapturedClips
         ( mClipMoveState, viewInfo, *pTrack, *trackList,
           pProject->IsSyncLocked(), clickTime );

   mSlideUpDownOnly = event.CmdDown() && !multiToolModeActive;
   mRect = rect;
   mClipMoveState.mMouseClickX = event.m_x;
   mSnapManager = std::make_shared<SnapManager>(trackList,
                                  &viewInfo,
                                  &mClipMoveState.capturedClipArray,
                                  &mClipMoveState.trackExclusions,
                                  true); // don't snap to time
   mClipMoveState.snapLeft = -1;
   mClipMoveState.snapRight = -1;
   mSnapPreferRightEdge =
      mClipMoveState.capturedClip &&
      (fabs(clickTime - mClipMoveState.capturedClip->GetEndTime()) <
       fabs(clickTime - mClipMoveState.capturedClip->GetStartTime()));

   return RefreshNone;
}

namespace {
   double FindDesiredSlideAmount(
      const ViewInfo &viewInfo, wxCoord xx, const wxMouseEvent &event,
      SnapManager *pSnapManager,
      bool slideUpDownOnly, bool snapPreferRightEdge,
      ClipMoveState &state,
      Track &capturedTrack, Track &track )
   {
      if (slideUpDownOnly)
         return 0.0;
      else {
         double desiredSlideAmount =
            viewInfo.PositionToTime(event.m_x) -
            viewInfo.PositionToTime(state.mMouseClickX);
         double clipLeft = 0, clipRight = 0;

         track.TypeSwitch( [&](WaveTrack *mtw){
            const double rate = mtw->GetRate();
            // set it to a sample point
            desiredSlideAmount = rint(desiredSlideAmount * rate) / rate;
         });

         // Adjust desiredSlideAmount using SnapManager
         if (pSnapManager) {
            if (state.capturedClip) {
               clipLeft = state.capturedClip->GetStartTime()
                  + desiredSlideAmount;
               clipRight = state.capturedClip->GetEndTime()
                  + desiredSlideAmount;
            }
            else {
               clipLeft = capturedTrack.GetStartTime() + desiredSlideAmount;
               clipRight = capturedTrack.GetEndTime() + desiredSlideAmount;
            }

            auto results =
               pSnapManager->Snap(&capturedTrack, clipLeft, false);
            auto newClipLeft = results.outTime;
            results =
               pSnapManager->Snap(&capturedTrack, clipRight, false);
            auto newClipRight = results.outTime;

            // Only one of them is allowed to snap
            if (newClipLeft != clipLeft && newClipRight != clipRight) {
               // Un-snap the un-preferred edge
               if (snapPreferRightEdge)
                  newClipLeft = clipLeft;
               else
                  newClipRight = clipRight;
            }

            // Take whichever one snapped (if any) and compute the NEW desiredSlideAmount
            state.snapLeft = -1;
            state.snapRight = -1;
            if (newClipLeft != clipLeft) {
               const double difference = (newClipLeft - clipLeft);
               desiredSlideAmount += difference;
               state.snapLeft =
                  viewInfo.TimeToPosition(newClipLeft, xx);
            }
            else if (newClipRight != clipRight) {
               const double difference = (newClipRight - clipRight);
               desiredSlideAmount += difference;
               state.snapRight =
                  viewInfo.TimeToPosition(newClipRight, xx);
            }
         }
         return desiredSlideAmount;
      }
   }

   bool FindCorrespondence(
      TrackList &trackList, Track &track, Track &capturedTrack,
      ClipMoveState &state)
   {
      const int diff =
         TrackPosition(trackList, &track) -
         TrackPosition(trackList, &capturedTrack);
      for ( auto &trackClip : state.capturedClipArray ) {
         if (trackClip.clip) {
            // Move all clips up or down by an equal count of audio tracks.
            // Can only move between tracks with equal numbers of channels,
            // and among corresponding channels.

            Track *const pSrcTrack = trackClip.track;
            auto pDstTrack = NthAudioTrack(trackList,
               diff + TrackPosition(trackList, pSrcTrack));
            if (!pDstTrack)
               return false;

            if (TrackList::Channels(pSrcTrack).size() !=
                TrackList::Channels(pDstTrack).size())
               return false;

            auto pDstChannel = NthChannel(
               *pDstTrack, ChannelPosition(pSrcTrack));

            if (!pDstChannel) {
               wxASSERT(false);
               return false;
            }

           trackClip.dstTrack = pDstChannel;
         }
      }
      return true;
   }

   bool CheckFit(
      const ViewInfo &viewInfo, wxCoord xx, ClipMoveState &state,
      double tolerance, double &desiredSlideAmount )
   {
      (void)xx;// Compiler food
      (void)viewInfo;// Compiler food
      bool ok = true;
      double firstTolerance = tolerance;

      // The desiredSlideAmount may change and the tolerance may get used up.
      for ( unsigned iPass = 0; iPass < 2 && ok; ++iPass ) {
         for ( auto &trackClip : state.capturedClipArray ) {
            WaveClip *const pSrcClip = trackClip.clip;
            if (pSrcClip) {
               ok = trackClip.dstTrack->CanInsertClip(
                  pSrcClip, desiredSlideAmount, tolerance );
               if( !ok  )
                  break;
            }
         }
         // If it fits ok, desiredSlideAmount could have been updated to get
         // the clip to fit.
         // Check again, in the new position, this time with zero tolerance.
         if (firstTolerance == 0)
            break;
         else
            tolerance = 0.0;
      }

      return ok;
   }

   struct TemporaryClipRemover {
      TemporaryClipRemover( ClipMoveState &clipMoveState )
         : state( clipMoveState )
      {
         // Pluck the moving clips out of their tracks
         for ( auto &trackClip : state.capturedClipArray ) {
            WaveClip *const pSrcClip = trackClip.clip;
            if (pSrcClip)
               trackClip.holder =
                  // Assume track is wave because it has a clip
                  static_cast<WaveTrack*>(trackClip.track)->
                     RemoveAndReturnClip(pSrcClip);
         }
      }

      void Fail()
      {
         // Cause destructor to put all clips back where they came from
         for ( auto &trackClip : state.capturedClipArray )
            trackClip.dstTrack = static_cast<WaveTrack*>(trackClip.track);
      }

      ~TemporaryClipRemover()
      {
         // Complete (or roll back) the vertical move.
         // Put moving clips into their destination tracks
         // which become the source tracks when we move again
         for ( auto &trackClip : state.capturedClipArray ) {
            WaveClip *const pSrcClip = trackClip.clip;
            if (pSrcClip) {
               const auto dstTrack = trackClip.dstTrack;
               dstTrack->AddClip(std::move(trackClip.holder));
               trackClip.track = dstTrack;
            }
         }
      }

      ClipMoveState &state;
   };
}

bool TimeShiftHandle::DoSlideVertical
( ViewInfo &viewInfo, wxCoord xx,
  ClipMoveState &state, TrackList &trackList, Track &capturedTrack,
  Track &dstTrack, double &desiredSlideAmount )
{
   if (!FindCorrespondence( trackList, dstTrack, capturedTrack, state))
      return false;

   // Having passed that test, remove clips temporarily from their
   // tracks, so moving clips don't interfere with each other
   // when we call CanInsertClip()
   TemporaryClipRemover remover{ state };

   // Now check that the move is possible
   double slide = desiredSlideAmount; // remember amount requested.
   // The test for tolerance will need review with FishEye!
   // The tolerance is supposed to be the time for one pixel,
   // i.e. one pixel tolerance at current zoom.
   double tolerance =
      viewInfo.PositionToTime(xx + 1) - viewInfo.PositionToTime(xx);
   bool ok = CheckFit( viewInfo, xx, state, tolerance, desiredSlideAmount );

   if (!ok) {
      // Failure, even with using tolerance.
      remover.Fail();

      // Failure -- we'll put clips back where they were
      // ok will next indicate if a horizontal slide is OK.
      tolerance = 0.0;
      desiredSlideAmount = slide;
      ok = CheckFit( viewInfo, xx, state, tolerance, desiredSlideAmount );
      for ( auto &trackClip : state.capturedClipArray)  {
         WaveClip *const pSrcClip = trackClip.clip;
         if (pSrcClip){
            
            // Attempt to move to a new track did not work.
            // Put the clip back appropriately shifted!
            if( ok)
               trackClip.holder->Offset(slide);
         }
      }
      // Make the offset permanent; start from a "clean slate"
      if( ok ) {
         state.mMouseClickX = xx;
         if (state.capturedClipIsSelection) {
            // Slide the selection, too
            viewInfo.selectedRegion.move( slide );
         }
         state.hSlideAmount = 0;
      }

      return false;
   }

   // Make the offset permanent; start from a "clean slate"
   state.mMouseClickX = xx;
   return true;
}

UIHandle::Result TimeShiftHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const bool unsafe = pProject->IsAudioActive();
   if (unsafe) {
      this->Cancel(pProject);
      return RefreshAll | Cancelled;
   }

   const wxMouseEvent &event = evt.event;
   ViewInfo &viewInfo = pProject->GetViewInfo();

   Track *track = dynamic_cast<Track*>(evt.pCell.get());

   // Uncommenting this permits drag to continue to work even over the controls area
   /*
   track = static_cast<CommonTrackPanelCell*>(evt.pCell)->FindTrack().get();
   */

   if (!track) {
      // Allow sliding if the pointer is not over any track, but only if x is
      // within the bounds of the tracks area.
      if (event.m_x >= mRect.GetX() &&
         event.m_x < mRect.GetX() + mRect.GetWidth())
          track = mCapturedTrack.get();
   }

   // May need a shared_ptr to reassign mCapturedTrack below
   auto pTrack = Track::SharedPointer( track );
   if (!pTrack)
      return RefreshCode::RefreshNone;


   TrackList *const trackList = pProject->GetTracks();

   // GM: DoSlide now implementing snap-to
   // samples functionality based on sample rate.

   // Start by undoing the current slide amount; everything
   // happens relative to the original horizontal position of
   // each clip...
   DoOffset(
      mClipMoveState, mCapturedTrack.get(), -mClipMoveState.hSlideAmount );

   if ( mClipMoveState.capturedClipIsSelection ) {
      // Slide the selection, too
      viewInfo.selectedRegion.move( -mClipMoveState.hSlideAmount );
   }
   mClipMoveState.hSlideAmount = 0.0;

   double desiredSlideAmount =
      FindDesiredSlideAmount( viewInfo, mRect.x, event, mSnapManager.get(),
         mSlideUpDownOnly, mSnapPreferRightEdge, mClipMoveState,
         *mCapturedTrack, *pTrack );

   // Scroll during vertical drag.
   // EnsureVisible(pTrack); //vvv Gale says this has problems on Linux, per bug 393 thread. Revert for 2.0.2.
   bool slidVertically = false;

   // If the mouse is over a track that isn't the captured track,
   // decide which tracks the captured clips should go to.
   bool fail = (
      mClipMoveState.capturedClip &&
       pTrack != mCapturedTrack
       /* && !mCapturedClipIsSelection*/
      && pTrack->TypeSwitch<bool>( [&] (WaveTrack *) {
            if ( DoSlideVertical( viewInfo, event.m_x, mClipMoveState,
                     *trackList, *mCapturedTrack, *pTrack, desiredSlideAmount ) ) {
               mCapturedTrack = pTrack;
               mDidSlideVertically = true;
            }
            else
               return true;

            // Not done yet, check for horizontal movement.
            slidVertically = true;
            return false;
        })
   );
   
   if (fail)
      return RefreshAll;

   if (desiredSlideAmount == 0.0)
      return RefreshAll;

   mClipMoveState.hSlideAmount = desiredSlideAmount;

   DoSlideHorizontal( mClipMoveState, *trackList, *mCapturedTrack );

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
(const TrackPanelMouseState &, const AudacityProject *pProject)
{
   // After all that, it still may be unsafe to drag.
   // Even if so, make an informative cursor change from default to "banned."
   const bool unsafe = pProject->IsAudioActive();
   return HitPreview(pProject, unsafe);
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

   // Do not draw yellow lines
   if ( mClipMoveState.snapLeft != -1 || mClipMoveState.snapRight != -1) {
      mClipMoveState.snapLeft = mClipMoveState.snapRight = -1;
      result |= RefreshAll;
   }

   if ( !mDidSlideVertically && mClipMoveState.hSlideAmount == 0 )
      return result;
   
   for ( auto &trackClip : mClipMoveState.capturedClipArray )
   {
      WaveClip* pWaveClip = trackClip.clip;
      // Note that per AddClipsToCaptured(Track *t, double t0, double t1),
      // in the non-WaveTrack case, the code adds a NULL clip to capturedClipArray,
      // so we have to check for that any time we're going to deref it.
      // Previous code did not check it here, and that caused bug 367 crash.
      if (pWaveClip &&
         trackClip.track != trackClip.origTrack)
      {
         // Now that user has dropped the clip into a different track,
         // make sure the sample rate matches the destination track.
         // Assume the clip was dropped in a wave track
         pWaveClip->Resample
            (static_cast<WaveTrack*>(trackClip.track)->GetRate());
         pWaveClip->MarkChanged();
      }
   }

   wxString msg;
   bool consolidate;
   if (mDidSlideVertically) {
      msg = _("Moved clips to another track");
      consolidate = false;
   }
   else {
      msg.Printf(
         ( mClipMoveState.hSlideAmount > 0
           ? _("Time shifted tracks/clips right %.02f seconds")
           : _("Time shifted tracks/clips left %.02f seconds") ),
         fabs( mClipMoveState.hSlideAmount ) );
      consolidate = true;
   }
   pProject->PushState(msg, _("Time-Shift"),
      consolidate ? (UndoPush::CONSOLIDATE) : (UndoPush::AUTOSAVE));

   return result | FixScrollbars;
}

UIHandle::Result TimeShiftHandle::Cancel(AudacityProject *pProject)
{
   pProject->RollbackState();
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
