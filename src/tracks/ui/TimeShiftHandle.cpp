/**********************************************************************

Audacity: A Digital Audio Editor

TimeShiftHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h" // for USE_* macros
#include "TimeShiftHandle.h"

#include "../../Experimental.h"

#include "TrackView.h"
#include "../../AColor.h"
#include "../../HitTestResult.h"
#include "../../NoteTrack.h"
#include "../../ProjectAudioIO.h"
#include "../../ProjectHistory.h"
#include "../../ProjectSettings.h"
#include "../../RefreshCode.h"
#include "../../Snap.h"
#include "../../TrackArtist.h"
#include "../../TrackPanelDrawingContext.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../UndoManager.h"
#include "../../WaveClip.h"
#include "../../ViewInfo.h"
#include "../../WaveTrack.h"
#include "../../../images/Cursors.h"

TrackClip::TrackClip(Track *t, WaveClip *c)
{
   track = origTrack = t;
   dstTrack = NULL;
   clip = c;
}

TrackClip::~TrackClip()
{

}

TimeShiftHandle::TimeShiftHandle
( const std::shared_ptr<Track> &pTrack, bool gripHit )
   : mGripHit{ gripHit }
{
   mClipMoveState.mCapturedTrack = pTrack;
}

void TimeShiftHandle::Enter(bool, AudacityProject *)
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
   auto message = XO("Click and drag to move a track in time");

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
      auto &clips = state.capturedClipArray;
      t->TypeSwitch(
         [&](WaveTrack *wt) {
            for(const auto &clip: wt->GetClips())
               if ( ! clip->IsClipStartAfterClip(t0) && ! clip->BeforeClip(t1) &&
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
   }

   // Helper for the above, adds a track's clips to capturedClipArray (eliminates
   // duplication of this logic)
   void AddClipsToCaptured
      ( ClipMoveState &state, const ViewInfo &viewInfo,
        Track *t )
   {
      AddClipsToCaptured( state, t, viewInfo.selectedRegion.t0(),
                         viewInfo.selectedRegion.t1() );
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

TrackShifter::~TrackShifter() = default;

void TrackShifter::UnfixIntervals(
   std::function< bool( const TrackInterval& ) > pred )
{
   for ( auto iter = mFixed.begin(); iter != mFixed.end(); ) {
      if ( pred( *iter) ) {
         mMoving.push_back( std::move( *iter ) );
         iter = mFixed.erase( iter );
      }
      else
         ++iter;
   }
}

void TrackShifter::UnfixAll()
{
   std::move( mFixed.begin(), mFixed.end(), std::back_inserter(mMoving) );
   mFixed = Intervals{};
}

void TrackShifter::SelectInterval( const TrackInterval & )
{
   UnfixAll();
}

void TrackShifter::CommonSelectInterval(const TrackInterval &interval)
{
   UnfixIntervals( [&](auto &myInterval){
      return !(interval.End() < myInterval.Start() ||
               myInterval.End() < interval.Start());
   });
}

double TrackShifter::HintOffsetLarger(double desiredOffset)
{
   return desiredOffset;
}

bool TrackShifter::MayMigrateTo(Track &)
{
   return false;
}

bool TrackShifter::CommonMayMigrateTo(Track &otherTrack)
{
   auto &track = GetTrack();

   // Both tracks need to be owned to decide this
   auto pMyList = track.GetOwner().get();
   auto pOtherList = otherTrack.GetOwner().get();
   if (pMyList && pOtherList) {

      // Can migrate to another track of the same kind...
      if ( otherTrack.SameKindAs( track ) ) {

         // ... with the same number of channels ...
         auto myChannels = TrackList::Channels( &track );
         auto otherChannels = TrackList::Channels( &otherTrack );
         if (myChannels.size() == otherChannels.size()) {
            
            // ... and where this track and the other have corresponding
            // positions
            return myChannels.size() == 1 ||
               std::distance(myChannels.first, pMyList->Find(&track)) ==
               std::distance(otherChannels.first, pOtherList->Find(&otherTrack));

         }

      }

   }
   return false;
}

auto TrackShifter::Detach() -> Intervals
{
   return {};
}

bool TrackShifter::Attach( Intervals )
{
   return true;
}

bool TrackShifter::FinishMigration()
{
   return true;
}

void TrackShifter::InitIntervals()
{
   mMoving.clear();
   mFixed = GetTrack().GetIntervals();
}

CoarseTrackShifter::CoarseTrackShifter( Track &track )
   : mpTrack{ track.SharedPointer() }
{
   InitIntervals();
}

CoarseTrackShifter::~CoarseTrackShifter() = default;

auto CoarseTrackShifter::HitTest( double ) -> HitTestResult
{
   return HitTestResult::Track;
}

bool CoarseTrackShifter::SyncLocks()
{
   return false;
}

template<> auto MakeTrackShifter::Implementation() -> Function {
   return [](Track &track) {
      return std::make_unique<CoarseTrackShifter>(track);
   };
}

void ClipMoveState::Init(
   Track &capturedTrack,
   std::unique_ptr<TrackShifter> pHit,
   double clickTime,
   const ViewInfo &viewInfo,
   TrackList &trackList, bool syncLocked )
{
   capturedClipArray.clear();
   shifters.clear();
   auto cleanup = finally([&]{
      // In transition, this class holds two representations of what to shift.
      // Be sure each is filled only if the other is.
      wxASSERT( capturedClipArray.empty() == shifters.empty() );
   });

   auto &state = *this;
   state.mCapturedTrack = capturedTrack.SharedPointer();

   state.movingSelection = capturedTrack.IsSelected() &&
      clickTime >= viewInfo.selectedRegion.t0() &&
      clickTime < viewInfo.selectedRegion.t1();

   if (!pHit)
      return;

   const bool capturedAClip =
      pHit && !pHit->MovingIntervals().empty();
   if ( capturedAClip ) {
      // There is still some code special to WaveTracks here that
      // needs to go elsewhere
      auto &interval = pHit->MovingIntervals()[0];
      auto pInfo =
         dynamic_cast<WaveTrack::IntervalData*>(interval.Extra());
      if ( pInfo )
         state.capturedClip = pInfo->GetClip().get();
   }

   state.shifters[&capturedTrack] = std::move( pHit );

   // Collect TrackShifters for the rest of the tracks
   for ( auto track : trackList.Any() ) {
      auto &pShifter = state.shifters[track];
      if (!pShifter)
         pShifter = MakeTrackShifter::Call( *track );
   }

// The captured clip is the focus, but we need to create a list
   // of all clips that have to move, also...

   // First, if click was in selection, capture selected clips; otherwise
   // just the clicked-on clip
   if ( state.movingSelection )
      // All selected tracks may move some intervals
      for (auto t : trackList.Selected())
         AddClipsToCaptured( state, viewInfo, t );
   else {
      // Move intervals only of the chosen channel group
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
      // Sync lock propagation of unfixing of intervals
      // AWD: capturedClipArray expands as the loop runs, so newly-added
      // clips are considered (the effect is like recursion and terminates
      // because AddClipsToCaptured doesn't add duplicate clips); to remove
      // this behavior just store the array size beforehand.
      for (unsigned int i = 0; i < state.capturedClipArray.size(); ++i) {
         auto trackClip = state.capturedClipArray[i];
         {
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

   // Analogy of the steps above, but with TrackShifters, follows below
   
   if ( state.movingSelection ) {
      // All selected tracks may move some intervals
      const TrackInterval interval{
         viewInfo.selectedRegion.t0(),
         viewInfo.selectedRegion.t1()
      };
      for ( const auto &pair : state.shifters ) {
         auto &shifter = *pair.second;
         auto &track = shifter.GetTrack();
         if ( track.IsSelected() )
            shifter.SelectInterval( interval );
      }
   }
   else {
      // Move intervals only of the chosen channel group
      for ( auto channel : TrackList::Channels( &capturedTrack ) ) {
         auto &shifter = *state.shifters[channel];
         if ( capturedAClip ) {
            if ( channel != &capturedTrack )
               shifter.SelectInterval(TrackInterval{clickTime, clickTime});
         }
         else
            shifter.UnfixAll();
      }
   }
   
   // Sync lock propagation of unfixing of intervals
   if ( syncLocked ) {
      bool change = true;
      while( change ) {
         change = false;

         // Iterate over all unfixed intervals in all shifters
         // that do propagation...
         for ( auto &pair : state.shifters ) {
            auto &shifter = *pair.second.get();
            if (!shifter.SyncLocks())
               continue;
            auto &track = shifter.GetTrack();
            auto &intervals = shifter.MovingIntervals();
            for (auto &interval : intervals) {

               // ...and tell all other tracks to select that interval...
               for ( auto &pair2 : state.shifters ) {
                  auto &shifter2 = *pair2.second.get();
                  if (&shifter2.GetTrack() == &track)
                     continue;
                  auto size = shifter2.MovingIntervals().size();
                  shifter2.SelectInterval( interval );
                  change = change ||
                     (shifter2.SyncLocks() &&
                      size != shifter2.MovingIntervals().size());
               }

            }
         }

         // ... and repeat if any other interval became unfixed in a
         // shifter that propagates
      }
   }
}

const TrackInterval *ClipMoveState::CapturedInterval() const
{
   auto pTrack = mCapturedTrack.get();
   if ( pTrack ) {
      auto iter = shifters.find( pTrack );
      if ( iter != shifters.end() ) {
         auto &pShifter = iter->second;
         if ( pShifter ) {
            auto &intervals = pShifter->MovingIntervals();
            if ( !intervals.empty() )
               return &intervals[0];
         }
      }
   }
   return nullptr;
}

double ClipMoveState::DoSlideHorizontal(
   double desiredSlideAmount, TrackList &trackList )
{
   auto &state = *this;
   auto &capturedTrack = *state.mCapturedTrack;
   state.hSlideAmount = desiredSlideAmount;

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

   return state.hSlideAmount;
}

namespace {
SnapPointArray FindCandidates(
   const TrackList &tracks, const ClipMoveState::ShifterMap &shifters )
{
   // Compare with the other function FindCandidates in Snap
   // Make the snap manager more selective than it would be if just constructed
   // from the track list
   SnapPointArray candidates;
   for ( const auto &pair : shifters ) {
      auto &shifter = pair.second;
      auto &track = shifter->GetTrack();
      for (const auto &interval : shifter->FixedIntervals() ) {
         candidates.emplace_back( interval.Start(), &track );
         if ( interval.Start() != interval.End() )
            candidates.emplace_back( interval.End(), &track );
      }
   }
   return candidates;
}
}

UIHandle::Result TimeShiftHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
   if ( unsafe )
      return Cancelled;

   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   auto &viewInfo = ViewInfo::Get( *pProject );

   const auto pView = std::static_pointer_cast<TrackView>(evt.pCell);
   const auto pTrack = pView ? pView->FindTrack().get() : nullptr;
   if (!pTrack)
      return RefreshCode::Cancelled;

   auto &trackList = TrackList::Get( *pProject );

   mClipMoveState.clear();
   mDidSlideVertically = false;

   const bool multiToolModeActive =
      (ToolCodes::multiTool == ProjectSettings::Get( *pProject ).GetTool());

   const double clickTime =
      viewInfo.PositionToTime(event.m_x, rect.x);

   mClipMoveState.capturedClip = NULL;
   mClipMoveState.capturedClipArray.clear();

   bool captureClips = false;

   auto pShifter = MakeTrackShifter::Call( *pTrack );

   if (!event.ShiftDown()) {
      switch( pShifter->HitTest( clickTime ) ) {
      case TrackShifter::HitTestResult::Miss:
         return Cancelled;
      case TrackShifter::HitTestResult::Intervals: {
         captureClips = true;
         break;
      }
      case TrackShifter::HitTestResult::Track:
      default:
         break;
      }
   }
   else {
      // As in the default above: just do shifting of one whole track
   }

   mClipMoveState.Init( *pTrack,
      captureClips ? std::move( pShifter ) : nullptr,
      clickTime,

      viewInfo, trackList,
      ProjectSettings::Get( *pProject ).IsSyncLocked() );

   mSlideUpDownOnly = event.CmdDown() && !multiToolModeActive;
   mRect = rect;
   mClipMoveState.mMouseClickX = event.m_x;
   mSnapManager =
   std::make_shared<SnapManager>(*trackList.GetOwner(),
       FindCandidates( trackList, mClipMoveState.shifters ),
       viewInfo,
       true, // don't snap to time
       kPixelTolerance);
   mClipMoveState.snapLeft = -1;
   mClipMoveState.snapRight = -1;
   auto pInterval = mClipMoveState.CapturedInterval();
   mSnapPreferRightEdge = pInterval &&
      (fabs(clickTime - pInterval->End()) <
       fabs(clickTime - pInterval->Start()));

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
            auto pInterval = state.CapturedInterval();
            if (pInterval) {
               clipLeft = pInterval->Start() + desiredSlideAmount;
               clipRight = pInterval->End() + desiredSlideAmount;
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

   using Correspondence = std::unordered_map< Track*, Track* >;

   bool FindCorrespondence(
      Correspondence &correspondence,
      TrackList &trackList, Track &track,
      ClipMoveState &state)
   {
      auto &capturedTrack = state.mCapturedTrack;
      auto sameType = [&]( auto pTrack ){
         return capturedTrack->SameKindAs( *pTrack );
      };
      if (!sameType(&track))
         return false;
   
      // All tracks of the same kind as the captured track
      auto range = trackList.Any() + sameType;

      // Find how far this track would shift down among those (signed)
      const auto myPosition =
         std::distance( range.first, trackList.Find( capturedTrack.get() ) );
      const auto otherPosition =
         std::distance( range.first, trackList.Find( &track ) );
      auto diff = otherPosition - myPosition;

      // Point to destination track
      auto iter = range.first.advance( diff > 0 ? diff : 0 );

      for (auto pTrack : range) {
         auto &pShifter = state.shifters[pTrack];
         if ( !pShifter->MovingIntervals().empty() ) {
            // One of the interesting tracks

            auto pOther = *iter;
            if ( diff < 0 || !pOther )
               // No corresponding track
               return false;

            if ( !pShifter->MayMigrateTo(*pOther) )
               // Rejected for other reason
               return false;

            correspondence[ pTrack ] = pOther;
         }

         if ( diff < 0 )
            ++diff; // Still consuming initial tracks
         else
            ++iter; // Safe to increment TrackIter even at end of range
      }

      // Record the correspondence in TrackClip
      for ( auto &trackClip: state.capturedClipArray )
         if ( trackClip.clip )
            trackClip.dstTrack =
               dynamic_cast<WaveTrack*>(correspondence[ trackClip.track ]);

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

   [[noreturn]] void MigrationFailure() {
      // Tracks may be in an inconsistent state; throw to the application
      // handler which restores consistency from undo history
      throw SimpleMessageBoxException{
         XO("Could not shift between tracks")};
   }

   struct TemporaryClipRemover {
      TemporaryClipRemover( ClipMoveState &clipMoveState )
         : state( clipMoveState )
      {
         // Pluck the moving clips out of their tracks
         for (auto &pair : state.shifters)
            detached[pair.first] = pair.second->Detach();
      }

      void Fail()
      {
         // Cause destructor to put all clips back where they came from
         for ( auto &trackClip : state.capturedClipArray )
            trackClip.dstTrack = static_cast<WaveTrack*>(trackClip.track);
         failed = true;
      }

      void Reinsert(
         std::unordered_map< Track*, Track* > &correspondence )
      {
         // Complete (or roll back) the vertical move.
         // Put moving clips into their destination tracks
         // which become the source tracks when we move again
         for ( auto &trackClip : state.capturedClipArray ) {
            WaveClip *const pSrcClip = trackClip.clip;
            if (pSrcClip) {
               const auto dstTrack = trackClip.dstTrack;
               trackClip.track = dstTrack;
            }
         }

         for (auto &pair : detached) {
            auto pTrack = pair.first;
            if (!failed && correspondence.count(pTrack))
               pTrack = correspondence[pTrack];
            auto &pShifter = state.shifters[pTrack];
            if (!pShifter->Attach( std::move( pair.second ) ))
               MigrationFailure();
         }
      }

      ClipMoveState &state;
      std::unordered_map<Track*, TrackShifter::Intervals> detached;
      bool failed = false;
   };
}

bool TimeShiftHandle::DoSlideVertical
( ViewInfo &viewInfo, wxCoord xx,
  ClipMoveState &state, TrackList &trackList,
  Track &dstTrack, double &desiredSlideAmount )
{
   Correspondence correspondence;
   if (!FindCorrespondence( correspondence, trackList, dstTrack, state ))
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
      remover.Reinsert( correspondence );
      return false;
   }

   // Make the offset permanent; start from a "clean slate"
   state.mMouseClickX = xx;
   remover.Reinsert( correspondence );
   return true;
}

UIHandle::Result TimeShiftHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
   if (unsafe) {
      this->Cancel(pProject);
      return RefreshAll | Cancelled;
   }

   const wxMouseEvent &event = evt.event;
   auto &viewInfo = ViewInfo::Get( *pProject );

   TrackView *trackView = dynamic_cast<TrackView*>(evt.pCell.get());
   Track *track = trackView ? trackView->FindTrack().get() : nullptr;

   // Uncommenting this permits drag to continue to work even over the controls area
   /*
   track = static_cast<CommonTrackPanelCell*>(evt.pCell)->FindTrack().get();
   */

   if (!track) {
      // Allow sliding if the pointer is not over any track, but only if x is
      // within the bounds of the tracks area.
      if (event.m_x >= mRect.GetX() &&
         event.m_x < mRect.GetX() + mRect.GetWidth())
          track = mClipMoveState.mCapturedTrack.get();
   }

   // May need a shared_ptr to reassign mCapturedTrack below
   auto pTrack = Track::SharedPointer( track );
   if (!pTrack)
      return RefreshCode::RefreshNone;


   auto &trackList = TrackList::Get( *pProject );

   // GM: slide now implementing snap-to
   // samples functionality based on sample rate.

   // Start by undoing the current slide amount; everything
   // happens relative to the original horizontal position of
   // each clip...
   DoOffset(
      mClipMoveState, mClipMoveState.mCapturedTrack.get(), -mClipMoveState.hSlideAmount );

   if ( mClipMoveState.movingSelection ) {
      // Slide the selection, too
      viewInfo.selectedRegion.move( -mClipMoveState.hSlideAmount );
   }
   mClipMoveState.hSlideAmount = 0.0;

   double desiredSlideAmount =
      FindDesiredSlideAmount( viewInfo, mRect.x, event, mSnapManager.get(),
         mSlideUpDownOnly, mSnapPreferRightEdge, mClipMoveState,
         *mClipMoveState.mCapturedTrack, *pTrack );

   // Scroll during vertical drag.
   // If the mouse is over a track that isn't the captured track,
   // decide which tracks the captured clips should go to.
   // EnsureVisible(pTrack); //vvv Gale says this has problems on Linux, per bug 393 thread. Revert for 2.0.2.
   bool slidVertically = (
      mClipMoveState.capturedClip &&
       pTrack != mClipMoveState.mCapturedTrack
       /* && !mCapturedClipIsSelection*/
      && pTrack->TypeSwitch<bool>( [&] (WaveTrack *) {
         if ( DoSlideVertical( viewInfo, event.m_x, mClipMoveState,
                  trackList, *pTrack, desiredSlideAmount ) ) {
            mClipMoveState.mCapturedTrack = pTrack;
            mDidSlideVertically = true;
            return true;
         }
         else
            return false;
     })
   );
   
   if (desiredSlideAmount == 0.0)
      return RefreshAll;

   // Note that mouse dragging doesn't use TrackShifter::HintOffsetLarger()

   mClipMoveState.DoSlideHorizontal( desiredSlideAmount, trackList );

   if (mClipMoveState.movingSelection) {
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
(const TrackPanelMouseState &, AudacityProject *pProject)
{
   // After all that, it still may be unsafe to drag.
   // Even if so, make an informative cursor change from default to "banned."
   const bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
   return HitPreview(pProject, unsafe);
}

UIHandle::Result TimeShiftHandle::Release
(const TrackPanelMouseEvent &, AudacityProject *pProject,
 wxWindow *)
{
   using namespace RefreshCode;
   const bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
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

   for ( auto &pair : mClipMoveState.shifters )
      if ( !pair.second->FinishMigration() )
         MigrationFailure();
   
   TranslatableString msg;
   bool consolidate;
   if (mDidSlideVertically) {
      msg = XO("Moved clips to another track");
      consolidate = false;
   }
   else {
      msg = ( mClipMoveState.hSlideAmount > 0
         ? XO("Time shifted tracks/clips right %.02f seconds")
         : XO("Time shifted tracks/clips left %.02f seconds")
      )
         .Format( fabs( mClipMoveState.hSlideAmount ) );
      consolidate = true;
   }
   ProjectHistory::Get( *pProject ).PushState(msg, XO("Time-Shift"),
      consolidate ? (UndoPush::CONSOLIDATE) : (UndoPush::NONE));

   return result | FixScrollbars;
}

UIHandle::Result TimeShiftHandle::Cancel(AudacityProject *pProject)
{
   ProjectHistory::Get( *pProject ).RollbackState();
   return RefreshCode::RefreshAll;
}

void TimeShiftHandle::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassSnapping ) {
      auto &dc = context.dc;
      // Draw snap guidelines if we have any
      if ( mSnapManager ) {
         mSnapManager->Draw(
            &dc, mClipMoveState.snapLeft, mClipMoveState.snapRight );
      }
   }
}

wxRect TimeShiftHandle::DrawingArea(
   TrackPanelDrawingContext &,
   const wxRect &rect, const wxRect &panelRect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassSnapping )
      return MaximizeHeight( rect, panelRect );
   else
      return rect;
}
