/**********************************************************************

Audacity: A Digital Audio Editor

TimeShiftHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "TimeShiftHandle.h"

#include "TrackView.h"
#include "../../AColor.h"
#include "../../HitTestResult.h"
#include "../../ProjectAudioIO.h"
#include "../../ProjectHistory.h"
#include "../../ProjectSettings.h"
#include "../../RefreshCode.h"
#include "../../Snap.h"
#include "../../Track.h"
#include "../../TrackArtist.h"
#include "../../TrackPanelDrawingContext.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../UndoManager.h"
#include "../../ViewInfo.h"
#include "../../../images/Cursors.h"

TimeShiftHandle::TimeShiftHandle
( const std::shared_ptr<Track> &pTrack, bool gripHit )
   : mGripHit{ gripHit }
{
   mClipMoveState.mCapturedTrack = pTrack;
}

std::shared_ptr<Track> TimeShiftHandle::GetTrack() const
{
   return mClipMoveState.mCapturedTrack;
}

bool TimeShiftHandle::WasMoved() const
{
    return mDidSlideVertically || (mClipMoveState.initialized && mClipMoveState.wasMoved);
}

bool TimeShiftHandle::Clicked() const
{
   return mClipMoveState.initialized;
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

void ClipMoveState::DoHorizontalOffset( double offset )
{
   if ( !shifters.empty() ) {
      for ( auto &pair : shifters )
         pair.second->DoHorizontalOffset( offset );
   }
   else {
      for (auto channel : TrackList::Channels( mCapturedTrack.get() ))
         channel->Offset( offset );
   }
}

TrackShifter::TrackShifter() = default;

TrackShifter::~TrackShifter() = default;

void TrackShifter::UnfixIntervals(
   std::function< bool( const TrackInterval& ) > pred )
{
   for ( auto iter = mFixed.begin(); iter != mFixed.end(); ) {
      if ( pred( *iter) ) {
         mMoving.push_back( std::move( *iter ) );
         iter = mFixed.erase( iter );
         mAllFixed = false;
      }
      else
         ++iter;
   }
}

void TrackShifter::UnfixAll()
{
   std::move( mFixed.begin(), mFixed.end(), std::back_inserter(mMoving) );
   mFixed = Intervals{};
   mAllFixed = false;
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

double TrackShifter::QuantizeOffset(double desiredOffset)
{
   return desiredOffset;
}

double TrackShifter::AdjustOffsetSmaller(double desiredOffset)
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

bool TrackShifter::AdjustFit(
   const Track &, const Intervals&, double &, double)
{
   return false;
}

bool TrackShifter::Attach( Intervals )
{
   return true;
}

bool TrackShifter::FinishMigration()
{
   return true;
}

void TrackShifter::DoHorizontalOffset( double offset )
{
   if (!AllFixed())
      GetTrack().Offset( offset );
}

double TrackShifter::AdjustT0(double t0) const
{
   return t0;
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

auto CoarseTrackShifter::HitTest(
   double, const ViewInfo&, HitTestParams* ) -> HitTestResult
{
   return HitTestResult::Track;
}

bool CoarseTrackShifter::SyncLocks()
{
   return false;
}

template<> auto MakeTrackShifter::Implementation() -> Function {
   return [](Track &track, AudacityProject&) {
      return std::make_unique<CoarseTrackShifter>(track);
   };
}
static MakeTrackShifter registerMakeTrackShifter;

void ClipMoveState::Init(
   AudacityProject &project,
   Track &capturedTrack,
   TrackShifter::HitTestResult hitTestResult,
   std::unique_ptr<TrackShifter> pHit,
   double clickTime,
   const ViewInfo &viewInfo,
   TrackList &trackList, bool syncLocked )
{
   shifters.clear();

   initialized = true;

   auto &state = *this;
   state.mCapturedTrack = capturedTrack.SharedPointer();

   switch (hitTestResult) {
      case TrackShifter::HitTestResult::Miss:
         wxASSERT(false);
         pHit.reset();
         break;
      case TrackShifter::HitTestResult::Track:
         pHit.reset();
         break;
      case TrackShifter::HitTestResult::Intervals:
         break;
      case TrackShifter::HitTestResult::Selection:
         state.movingSelection = true;
         break;
      default:
         break;
   }

   if (!pHit)
      return;

   state.shifters[&capturedTrack] = std::move( pHit );

   // Collect TrackShifters for the rest of the tracks
   for ( auto track : trackList.Any() ) {
      auto &pShifter = state.shifters[track];
      if (!pShifter)
         pShifter = MakeTrackShifter::Call( *track, project );
   }

   if ( state.movingSelection ) {
      // All selected tracks may move some intervals
      const TrackInterval interval{
         viewInfo.selectedRegion.t0(),
         viewInfo.selectedRegion.t1()
      };
      for ( const auto &pair : state.shifters ) {
         auto &shifter = *pair.second;
         auto &track = shifter.GetTrack();
         if (&track == &capturedTrack)
            // Don't change the choice of intervals made by HitTest
            continue;
         if ( track.IsSelected() )
            shifter.SelectInterval( interval );
      }
   }
   else {
      // Move intervals only of the chosen channel group
      for ( auto channel : TrackList::Channels( &capturedTrack ) ) {
         auto &shifter = *state.shifters[channel];
         if ( channel != &capturedTrack )
            shifter.SelectInterval(TrackInterval{clickTime, clickTime});
      }
   }
   
   // Sync lock propagation of unfixing of intervals
   if ( syncLocked ) {
      bool change = true;
      while( change ) {
         change = false;

         // Iterate over all unfixed intervals in all tracks
         // that do propagation and are in sync lock groups ...
         for ( auto &pair : state.shifters ) {
            auto &shifter = *pair.second.get();
            if (!shifter.SyncLocks())
               continue;
            auto &track = shifter.GetTrack();
            auto group = TrackList::SyncLockGroup(&track);
            if ( group.size() <= 1 )
               continue;

            auto &intervals = shifter.MovingIntervals();
            for (auto &interval : intervals) {

               // ...and tell all other tracks in the sync lock group
               // to select that interval...
               for ( auto pTrack2 : group ) {
                  if (pTrack2 == &track)
                     continue;
                  
                  auto &shifter2 = *shifters[pTrack2];
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

double ClipMoveState::DoSlideHorizontal( double desiredSlideAmount )
{
   auto &state = *this;
   auto &capturedTrack = *state.mCapturedTrack;

   // Given a signed slide distance, move clips, but subject to constraint of
   // non-overlapping with other clips, so the distance may be adjusted toward
   // zero.
   if ( !state.shifters.empty() ) {
      double initialAllowed = 0;
      do { // loop to compute allowed, does not actually move anything yet
         initialAllowed = desiredSlideAmount;

         for (auto &pair : shifters) {
            auto newAmount = pair.second->AdjustOffsetSmaller( desiredSlideAmount );
            if ( desiredSlideAmount != newAmount ) {
               if ( newAmount * desiredSlideAmount < 0 ||
                    fabs(newAmount) > fabs(desiredSlideAmount) ) {
                  wxASSERT( false ); // AdjustOffsetSmaller didn't honor postcondition!
                  newAmount = 0; // Be sure the loop progresses to termination!
               }
               desiredSlideAmount = newAmount;
               state.snapLeft = state.snapRight = -1; // see bug 1067
            }
            if (newAmount == 0)
               break;
         }
      } while ( desiredSlideAmount != initialAllowed );
   }

   // Whether moving intervals or a whole track,
   // finally, here is where clips are moved
   if ( desiredSlideAmount != 0.0 )
      state.DoHorizontalOffset( desiredSlideAmount );

   //attempt to move a clip is counted to
   wasMoved = true;

   return (state.hSlideAmount = desiredSlideAmount);
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

   auto pShifter = MakeTrackShifter::Call( *pTrack, *pProject );

   auto hitTestResult = TrackShifter::HitTestResult::Track;
   if (!event.ShiftDown()) {
      TrackShifter::HitTestParams params{
         rect, event.m_x, event.m_y
      };
      hitTestResult = pShifter->HitTest( clickTime, viewInfo, &params );
      switch( hitTestResult ) {
      case TrackShifter::HitTestResult::Miss:
         return Cancelled;
      default:
         break;
      }
   }
   else {
      // just do shifting of one whole track
   }

   mClipMoveState.Init( *pProject, *pTrack,
      hitTestResult,
      std::move( pShifter ),
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
      Track &track )
   {
      auto &capturedTrack = *state.mCapturedTrack;
      if (slideUpDownOnly)
         return 0.0;
      else {
         double desiredSlideAmount =
            viewInfo.PositionToTime(event.m_x) -
            viewInfo.PositionToTime(state.mMouseClickX);
         double clipLeft = 0, clipRight = 0;

         if (!state.shifters.empty())
            desiredSlideAmount =
               state.shifters[ &track ]->QuantizeOffset( desiredSlideAmount );

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
      TrackList &trackList, Track &capturedTrack, Track &track,
      ClipMoveState &state)
   {
      if (state.shifters.empty())
         // Shift + Dragging hasn't yet supported vertical movement
         return false;

      // Accumulate new pairs for the correspondence, and merge them
      // into the given correspondence only on success
      Correspondence newPairs;

      auto sameType = [&]( auto pTrack ){
         return capturedTrack.SameKindAs( *pTrack );
      };
      if (!sameType(&track))
         return false;
   
      // All tracks of the same kind as the captured track
      auto range = trackList.Any() + sameType;

      // Find how far this track would shift down among those (signed)
      const auto myPosition =
         std::distance( range.first, trackList.Find( &capturedTrack ) );
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

            if ( correspondence.count(pTrack) )
               // Don't overwrite the given correspondence
               return false;

            newPairs[ pTrack ] = pOther;
         }

         if ( diff < 0 )
            ++diff; // Still consuming initial tracks
         else
            ++iter; // Safe to increment TrackIter even at end of range
      }

      // Success
      if (correspondence.empty())
         correspondence.swap(newPairs);
      else
         std::copy( newPairs.begin(), newPairs.end(),
            std::inserter( correspondence, correspondence.end() ) );
      return true;
   }

   using DetachedIntervals =
      std::unordered_map<Track*, TrackShifter::Intervals>;

   bool CheckFit(
      ClipMoveState &state, const Correspondence &correspondence,
      const DetachedIntervals &intervals,
      double tolerance, double &desiredSlideAmount )
   {
      bool ok = true;
      double firstTolerance = tolerance;
      
      // The desiredSlideAmount may change and the tolerance may get used up.
      for ( unsigned iPass = 0; iPass < 2 && ok; ++iPass ) {
         for ( auto &pair : state.shifters ) {
            auto *pSrcTrack = pair.first;
            auto iter = correspondence.find( pSrcTrack );
            if ( iter != correspondence.end() )
               if( auto *pOtherTrack = iter->second )
                  if ( !(ok = pair.second->AdjustFit(
                     *pOtherTrack, intervals.at(pSrcTrack),
                     desiredSlideAmount /*in,out*/, tolerance)) )
                     break;
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
      throw SimpleMessageBoxException{ ExceptionType::Internal,
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

      void Reinsert(
         std::unordered_map< Track*, Track* > *pCorrespondence )
      {
         for (auto &pair : detached) {
            auto pTrack = pair.first;
            if (pCorrespondence && pCorrespondence->count(pTrack))
               pTrack = (*pCorrespondence)[pTrack];
            auto &pShifter = state.shifters[pTrack];
            if (!pShifter->Attach( std::move( pair.second ) ))
               MigrationFailure();
         }
      }

      ClipMoveState &state;
      DetachedIntervals detached;
   };
}

bool TimeShiftHandle::DoSlideVertical
( ViewInfo &viewInfo, wxCoord xx,
  ClipMoveState &state, TrackList &trackList,
  Track &dstTrack, double &desiredSlideAmount )
{
   Correspondence correspondence;

   // See if captured track corresponds to another
   auto &capturedTrack = *state.mCapturedTrack;
   if (!FindCorrespondence(
      correspondence, trackList, capturedTrack, dstTrack, state ))
      return false;

   // Try to extend the correpondence
   auto tryExtend = [&](bool forward){
      auto begin = trackList.begin(), end = trackList.end();
      auto pCaptured = trackList.Find( &capturedTrack );
      auto pDst = trackList.Find( &dstTrack );
      // Scan for more correspondences
      while ( true ) {
         // Remember that TrackIter wraps circularly to the end iterator when
         // decrementing it

         // First move to a track with moving intervals and
         // without a correspondent
         do
            forward ? ++pCaptured : --pCaptured;
         while ( pCaptured != end &&
            ( correspondence.count(*pCaptured) || state.shifters[*pCaptured]->MovingIntervals().empty() ) );
         if ( pCaptured == end )
            break;

         // Change the choice of possible correspondent track too
         do
            forward ? ++pDst : --pDst;
         while ( pDst != end && correspondence.count(*pDst) );
         if ( pDst == end )
            break;

         // Make correspondence if we can
         if (!FindCorrespondence(
            correspondence, trackList, **pCaptured, **pDst, state ))
            break;
      }
   };
   // Try extension, backward first, then forward
   // (anticipating the case of dragging a label that is under a clip)
   tryExtend(false);
   tryExtend(true);

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
   bool ok = CheckFit( state, correspondence, remover.detached,
      tolerance, desiredSlideAmount /*in,out*/ );

   if (!ok) {
      // Failure, even with using tolerance.
      remover.Reinsert( nullptr );
      return false;
   }

   // Make the offset permanent; start from a "clean slate"
   state.mMouseClickX = xx;
   remover.Reinsert( &correspondence );
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
   mClipMoveState.DoHorizontalOffset( -mClipMoveState.hSlideAmount );

   if ( mClipMoveState.movingSelection ) {
      // Slide the selection, too
      viewInfo.selectedRegion.move( -mClipMoveState.hSlideAmount );
   }
   mClipMoveState.hSlideAmount = 0.0;

   double desiredSlideAmount =
      FindDesiredSlideAmount( viewInfo, mRect.x, event, mSnapManager.get(),
         mSlideUpDownOnly, mSnapPreferRightEdge, mClipMoveState,
         *pTrack );

   // Scroll during vertical drag.
   // If the mouse is over a track that isn't the captured track,
   // decide which tracks the captured clips should go to.
   // EnsureVisible(pTrack); //vvv Gale says this has problems on Linux, per bug 393 thread. Revert for 2.0.2.
   bool slidVertically = (
       pTrack != mClipMoveState.mCapturedTrack
       /* && !mCapturedClipIsSelection*/
      && DoSlideVertical( viewInfo, event.m_x, mClipMoveState,
                  trackList, *pTrack, desiredSlideAmount ) );
   if (slidVertically)
   {
      mClipMoveState.mCapturedTrack = pTrack;
      mDidSlideVertically = true;
   }
   
   if (desiredSlideAmount == 0.0)
      return RefreshAll;

   // Note that mouse dragging doesn't use TrackShifter::HintOffsetLarger()

   mClipMoveState.DoSlideHorizontal( desiredSlideAmount );

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
