/*!
 @file WaveTrackShifter.cpp
 @brief headerless file injects method definitions for time shifting of WaveTrack
 */

#include "../../../ui/TimeShiftHandle.h"
#include "ViewInfo.h"
#include "../../../../WaveClip.h"
#include "../../../../WaveTrack.h"

class WaveTrackShifter final : public TrackShifter {
public:
   WaveTrackShifter( WaveTrack &track )
      : mpTrack{ track.SharedPointer<WaveTrack>() }
   {
      InitIntervals();
   }
   ~WaveTrackShifter() override {}
   Track &GetTrack() const override { return *mpTrack; }

   HitTestResult HitTest(
      double time, const ViewInfo &viewInfo, HitTestParams* ) override
   {
      auto pClip = [&]() {
         auto ts = mpTrack->TimeToLongSamples(time);
         
         for (auto clip : mpTrack->GetClips())
         {
            const auto c0 = mpTrack->TimeToLongSamples(clip->GetPlayStartTime());
            const auto c1 = mpTrack->TimeToLongSamples(clip->GetPlayEndTime());
            if (ts >= c0 && ts < c1)
               return clip;
         }
         return std::shared_ptr<WaveClip>{};
      }();
      
      if (!pClip)
         return HitTestResult::Miss;

      auto t0 = viewInfo.selectedRegion.t0();
      auto t1 = viewInfo.selectedRegion.t1();
      if ( mpTrack->IsSelected() && time >= t0 && time < t1 ) {
         // Unfix maybe many intervals (at least one because of test above)
         SelectInterval({t0, t1});
         return HitTestResult::Selection;
      }

      // Select just one interval
      UnfixIntervals( [&](const auto &interval){
         return
            static_cast<WaveTrack::IntervalData*>(interval.Extra())
               ->GetClip() == pClip;
      } );
      
      return HitTestResult::Intervals;
   }

   void SelectInterval( const TrackInterval &interval ) override
   {
      UnfixIntervals( [&](auto &myInterval){
         // Use a slightly different test from CommonSelectInterval, rounding times
         // to exact samples according to the clip's rate
         auto data =
            static_cast<WaveTrack::IntervalData*>( myInterval.Extra() );
         auto clip = data->GetClip().get();
         const auto c0 = mpTrack->TimeToLongSamples(clip->GetPlayStartTime());
         const auto c1 = mpTrack->TimeToLongSamples(clip->GetPlayEndTime());
         return 
             mpTrack->TimeToLongSamples(interval.Start()) < c1 && 
             mpTrack->TimeToLongSamples(interval.End()) >= c0;
      });
   }

   bool SyncLocks() override { return true; }

   bool MayMigrateTo(Track &other) override
   {
      return TrackShifter::CommonMayMigrateTo(other);
   }

   double HintOffsetLarger(double desiredOffset) override
   {
      // set it to a sample point, and minimum of 1 sample point
      bool positive = (desiredOffset > 0);
      if (!positive)
         desiredOffset *= -1;
      double nSamples = rint(mpTrack->GetRate() * desiredOffset);
      nSamples = std::max(nSamples, 1.0);
      desiredOffset = nSamples / mpTrack->GetRate();
      if (!positive)
         desiredOffset *= -1;
      return desiredOffset;
   }
   
   double QuantizeOffset( double desiredOffset ) override
   {
      const auto rate = mpTrack->GetRate();
      // set it to a sample point
      return rint(desiredOffset * rate) / rate;
   }

   double AdjustOffsetSmaller(double desiredOffset) override
   {
      std::vector< WaveClip * > movingClips;
      for ( auto &interval : MovingIntervals() ) {
         auto data =
            static_cast<WaveTrack::IntervalData*>( interval.Extra() );
         movingClips.push_back(data->GetClip().get());
      }
      double newAmount = 0;
      (void) mpTrack->CanOffsetClips(movingClips, desiredOffset, &newAmount);
      return newAmount;
   }

   Intervals Detach() override
   {
      for ( auto &interval: mMoving ) {
         auto pData = static_cast<WaveTrack::IntervalData*>( interval.Extra() );
         auto pClip = pData->GetClip().get();
         // interval will still hold the clip, so ignore the return:
         (void) mpTrack->RemoveAndReturnClip(pClip);
         mMigrated.erase(pClip);
      }
      return std::move( mMoving );
   }

   bool AdjustFit(
      const Track &otherTrack, const Intervals &intervals,
      double &desiredOffset, double tolerance) override
   {
      bool ok = true;
      auto pOtherWaveTrack = static_cast<const WaveTrack*>(&otherTrack);
      for ( auto &interval: intervals ) {
         auto pData =
            static_cast<WaveTrack::IntervalData*>( interval.Extra() );
         auto pClip = pData->GetClip().get();
         ok = pOtherWaveTrack->CanInsertClip(
            pClip, desiredOffset, tolerance );
         if( !ok  )
            break;
      }
      return ok;
   }

   bool Attach( Intervals intervals ) override
   {
      for (auto &interval : intervals) {
         auto pData = static_cast<WaveTrack::IntervalData*>( interval.Extra() );
         auto pClip = pData->GetClip();
         if ( !mpTrack->AddClip( pClip ) )
            return false;
         mMigrated.insert( pClip.get() );
         mMoving.emplace_back( std::move( interval ) );
      }
      return true;
   }

   bool FinishMigration() override
   {
      auto rate = mpTrack->GetRate();
      for (auto pClip : mMigrated) {
         // Now that user has dropped the clip into a different track,
         // make sure the sample rate matches the destination track.
         pClip->Resample(rate);
         pClip->MarkChanged();
      }
      return true;
   }

   void DoHorizontalOffset( double offset ) override
   {
      for ( auto &interval : MovingIntervals() ) {
         auto data =
            static_cast<WaveTrack::IntervalData*>( interval.Extra() );
         data->GetClip()->Offset( offset );
      }
   }


   // Ensure that t0 is still within the clip which it was in before the move.
   // This corrects for any rounding errors.
   double AdjustT0( double t0 ) const override
   {
      if (MovingIntervals().empty())
         return t0;
      else {
         auto data = static_cast<WaveTrack::IntervalData*>(MovingIntervals()[0].Extra());
         auto& clip = data->GetClip();
         if (t0 < clip->GetPlayStartTime())
            t0 = clip->GetPlayStartTime();
         if (t0 > clip->GetPlayEndTime())
            t0 = clip->GetPlayEndTime();
      }
      return t0;
   }
   
private:
   std::shared_ptr<WaveTrack> mpTrack;

   // Clips that may require resampling
   std::unordered_set<WaveClip *> mMigrated;
};

using MakeWaveTrackShifter = MakeTrackShifter::Override<WaveTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(MakeWaveTrackShifter) {
   return [](WaveTrack &track, AudacityProject&) {
      return std::make_unique<WaveTrackShifter>(track);
   };
}
