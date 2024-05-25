/**********************************************************************

 Audacity: A Digital Audio Editor

 @file DefaultPlaybackPolicy.cpp

 Paul Licameli split from PlaybackSchedule.cpp

 **********************************************************************/

#include "DefaultPlaybackPolicy.h"
#include "ProjectAudioIO.h"
#include "SampleCount.h"
#include "ViewInfo.h"

DefaultPlaybackPolicy::DefaultPlaybackPolicy( AudacityProject &project,
   double trackEndTime, double loopEndTime, std::optional<double> pStartTime,
   bool loopEnabled, bool variableSpeed )
   : mProject{ project }
   , mTrackEndTime{ trackEndTime }
   , mLoopEndTime{ loopEndTime }
   , mpStartTime{ pStartTime }
   , mLoopEnabled{ loopEnabled }
   , mVariableSpeed{ variableSpeed }
{}

DefaultPlaybackPolicy::~DefaultPlaybackPolicy() = default;

void DefaultPlaybackPolicy::Initialize(const PlaybackSchedule &schedule,
   PlaybackState &state, double rate)
{
   PlaybackPolicy::Initialize(schedule, state, rate);
   mLastPlaySpeed = GetPlaySpeed();
   mMessageChannel.Write( { mLastPlaySpeed,
      state.mT0, mLoopEndTime, mLoopEnabled } );

   auto callback = [this](auto&){ WriteMessage(); };
   mRegionSubscription =
       ViewInfo::Get(mProject).playRegion.Subscribe(callback);
   if (mVariableSpeed)
      mSpeedSubscription = ProjectAudioIO::Get(mProject).Subscribe(callback);
}

Mixer::WarpOptions
DefaultPlaybackPolicy::MixerWarpOptions(const PlaybackSchedule &schedule)
{
   if (mVariableSpeed)
      // Enable variable rate mixing
      return Mixer::WarpOptions(0.01, 32.0, GetPlaySpeed());
   else
      return PlaybackPolicy::MixerWarpOptions(schedule);
}

PlaybackPolicy::BufferTimes
DefaultPlaybackPolicy::SuggestedBufferTimes(const PlaybackSchedule &)
{
   // Shorter times than in the default policy so that responses to changes of
   // loop region or speed slider don't lag too much
   using namespace std::chrono;
   return { 0.05s, 0.05s, 0.25s };
}

bool DefaultPlaybackPolicy::RevertToOldDefault(const PlaybackSchedule &schedule) const
{
   return !mLoopEnabled ||
      // Even if loop is enabled, ignore it if right of looping region
      schedule.mTimeQueue.GetLastTime() > mLoopEndTime;
}

double DefaultPlaybackPolicy::OffsetSequenceTime(
   const PlaybackSchedule& schedule, PlaybackState &state, double offset)
{
   auto time = schedule.GetSequenceTime();

   // Assuming that mpStartTime always has a value when this policy is used
   if (mpStartTime) {
      if (mLoopEnabled) {
         if (time < state.mT0)
            time = std::clamp(time + offset, *mpStartTime, state.mT1);
         else
            time = std::clamp(time + offset, state.mT0, state.mT1);
      }
      else {
         // this includes the case where the start time is after the
         // looped region, and mLoopEnabled is set to false
         time = std::clamp(time + offset, *mpStartTime, state.mT1);
      }
   }

   state.RealDurationInit(schedule.RealDurationSigned(state.mT0, time));
   return time;
}

PlaybackSlice
DefaultPlaybackPolicy::GetPlaybackSlice(
   const PlaybackSchedule &schedule, PlaybackState &state, size_t available)
{
   // How many samples to produce for each channel.
   const auto realTimeRemaining = std::max(0.0, state.RealDurationRemaining());
   mRemaining = realTimeRemaining * mRate / mLastPlaySpeed;

   auto frames = available;
   auto toProduce = frames;
   double deltat = (frames / mRate) * mLastPlaySpeed;

   if (deltat > realTimeRemaining) {
      toProduce = frames = 0.5 + (realTimeRemaining * mRate) / mLastPlaySpeed;
      auto realTime = realTimeRemaining;
      double extra = 0;
      if (RevertToOldDefault(schedule)) {
         // Produce some extra silence so that the time queue consumer can
         // satisfy its end condition
         const double extraRealTime =
            ((TimeQueueGrainSize + 1) / mRate) * mLastPlaySpeed;
         extra = std::min( extraRealTime, deltat - realTimeRemaining );
         frames = ((realTimeRemaining + extra) * mRate) / mLastPlaySpeed;
      }
      state.RealTimeAdvance(realTimeRemaining + extra);
   }
   else
      state.RealTimeAdvance(deltat);

   // Don't fall into an infinite loop, if loop-playing a selection
   // that is so short, it has no samples: detect that case
   if (frames == 0) {
      bool progress = (state.RealDurationElapsed() != 0.0);
      if (!progress)
         // Cause FillPlayBuffers to make progress, filling all available with 0
         frames = available, toProduce = 0;
   }
   return { available, frames, toProduce };
}

double DefaultPlaybackPolicy::AdvancedTrackTime(
   const PlaybackSchedule &schedule, PlaybackState &state,
   double trackTime, size_t nSamples)
{
   bool revert = RevertToOldDefault(schedule);
   if (!mVariableSpeed && revert)
      return PlaybackPolicy
         ::AdvancedTrackTime(schedule, state, trackTime, nSamples);

   mRemaining -= std::min(mRemaining, nSamples);
   if ( mRemaining == 0 && !revert )
      // Wrap to start
      return state.mT0;

   // Defense against cases that might cause loops not to terminate
   if (fabs(state.mT0 - state.mT1) < 1e-9)
      return state.mT0;

   auto realDuration = (nSamples / mRate) * mLastPlaySpeed;
   if (state.ReversedTime())
      realDuration *= -1.0;

   if (schedule.mEnvelope)
      trackTime =
         schedule.SolveWarpedLength(trackTime, realDuration);
   else
      trackTime += realDuration;

   return trackTime;
}

bool DefaultPlaybackPolicy::RepositionPlayback(PlaybackSchedule &schedule,
   PlaybackState &state, const Mixers &playbackMixers, size_t available)
{
   // This executes in the SequenceBufferExchange thread
   auto data = mMessageChannel.Read();

   bool speedChange = false;
   if (mVariableSpeed) {
      speedChange = (mLastPlaySpeed != data.mPlaySpeed);
      mLastPlaySpeed = data.mPlaySpeed;
   }

   bool empty = (data.mT0 >= data.mT1);
   bool kicked = false;

   // Amount in seconds by which right boundary can be moved left of the play
   // head, yet loop play in progress will still capture the head
   constexpr auto allowance = 0.5;

   // Looping may become enabled if the main thread said so, but require too
   // that the loop region is non-empty and the play head is not far to its
   // right
   bool loopWasEnabled = !RevertToOldDefault(schedule);
   mLoopEnabled = data.mLoopEnabled && !empty &&
      schedule.mTimeQueue.GetLastTime() <= data.mT1 + allowance;

   // Four cases:  looping transitions off, or transitions on, or stays on,
   // or stays off.
   // Besides which, the variable speed slider may have changed.

   // If looping transitions on, or remains on and the region changed,
   // adjust the schedule...
   auto mine = std::tie(state.mT0, mLoopEndTime);
   auto theirs = std::tie(data.mT0, data.mT1);
   if ((loopWasEnabled != mLoopEnabled) || (mLoopEnabled && mine != theirs))
   {
      kicked = true;
      if (!empty) {
         mine = theirs;
         state.mT1 = data.mT1;
      }
      if (!mLoopEnabled)
         // Continue play to the end
         state.mT1 = std::max(state.mT0, mTrackEndTime);
      state.mWarpedLength = schedule.RealDuration(state.mT0, state.mT1);

      // This may read an infinity
      auto newTime = schedule.mTimeQueue.GetLastTime();
#if 0
      // This would make play jump forward or backward into the adjusted
      // looping region if not already in it
      newTime = std::clamp(newTime, schedule.mT0, schedule.mT1);
#endif

      if (std::isfinite(newTime) && newTime >= state.mT1 && mLoopEnabled)
         newTime = state.mT0;

      // So that the play head will redraw in the right place:
      schedule.mTimeQueue.SetLastTime(newTime);

      state.RealDurationInit(schedule.RealDurationSigned(state.mT0, newTime));
      const auto realTimeRemaining =
         std::max(0.0, state.RealDurationRemaining());
      mRemaining = realTimeRemaining * mRate / mLastPlaySpeed;
   }
   else if (speedChange)
      // Don't return early
      kicked = true;
   else {
      // ... else the region did not change, or looping is now off, in
      // which case we have nothing special to do
      if (RevertToOldDefault(schedule))
         return PlaybackPolicy::RepositionPlayback(schedule, state,
            playbackMixers, available);
   }

   // msmeyer: If playing looped, check if we are at the end of the buffer
   // and if yes, restart from the beginning.
   if (mRemaining <= 0)
   {
      // Looping jumps left
      for (auto &pMixer : playbackMixers)
         pMixer->SetTimesAndSpeed(
            state.mT0, state.mT1, mLastPlaySpeed, true);
      state.RealTimeRestart();
   }
   else if (kicked)
   {
      // Play bounds need redefinition
      const auto time = schedule.mTimeQueue.GetLastTime();
      for (auto &pMixer : playbackMixers) {
         // So that the mixer will fetch the next samples from the right place:
         pMixer->SetTimesAndSpeed(time, state.mT1, mLastPlaySpeed);
         pMixer->Reposition(time, true);
      }
   }
   return false;
}

bool DefaultPlaybackPolicy::Looping( const PlaybackSchedule & ) const
{
   return mLoopEnabled;
}

void DefaultPlaybackPolicy::WriteMessage()
{
   const auto &region = ViewInfo::Get( mProject ).playRegion;
   mMessageChannel.Write( { GetPlaySpeed(),
      region.GetStart(), region.GetEnd(), region.Active()
   } );
}

double DefaultPlaybackPolicy::GetPlaySpeed()
{
   return mVariableSpeed
      ? ProjectAudioIO::Get(mProject).GetPlaySpeed()
      : 1.0;
}
