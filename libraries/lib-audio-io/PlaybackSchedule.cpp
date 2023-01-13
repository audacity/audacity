/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file PlaybackSchedule.cpp
 
 Paul Licameli split from AudioIOBase.cpp
 
 **********************************************************************/

#include "PlaybackSchedule.h"

#include "AudioIOBase.h"
#include "Envelope.h"
#include "Mix.h"
#include "Project.h"
#include "SampleCount.h"

#include <cmath>

PlaybackPolicy::~PlaybackPolicy() = default;

void PlaybackPolicy::Initialize( PlaybackSchedule &, double rate )
{
   mRate = rate;
}

void PlaybackPolicy::Finalize( PlaybackSchedule & ){}

Mixer::WarpOptions PlaybackPolicy::MixerWarpOptions(PlaybackSchedule &schedule)
{
   return Mixer::WarpOptions{ schedule.mEnvelope };
}

PlaybackPolicy::BufferTimes
PlaybackPolicy::SuggestedBufferTimes(PlaybackSchedule &)
{
   using namespace std::chrono;
#if 1
   // Shorter times than in the default policy so that responses, to changes of
   // loop region or speed slider or other such controls, don't lag too much
   return { 0.05s, 0.05s, 0.25s };
#else
/*
The old values, going very far back.

There are old comments in the code about larger batches of work filling the
queue with samples, to reduce CPU usage.  Maybe this doesn't matter with most
modern machines, or maybe there will prove to be a need to choose the numbers
more smartly than these hardcoded values.  Maybe we will need to figure out
adaptiveness of the buffer size by detecting how long the work takes.  Maybe
we can afford even smaller times.
*/
   return { 4.0s, 4.0s, 10.0s };
#endif
}

bool PlaybackPolicy::AllowSeek(PlaybackSchedule &)
{
   return true;
}

bool PlaybackPolicy::Done( PlaybackSchedule &schedule,
   unsigned long outputFrames)
{
   // Called from portAudio thread, use GetTrackTime()
   auto diff = schedule.GetTrackTime() - schedule.mT1;
   if (schedule.ReversedTime())
      diff *= -1;
   return sampleCount(floor(diff * mRate + 0.5)) >= 0 &&
      // Require also that output frames are all consumed from ring buffer
      outputFrames == 0;
}

double PlaybackPolicy::OffsetTrackTime(
   PlaybackSchedule &schedule, double offset )
{
   const auto time = schedule.GetTrackTime() + offset;
   schedule.RealTimeInit( time );
   return time;
}

std::chrono::milliseconds PlaybackPolicy::SleepInterval(PlaybackSchedule &)
{
   using namespace std::chrono;
   return 10ms;
}

PlaybackSlice
PlaybackPolicy::GetPlaybackSlice(PlaybackSchedule &schedule, size_t available)
{
   // How many samples to produce for each channel.
   const auto realTimeRemaining = schedule.RealTimeRemaining();
   auto frames = available;
   auto toProduce = frames;
   double deltat = frames / mRate;

   if (deltat > realTimeRemaining)
   {
      // Produce some extra silence so that the time queue consumer can
      // satisfy its end condition
      const double extraRealTime = (TimeQueueGrainSize + 1) / mRate;
      auto extra = std::min( extraRealTime, deltat - realTimeRemaining );
      auto realTime = realTimeRemaining + extra;
      frames = realTime * mRate + 0.5;
      toProduce = realTimeRemaining * mRate + 0.5;
      schedule.RealTimeAdvance( realTime );
   }
   else
      schedule.RealTimeAdvance( deltat );

   return { available, frames, toProduce };
}

std::pair<double, double>
PlaybackPolicy::AdvancedTrackTime( PlaybackSchedule &schedule,
   double trackTime, size_t nSamples )
{
   auto realDuration = nSamples / mRate;
   if (schedule.ReversedTime())
      realDuration *= -1.0;

   if (schedule.mEnvelope)
      trackTime =
         schedule.SolveWarpedLength(trackTime, realDuration);
   else
      trackTime += realDuration;
   
   if ( trackTime >= schedule.mT1 )
      return { schedule.mT1, std::numeric_limits<double>::infinity() };
   else
      return { trackTime, trackTime };
}

bool PlaybackPolicy::RepositionPlayback(
   PlaybackSchedule &, const Mixers &, size_t, size_t)
{
   return true;
}

bool PlaybackPolicy::Looping(const PlaybackSchedule &) const
{
   return false;
}

namespace {
//! The old default playback policy plays once and consumes no messages
struct OldDefaultPlaybackPolicy final : PlaybackPolicy {
   ~OldDefaultPlaybackPolicy() override = default;
};
}

PlaybackPolicy &PlaybackSchedule::GetPolicy()
{
   if (mPolicyValid.load(std::memory_order_acquire) && mpPlaybackPolicy)
      return *mpPlaybackPolicy;

   static OldDefaultPlaybackPolicy defaultPolicy;
   return defaultPolicy;
}

const PlaybackPolicy &PlaybackSchedule::GetPolicy() const
{
   return const_cast<PlaybackSchedule&>(*this).GetPolicy();
}

void PlaybackSchedule::Init(
   const double t0, const double t1,
   const AudioIOStartStreamOptions &options,
   const RecordingSchedule *pRecordingSchedule )
{
   mpPlaybackPolicy.reset();

   if ( pRecordingSchedule )
      // It does not make sense to apply the time warp during overdub recording,
      // which defeats the purpose of making the recording synchronized with
      // the existing audio.  (Unless we figured out the inverse warp of the
      // captured samples in real time.)
      // So just quietly ignore the time track.
      mEnvelope = nullptr;
   else
      mEnvelope = options.envelope;

   mT0      = t0;
   if (pRecordingSchedule)
      mT0 -= pRecordingSchedule->mPreRoll;

   mT1      = t1;
   if (pRecordingSchedule)
      // adjust mT1 so that we don't give paComplete too soon to fill up the
      // desired length of recording
      mT1 -= pRecordingSchedule->mLatencyCorrection;

   // Main thread's initialization of mTime
   SetTrackTime( mT0 );

   if (options.policyFactory)
      mpPlaybackPolicy = options.policyFactory(options);

   mWarpedTime = 0.0;
   mWarpedLength = RealDuration(mT1);

   mPolicyValid.store(true, std::memory_order_release);
}

double PlaybackSchedule::ComputeWarpedLength(double t0, double t1) const
{
   if (mEnvelope)
      return mEnvelope->IntegralOfInverse(t0, t1);
   else
      return t1 - t0;
}

double PlaybackSchedule::SolveWarpedLength(double t0, double length) const
{
   if (mEnvelope)
      return mEnvelope->SolveIntegralOfInverse(t0, length);
   else
      return t0 + length;
}

double PlaybackSchedule::RealDuration(double trackTime1) const
{
   return fabs(RealDurationSigned(trackTime1));
}

double PlaybackSchedule::RealDurationSigned(double trackTime1) const
{
   return ComputeWarpedLength(mT0, trackTime1);
}

double PlaybackSchedule::RealTimeRemaining() const
{
   return mWarpedLength - mWarpedTime;
}

void PlaybackSchedule::RealTimeAdvance( double increment )
{
   mWarpedTime += increment;
}

void PlaybackSchedule::RealTimeInit( double trackTime )
{
   mWarpedTime = RealDurationSigned( trackTime );
}

void PlaybackSchedule::RealTimeRestart()
{
   mWarpedTime = 0;
}

double RecordingSchedule::ToConsume() const
{
   return mDuration - Consumed();
}

double RecordingSchedule::Consumed() const
{
   return std::max( 0.0, mPosition + TotalCorrection() );
}

double RecordingSchedule::ToDiscard() const
{
   return std::max(0.0, -( mPosition + TotalCorrection() ) );
}

void PlaybackSchedule::TimeQueue::Clear()
{
   mData = Records{};
   mHead = {};
   mTail = {};
}

void PlaybackSchedule::TimeQueue::Resize(size_t size)
{
   mData.resize(size);
}

void PlaybackSchedule::TimeQueue::Producer(
   PlaybackSchedule &schedule, PlaybackSlice slice )
{
   auto &policy = schedule.GetPolicy();

   if ( mData.empty() )
      // Recording only.  Don't fill the queue.
      return;

   // Don't check available space:  assume it is enough because of coordination
   // with RingBuffer.
   auto index = mTail.mIndex;
   auto time = mLastTime;
   auto remainder = mTail.mRemainder;
   auto space = TimeQueueGrainSize - remainder;
   const auto size = mData.size();

   // Produce advancing times
   auto frames = slice.toProduce;
   while ( frames >= space ) {
      auto times = policy.AdvancedTrackTime( schedule, time, space );
      time = times.second;
      if (!std::isfinite(time))
         time = times.first;
      index = (index + 1) % size;
      mData[ index ].timeValue = time;
      frames -= space;
      remainder = 0;
      space = TimeQueueGrainSize;
   }
   // Last odd lot
   if ( frames > 0 ) {
      auto times = policy.AdvancedTrackTime( schedule, time, frames );
      time = times.second;
      if (!std::isfinite(time))
         time = times.first;
      remainder += frames;
      space -= frames;
   }

   // Produce constant times if there is also some silence in the slice
   frames = slice.frames - slice.toProduce;
   while ( frames > 0 && frames >= space ) {
      index = (index + 1) % size;
      mData[ index ].timeValue = time;
      frames -= space;
      remainder = 0;
      space = TimeQueueGrainSize;
   }

   mLastTime = time;
   mTail.mRemainder = remainder + frames;
   mTail.mIndex = index;
}

double PlaybackSchedule::TimeQueue::GetLastTime() const
{
   return mLastTime;
}

void PlaybackSchedule::TimeQueue::SetLastTime(double time)
{
   mLastTime = time;
}

double PlaybackSchedule::TimeQueue::Consumer( size_t nSamples, double rate )
{
   if ( mData.empty() ) {
      // Recording only.  No scrub or playback time warp.  Don't use the queue.
      return ( mLastTime += nSamples / rate );
   }

   // Don't check available space:  assume it is enough because of coordination
   // with RingBuffer.
   auto remainder = mHead.mRemainder;
   auto space = TimeQueueGrainSize - remainder;
   const auto size = mData.size();
   if ( nSamples >= space ) {
      remainder = 0,
      mHead.mIndex = (mHead.mIndex + 1) % size,
      nSamples -= space;
      if ( nSamples >= TimeQueueGrainSize )
         mHead.mIndex =
            (mHead.mIndex + ( nSamples / TimeQueueGrainSize ) ) % size,
         nSamples %= TimeQueueGrainSize;
   }
   mHead.mRemainder = remainder + nSamples;
   return mData[ mHead.mIndex ].timeValue;
}

void PlaybackSchedule::TimeQueue::Prime(double time)
{
   mHead = mTail = {};
   mLastTime = time;
   if ( !mData.empty() )
      mData[0].timeValue = time;
}
