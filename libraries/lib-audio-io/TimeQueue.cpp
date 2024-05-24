/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file TimeQueue.cpp
 
 Paul Licameli split from PlaybackSchedule.cpp
 
 **********************************************************************/
#include "TimeQueue.h"
#include "PlaybackSchedule.h"

void TimeQueue::Reset(double *pLastTime)
{
   mpLastTime = pLastTime;
   mData = Records{};
   mHead = {};
   mTail = {};
}

void TimeQueue::Resize(size_t size)
{
   mData.resize(size);
}

void TimeQueue::Producer(
   PlaybackSchedule &schedule, PlaybackState &state, PlaybackSlice slice)
{
   assert(mpLastTime);
   auto &policy = schedule.GetPolicy();

   if ( mData.empty() )
      // Recording only.  Don't fill the queue.
      return;

   // Don't check available space:  assume it is enough because of coordination
   // with RingBuffer.
   auto index = mTail.mIndex;
   auto time = *mpLastTime;
   auto remainder = mTail.mRemainder;
   auto space = TimeQueueGrainSize - remainder;
   const auto size = mData.size();

   // Produce advancing times
   auto frames = slice.toProduce;
   while ( frames >= space ) {
      time = policy.AdvancedTrackTime(schedule, state, time, space);
      index = (index + 1) % size;
      mData[ index ].timeValue = time;
      frames -= space;
      remainder = 0;
      space = TimeQueueGrainSize;
   }
   // Last odd lot
   if ( frames > 0 ) {
      time = policy.AdvancedTrackTime(schedule, state, time, frames);
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

   *mpLastTime = time;
   mTail.mRemainder = remainder + frames;
   mTail.mIndex = index;
}

double TimeQueue::GetLastTime() const
{
   return mpLastTime ? *mpLastTime : 0;
}

void TimeQueue::SetLastTime(double time)
{
   if (mpLastTime)
      *mpLastTime = time;
}

std::optional<double>
TimeQueue::Consumer(size_t nSamples, double rate)
{
   assert(mpLastTime);
   if ( mData.empty() ) {
      // Recording only.  No scrub or playback time warp.  Don't use the queue.
      return { *mpLastTime += nSamples / rate };
   }

   // Don't check available space:  assume it is enough because of coordination
   // with RingBuffer.
   auto remainder = mHead.mRemainder;
   auto space = TimeQueueGrainSize - remainder;
   if (nSamples < space) {
      mHead.mRemainder += nSamples;
      return {};
   }
   const auto size = mData.size();
   mHead.mIndex = (mHead.mIndex + 1) % size;
   nSamples -= space;
   if (nSamples >= TimeQueueGrainSize)
      mHead.mIndex = (mHead.mIndex + (nSamples / TimeQueueGrainSize)) % size,
      nSamples %= TimeQueueGrainSize;
   mHead.mRemainder = nSamples;
   return { mData[ mHead.mIndex ].timeValue };
}

void TimeQueue::Prime(double time)
{
   mHead = mTail = {};
   SetLastTime(time);
   if ( !mData.empty() )
      mData[0].timeValue = time;
}
