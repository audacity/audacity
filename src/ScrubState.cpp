/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file ScrubState.cpp
 
 Paul Licameli split from AudioIO.cpp
 
 **********************************************************************/

#include "ScrubState.h"
#include "AudioIO.h"

ScrubbingPlaybackPolicy::ScrubbingPlaybackPolicy(
   const ScrubbingOptions &options)
   : mOptions{ options }
{}

ScrubbingPlaybackPolicy::~ScrubbingPlaybackPolicy() = default;

double ScrubbingPlaybackPolicy::NormalizeTrackTime( PlaybackSchedule &schedule )
{
   return schedule.GetTrackTime();
}

bool ScrubbingPlaybackPolicy::AllowSeek( PlaybackSchedule & )
{
   // While scrubbing, ignore seek requests
   return false;
}

bool ScrubbingPlaybackPolicy::Done(
   PlaybackSchedule &schedule, unsigned long )
{
   if (mOptions.isPlayingAtSpeed)
      // some leftover length allowed in this case; ignore outputFrames
      return PlaybackPolicy::Done(schedule, 0);
   else
      return false;
}

std::chrono::milliseconds
ScrubbingPlaybackPolicy::SleepInterval( PlaybackSchedule & )
{
   return std::chrono::milliseconds{ ScrubPollInterval_ms };
}

PlaybackSlice ScrubbingPlaybackPolicy::GetPlaybackSlice(
   PlaybackSchedule &, size_t available)
{
   auto gAudioIO = AudioIO::Get();
   auto &mScrubDuration = gAudioIO->mScrubDuration;
   auto &mSilentScrub = gAudioIO->mSilentScrub;

   // How many samples to produce for each channel.
   auto frames = available;
   auto toProduce = frames;

   // scrubbing and play-at-speed are not limited by the real time
   // and length accumulators
   toProduce =
      frames = limitSampleBufferSize(frames, mScrubDuration);

   if (mSilentScrub)
      toProduce = 0;

   return { available, frames, toProduce, true };
}
