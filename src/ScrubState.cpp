/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file ScrubState.cpp
 
 Paul Licameli split from AudioIO.cpp
 
 **********************************************************************/

#include "ScrubState.h"
#include "AudioIOBase.h"

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
