/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file ScrubState.h
 
 Paul Licameli split from AudioIO.cpp
 
 **********************************************************************/

#ifndef __AUDACITY_SCRUB_STATE__
#define __AUDACITY_SCRUB_STATE__

#include "PlaybackSchedule.h" // to inherit
#include "SampleCount.h"

#include "AudioIOBase.h" // for ScrubbingOptions

class ScrubbingPlaybackPolicy final : public PlaybackPolicy {
public:
   explicit ScrubbingPlaybackPolicy(const ScrubbingOptions &);
   ~ScrubbingPlaybackPolicy() override;

   void Initialize( PlaybackSchedule &schedule, double rate ) override;
   void Finalize( PlaybackSchedule &schedule ) override;

   Mixer::WarpOptions MixerWarpOptions(PlaybackSchedule &schedule) override;

   BufferTimes SuggestedBufferTimes(PlaybackSchedule &schedule) override;

   double NormalizeTrackTime( PlaybackSchedule &schedule ) override;

   bool AllowSeek( PlaybackSchedule & ) override;

   std::chrono::milliseconds
      SleepInterval( PlaybackSchedule & ) override;

   bool Done( PlaybackSchedule &schedule, unsigned long ) override;

   PlaybackSlice GetPlaybackSlice(
      PlaybackSchedule &schedule, size_t available) override;

   double AdvancedTrackTime( PlaybackSchedule &schedule,
      double trackTime, double realDuration ) override;

   bool RepositionPlayback(
      PlaybackSchedule &schedule, const Mixers &playbackMixers,
      size_t frames,
      size_t available
   ) override;

private:
   sampleCount mScrubDuration{ 0 }, mStartSample{ 0 }, mEndSample{ 0 };
   double mScrubSpeed{ 0 };
   bool mSilentScrub{ false };
   bool mReplenish{ false };

   const ScrubbingOptions mOptions;
};

struct ScrubState
{
   static bool IsScrubbing();

   /** \brief Notify scrubbing engine of desired position or speed.
   * If options.adjustStart is true, then when mouse movement exceeds maximum
   * scrub speed, adjust the beginning of the scrub interval rather than the
   * end, so that the scrub skips or "stutters" to stay near the cursor.
   */
   static void UpdateScrub(double endTimeOrSpeed, const ScrubbingOptions &options);

   static void StopScrub();

   /** \brief return the ending time of the last scrub interval.
   */
   static double GetLastScrubTime();
};

static constexpr unsigned ScrubPollInterval_ms = 50;

#endif
