/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file ScrubState.h
 
 Paul Licameli split from AudioIO.cpp
 
 **********************************************************************/

#ifndef __AUDACITY_SCRUB_STATE__
#define __AUDACITY_SCRUB_STATE__

#include "PlaybackSchedule.h" // to inherit
#include "SampleCount.h"

// For putting an increment of work in the scrubbing queue
struct ScrubbingOptions {
   ScrubbingOptions() {}

   bool adjustStart {};

   // usually from TrackList::GetEndTime()
   double maxTime {};
   double minTime {};

   bool bySpeed {};
   bool isPlayingAtSpeed{};
   bool isKeyboardScrubbing{};

   double delay {};

   // Initial and limiting values for the speed of a scrub interval:
   double initSpeed { 1.0 };
   double minSpeed { 0.0 };
   double maxSpeed { 1.0 };


   // When maximum speed scrubbing skips to follow the mouse,
   // this is the minimum amount of playback allowed at the maximum speed:
   double minStutterTime {};

   static double MaxAllowedScrubSpeed()
   { return 32.0; } // Is five octaves enough for your amusement?
   static double MinAllowedScrubSpeed()
   { return 0.01; } // Mixer needs a lower bound speed.  Scrub no slower than this.
};

class ScrubbingPlaybackPolicy final : public PlaybackPolicy {
public:
   explicit ScrubbingPlaybackPolicy(const ScrubbingOptions &);
   ~ScrubbingPlaybackPolicy() override;

   void Initialize( PlaybackSchedule &schedule, double rate ) override;
   void Finalize( PlaybackSchedule &schedule ) override;

   Mixer::WarpOptions MixerWarpOptions(PlaybackSchedule &schedule) override;

   BufferTimes SuggestedBufferTimes(PlaybackSchedule &schedule) override;

   bool AllowSeek( PlaybackSchedule & ) override;

   std::chrono::milliseconds
      SleepInterval( PlaybackSchedule & ) override;

   bool Done( PlaybackSchedule &schedule, unsigned long ) override;

   PlaybackSlice GetPlaybackSlice(
      PlaybackSchedule &schedule, size_t available) override;

   std::pair<double, double>
      AdvancedTrackTime( PlaybackSchedule &schedule,
         double trackTime, size_t nSamples ) override;

   bool RepositionPlayback(
      PlaybackSchedule &schedule, const Mixers &playbackMixers,
      size_t frames,
      size_t available
   ) override;

private:
   sampleCount mScrubDuration{ 0 }, mStartSample{ 0 }, mEndSample{ 0 };
   double mOldEndTime{ 0 }, mNewStartTime{ 0 };
   double mScrubSpeed{ 0 };
   bool mSilentScrub{ false };
   bool mReplenish{ false };
   size_t mUntilDiscontinuity{ 0 };

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
