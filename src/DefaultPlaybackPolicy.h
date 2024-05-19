/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file DefaultPlaybackPolicy.h
 
 Paul Licameli split from PlaybackSchedule.h
 
 **********************************************************************/
#ifndef __AUDACITY_DEFAULT_PLAYBACK_POLICY__
#define __AUDACITY_DEFAULT_PLAYBACK_POLICY__

#include "PlaybackSchedule.h"

//! The PlaybackPolicy used by Audacity for most playback.
/*! It subscribes to messages from ViewInfo and PlayRegion for loop bounds
 adjustment.  Therefore it is not a low-level class that can be defined with
 the playback engine.
 */
class DefaultPlaybackPolicy final
   : public PlaybackPolicy
   , public NonInterferingBase
{
public:
   DefaultPlaybackPolicy( AudacityProject &project,
      double trackEndTime, double loopEndTime, std::optional<double> pStartTime,
      bool loopEnabled, bool variableSpeed);
   ~DefaultPlaybackPolicy() override;

   void Initialize(const PlaybackSchedule &schedule,
      PlaybackState &state, double rate) override;

   Mixer::WarpOptions MixerWarpOptions(PlaybackSchedule &schedule) override;

   BufferTimes SuggestedBufferTimes(PlaybackSchedule &schedule) override;

   double OffsetSequenceTime(const PlaybackSchedule& schedule,
      PlaybackState &state, double offset) override;

   PlaybackSlice GetPlaybackSlice(const PlaybackSchedule &schedule,
      PlaybackState &state, size_t available) override;

   double
      AdvancedTrackTime(const PlaybackSchedule &schedule, PlaybackState &state,
         double trackTime, size_t nSamples) override;

   bool RepositionPlayback(PlaybackSchedule &schedule, PlaybackState &state,
      const Mixers &playbackMixers, size_t available) override;

   bool Looping( const PlaybackSchedule & ) const override;

private:
   bool RevertToOldDefault( const PlaybackSchedule &schedule ) const;
   void WriteMessage();
   double GetPlaySpeed();

   AudacityProject &mProject;

   // The main thread writes changes in response to user events, and
   // the audio thread later reads, and changes the playback.
   struct SlotData {
      double mPlaySpeed;
      double mT0;
      double mT1;
      bool mLoopEnabled;
   };
   MessageBuffer<SlotData> mMessageChannel;

   Observer::Subscription mRegionSubscription,
      mSpeedSubscription;

   double mLastPlaySpeed{ 1.0 };
   const double mTrackEndTime;
   double mLoopEndTime;
   std::optional<double> mpStartTime;
   size_t mRemaining{ 0 };
   bool mProgress{ true };
   bool mLoopEnabled{ true };
   bool mVariableSpeed{ false };
};

#endif
