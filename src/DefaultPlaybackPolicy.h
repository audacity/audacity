/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file DefaultPlaybackPolicy.h
 
 Paul Licameli split from PlaybackSchedule.h
 
 **********************************************************************/
#ifndef __AUDACITY_DEFAULT_PLAYBACK_POLICY__
#define __AUDACITY_DEFAULT_PLAYBACK_POLICY__

#include "PlaybackSchedule.h"
#include "SharedObjectMessageBuffer.h"

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
   // The main thread writes changes in response to user events, and
   // the audio thread later reads, and changes the playback.
   struct SlotData : PlaybackMessage {
      SlotData() = default;
      SlotData(double playSpeed, double t0, double t1, bool loopEnabled)
         : mPlaySpeed{ playSpeed }, mT0{ t0 }, mT1{ t1 }
         , mLoopEnabled{ loopEnabled }
      {}

      double mPlaySpeed{};
      double mT0{};
      double mT1{};
      bool mLoopEnabled{};
   };

   DefaultPlaybackPolicy( AudacityProject &project,
      double trackEndTime, double loopEndTime, std::optional<double> pStartTime,
      bool loopEnabled, bool variableSpeed);
   ~DefaultPlaybackPolicy() override;

   std::unique_ptr<PlaybackState> CreateState() const override;

   void Initialize(const PlaybackSchedule &schedule,
      PlaybackState &state, double rate) override;

   Mixer::WarpOptions
   MixerWarpOptions(const PlaybackSchedule &schedule) override;

   BufferTimes
   SuggestedBufferTimes(const PlaybackSchedule &schedule) override;

   double OffsetSequenceTime(const PlaybackSchedule& schedule,
      PlaybackState &state, double offset) override;

   PlaybackSlice GetPlaybackSlice(const PlaybackSchedule &schedule,
      PlaybackState &state, size_t available) const override;

   double
      AdvancedTrackTime(const PlaybackSchedule &schedule, PlaybackState &state,
         double trackTime, size_t nSamples) const override;

   std::shared_ptr<PlaybackMessage>
      PollUser(const PlaybackSchedule &schedule) const override;

   bool RepositionPlayback(const PlaybackSchedule &schedule,
      PlaybackState &state, const PlaybackMessage &message,
      Mixer *pMixer, size_t available)
   const override;

   bool Looping( const PlaybackSchedule & ) const override;

private:
   void WriteMessage();
   double GetPlaySpeed();

   AudacityProject &mProject;

   SharedObjectMessageBuffer<SlotData> mMessageChannel;

   Observer::Subscription mRegionSubscription,
      mSpeedSubscription;

   const double mTrackEndTime;
   double mInitLoopEndTime{};
   std::optional<double> mpStartTime;
   bool mProgress{ true };
   bool mVariableSpeed{ false };
   const bool mInitLoopEnabled;
};

#endif
