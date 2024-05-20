/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file PlaybackSchedule.h
 
 Paul Licameli split from AudioIOBase.h
 
 **********************************************************************/
#ifndef __AUDACITY_PLAYBACK_SCHEDULE__
#define __AUDACITY_PLAYBACK_SCHEDULE__

#include "MessageBuffer.h"
#include "Mix.h"
#include <atomic>
#include <chrono>

struct AudioIOStartStreamOptions;
using PRCrossfadeData = std::vector< std::vector < float > >;

constexpr size_t TimeQueueGrainSize = 2000;

struct RecordingSchedule {
   double mPreRoll{};
   double mLatencyCorrection{}; // negative value usually
   double mDuration{};
   PRCrossfadeData mCrossfadeData;

   // These are initialized by the main thread, then updated
   // only by the thread calling SequenceBufferExchange:
   double mPosition{};
   bool mLatencyCorrected{};

   double TotalCorrection() const { return mLatencyCorrection - mPreRoll; }
   double ToConsume() const;
   double Consumed() const;
   double ToDiscard() const;
};

struct PlaybackSchedule;

//! Describes an amount of contiguous (but maybe time-warped) data to be extracted from tracks to play
struct PlaybackSlice {
   const size_t frames; //!< Total number of frames to be buffered
   const size_t toProduce; //!< Not more than `frames`; the difference will be trailing silence

   //! Constructor enforces some invariants
   /*! @invariant `result.toProduce <= result.frames && result.frames <= available`
    */
   PlaybackSlice(
      size_t available, size_t frames_, size_t toProduce_)
      : frames{ std::min(available, frames_) }
      , toProduce{ std::min(toProduce_, frames) }
   {}
};

class AUDIO_IO_API PlaybackState
{
public:
   virtual ~PlaybackState();

   double RealDurationElapsed() const;

   //! How much real time left?
   double RealDurationRemaining() const;

   //! Advance the real time position
   void RealTimeAdvance(double increment);

   //! Determine starting duration within the first pass -- sometimes not
   //! zero
   void RealDurationInit(double duration);

   void RealTimeRestart();

   bool ReversedTime() const { return mT1 < mT0; }

   double mT0{};
   double mT1{};
   double mWarpedLength;

   double mLastTime{};

   //! Rather than a Clone() method, this assumes pre-allocation
   /*!
    @pre `other` was created by the same `PlaybackPolicy::CreateState` override
    as was `*this`
    */
   virtual void Assign(const PlaybackState &other);

private:
   //! Accumulated real time (not track position), starting at zero,
   /// and wrapping back to zero each time around looping play.
   double              mWarpedTime{};
};

// CRTP to generate Assign() override
template<typename Derived> struct AssignablePlaybackState : PlaybackState
{
   void Assign(const PlaybackState &other) override
   {
      *static_cast<Derived*>(this) = static_cast<const Derived&>(other);
   }
};

//! Directs which parts of tracks to fetch for playback
/*!
 A non-default policy object may be created each time playback begins, and if so it is destroyed when
 playback stops, not reused in the next playback.

 Methods of the object are passed a PlaybackSchedule as context.
 */
class AUDIO_IO_API PlaybackPolicy {
public:
   using Duration = std::chrono::duration<double>;

   //! @section Called by the main thread

   virtual ~PlaybackPolicy() = 0;

   //! Allow extension of PlaybackState; call only when starting playback
   /*!
    @post result: `result != nullptr`
    */
   virtual std::unique_ptr<PlaybackState> CreateState() const;

   //! Called to update schedule and state for rate, before starting an audio
   //! stream
   /*!
    @param state was made by `this->CreateState()`
    */
   virtual void Initialize(const PlaybackSchedule &schedule,
      PlaybackState &state, double rate);

   //! Called after stopping of an audio stream or an unsuccessful start
   virtual void Finalize(const PlaybackSchedule &schedule);

   //! Options to use when constructing mixers for each playback track
   virtual Mixer::WarpOptions
      MixerWarpOptions(const PlaybackSchedule &schedule);

   //! Times are in seconds
   struct BufferTimes {
      Duration batchSize; //!< Try to put at least this much into the ring buffer in each pass
      Duration latency; //!< Try not to let ring buffer contents fall below this
      Duration ringBufferDelay; //!< Length of ring buffer
   };
   //! Provide hints for construction of playback RingBuffer objects
   virtual BufferTimes SuggestedBufferTimes(const PlaybackSchedule &schedule);

   //! @section Called by the PortAudio callback thread

   //! Whether repositioning commands are allowed during playback
   virtual bool AllowSeek(const PlaybackSchedule &schedule);

   //! Called when the play head needs to jump a certain distance
   /*!
      @param state was made by `this->CreateState()`
      @param offset signed amount requested to be added to schedule::GetSequenceTime()
      @return the new value that will be set as the schedule's track time
    */
   virtual double OffsetSequenceTime(
      const PlaybackSchedule &schedule, PlaybackState &state, double offset);

   //! @section Called by the AudioIO::SequenceBufferExchange thread

   //! How long to wait between calls to AudioIO::SequenceBufferExchange
   virtual std::chrono::milliseconds
      SleepInterval(const PlaybackSchedule &schedule);

   //! Choose length of one fetch of samples from tracks (in a worker thread)
   /*!
    @param state was made by `this->CreateState()`
    */
   virtual PlaybackSlice GetPlaybackSlice(
      const PlaybackSchedule &schedule, PlaybackState &state,
      size_t available //!< upper bound for the length of the fetch
   ) const;

   //! Compute a new point in a track's timeline from an old point and a real duration
   /*!
    Needed because playback might be at non-unit speed.

    Called one or more times between GetPlaybackSlice and RepositionPlayback,
    until the sum of the nSamples values equals the most recent playback slice
    (including any trailing silence).
    
    @param state was made by `this->CreateState()`
    @return updated track time, or infinity to enqueue a stop signal
    */
   virtual double
      AdvancedTrackTime(const PlaybackSchedule &schedule, PlaybackState &state,
         double trackTime, size_t nSamples) const;

   using Mixers = std::vector<std::unique_ptr<Mixer>>;

   //! A worker thread calls this to update its cursors into tracks for changes
   //! of position or speed
   /*!
    @param state was made by `this->CreateState()`
    @return if true, AudioIO::FillPlayBuffers stops producing samples even if space remains
    */
   virtual bool RepositionPlayback(
      const PlaybackSchedule &schedule, PlaybackState &state,
      const Mixers &playbackMixers,
      size_t available //!< how many more samples may be buffered
   ) const;

   //! @section To be removed

   virtual bool Looping( const PlaybackSchedule &schedule ) const;

protected:
   double mRate = 0;
};

struct AUDIO_IO_API PlaybackSchedule {
   /// Playback starts at offset of mInitT0, which is measured in seconds.
   double              mInitT0;
   /// Playback ends at offset of mInitT1, which is measured in seconds.
   double              mInitT1;

   /// Positive, real length in seconds to be played (if looping, for each
   /// pass) after warping via a time track
   double              mInitWarpedLength;

   // mWarpedTime and mWarpedLength are irrelevant when scrubbing,
   // else they are used in updating mTime,
   // and when not scrubbing or playing looped, mTime is also used
   // in the test for termination of playback.

   // with ComputeWarpedLength, it is now possible the calculate the warped length with 100% accuracy
   // (ignoring accumulated rounding errors during playback) which fixes the 'missing sound at the end' bug
   
   const BoundedEnvelope *mEnvelope;

   PlaybackPolicy &GetPolicy();
   const PlaybackPolicy &GetPolicy() const;

   std::unique_ptr<PlaybackState> Init(
      double t0, double t1,
      const AudioIOStartStreamOptions &options,
      const RecordingSchedule *pRecordingSchedule );

   /** @brief Compute signed duration (in seconds at playback) of the specified region of the track.
    *
    * Takes a region of the time track (specified by the unwarped time points in the project), and
    * calculates how long it will actually take to play this region back, taking the time track's
    * warping effects into account.
    * @param t0 unwarped time to start calculation from
    * @param t1 unwarped time to stop calculation at
    * @return the warped duration in seconds, negated if `t0 > t1`
    */
   double ComputeWarpedLength(double t0, double t1) const;

   /** @brief Compute how much unwarped time must have elapsed if length seconds of warped time has
    * elapsed, and add to t0
    *
    * @param t0 The unwarped time (seconds from project start) at which to start
    * @param length How many seconds of real time went past; signed
    * @return The end point (in seconds from project start) as unwarped time
    */
   double SolveWarpedLength(double t0, double length) const;

   /** \brief Get current track time value, unadjusted
    *
    * Returns a time in seconds.
    */
   double GetSequenceTime() const
   { return mTime.load(std::memory_order_relaxed); }

   /** \brief Set current track time value, unadjusted
    */
   void SetSequenceTime( double time )
   { mTime.store(time, std::memory_order_relaxed); }

   void ResetMode() {
      mPolicyValid.store(false, std::memory_order_release);
   }

   //! Convert time between arguments to real duration, according to
   //! time track if one is given; result is always nonnegative
   double RealDuration(double trackTime0, double trackTime1) const;

   //! Convert time between arguments to real duration, according to
   //! time track if one is given; may be negative
   double
   RealDurationSigned(double trackTime0, double trackTime1) const;

private:
   /// Current track time position during playback, in seconds.
   /// Initialized by the main thread but updated by worker threads during
   /// playback or recording, and periodically reread by the main thread for
   /// purposes such as display update.
   std::atomic<double> mTime;

   std::unique_ptr<PlaybackPolicy> mpPlaybackPolicy;
   std::atomic<bool> mPolicyValid{ false };
};
#endif
