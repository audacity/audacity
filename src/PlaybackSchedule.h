/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 PlaybackSchedule.h
 
 Paul Licameli split from AudioIOBase.h
 
 **********************************************************************/

#ifndef __AUDACITY_PLAYBACK_SCHEDULE__
#define __AUDACITY_PLAYBACK_SCHEDULE__

#include "MemoryX.h"
#include "MessageBuffer.h"
#include "Mix.h"
#include "Observer.h"
#include <atomic>
#include <chrono>
#include <vector>

#include <wx/event.h>

class AudacityProject;
struct AudioIOStartStreamOptions;
class BoundedEnvelope;
using PRCrossfadeData = std::vector< std::vector < float > >;
class PlayRegionEvent;

constexpr size_t TimeQueueGrainSize = 2000;

struct RecordingSchedule {
   double mPreRoll{};
   double mLatencyCorrection{}; // negative value usually
   double mDuration{};
   PRCrossfadeData mCrossfadeData;

   // These are initialized by the main thread, then updated
   // only by the thread calling TrackBufferExchange:
   double mPosition{};
   bool mLatencyCorrected{};

   double TotalCorrection() const { return mLatencyCorrection - mPreRoll; }
   double ToConsume() const;
   double Consumed() const;
   double ToDiscard() const;
};

class Mixer;
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

//! Directs which parts of tracks to fetch for playback
/*!
 A non-default policy object may be created each time playback begins, and if so it is destroyed when
 playback stops, not reused in the next playback.

 Methods of the object are passed a PlaybackSchedule as context.
 */
class PlaybackPolicy {
public:
   using Duration = std::chrono::duration<double>;

   //! @name Called only by the main thread
   //! @{

   virtual ~PlaybackPolicy() = 0;

   //! Called before starting an audio stream
   virtual void Initialize( PlaybackSchedule &schedule, double rate );

   //! Called after stopping of an audio stream or an unsuccessful start
   virtual void Finalize( PlaybackSchedule &schedule );

   //! Options to use when constructing mixers for each playback track
   virtual Mixer::WarpOptions MixerWarpOptions(PlaybackSchedule &schedule);

   //! Times are in seconds
   struct BufferTimes {
      Duration batchSize; //!< Try to put at least this much into the ring buffer in each pass
      Duration latency; //!< Try not to let ring buffer contents fall below this
      Duration ringBufferDelay; //!< Length of ring buffer
   };
   //! Provide hints for construction of playback RingBuffer objects
   virtual BufferTimes SuggestedBufferTimes(PlaybackSchedule &schedule);
   //! @}

   //! @name Called by the PortAudio callback thread
   //! @{

   //! Whether repositioning commands are allowed during playback
   virtual bool AllowSeek( PlaybackSchedule &schedule );

   //! Returns true if schedule.GetTrackTime() has reached the end of playback
   virtual bool Done( PlaybackSchedule &schedule,
      unsigned long outputFrames //!< how many playback frames were taken from RingBuffers
   );

   //! Called when the play head needs to jump a certain distance
   /*! @param offset signed amount requested to be added to schedule::GetTrackTime()
      @return the new value that will be set as the schedule's track time
    */
   virtual double OffsetTrackTime( PlaybackSchedule &schedule, double offset );
   //! @}

   //! @name Called by the AudioIO::TrackBufferExchange thread
   //! @{

   //! How long to wait between calls to AudioIO::TrackBufferExchange
   virtual std::chrono::milliseconds
      SleepInterval( PlaybackSchedule &schedule );

   //! Choose length of one fetch of samples from tracks in a call to AudioIO::FillPlayBuffers
   virtual PlaybackSlice GetPlaybackSlice( PlaybackSchedule &schedule,
      size_t available //!< upper bound for the length of the fetch
   );

   //! Compute a new point in a track's timeline from an old point and a real duration
   /*!
    Needed because playback might be at non-unit speed.

    Called one or more times between GetPlaybackSlice and RepositionPlayback,
    until the sum of the nSamples values equals the most recent playback slice
    (including any trailing silence).
    
    @return a pair, which indicates a discontinuous jump when its members are not equal, or
       specially the end of playback when the second member is infinite
    */
   virtual std::pair<double, double>
      AdvancedTrackTime( PlaybackSchedule &schedule,
         double trackTime, size_t nSamples );

   using Mixers = std::vector<std::unique_ptr<Mixer>>;

   //! AudioIO::FillPlayBuffers calls this to update its cursors into tracks for changes of position or speed
   /*!
    @return if true, AudioIO::FillPlayBuffers stops producing samples even if space remains
    */
   virtual bool RepositionPlayback(
      PlaybackSchedule &schedule, const Mixers &playbackMixers,
      size_t frames, //!< how many samples were just now buffered for play
      size_t available //!< how many more samples may be buffered
   );
   //! @}

protected:
   double mRate = 0;
};

struct AUDACITY_DLL_API PlaybackSchedule {

   /// Playback starts at offset of mT0, which is measured in seconds.
   double              mT0;
   /// Playback ends at offset of mT1, which is measured in seconds.  Note that mT1 may be less than mT0 during scrubbing.
   double              mT1;
   /// Current track time position during playback, in seconds.
   /// Initialized by the main thread but updated by worker threads during
   /// playback or recording, and periodically reread by the main thread for
   /// purposes such as display update.
   std::atomic<double> mTime;

   /// Accumulated real time (not track position), starting at zero (unlike
   /// mTime), and wrapping back to zero each time around looping play.
   /// Thus, it is the length in real seconds between mT0 and mTime.
   double              mWarpedTime;

   /// Real length to be played (if looping, for each pass) after warping via a
   /// time track, computed just once when starting the stream.
   /// Length in real seconds between mT0 and mT1.  Always positive.
   double              mWarpedLength;

   // mWarpedTime and mWarpedLength are irrelevant when scrubbing,
   // else they are used in updating mTime,
   // and when not scrubbing or playing looped, mTime is also used
   // in the test for termination of playback.

   // with ComputeWarpedLength, it is now possible the calculate the warped length with 100% accuracy
   // (ignoring accumulated rounding errors during playback) which fixes the 'missing sound at the end' bug
   
   const BoundedEnvelope *mEnvelope;

   //! A circular buffer
   /*!
    Holds track time values corresponding to every nth sample in the
    playback buffers, for the large n == TimeQueueGrainSize.

    The "producer" is the Audio thread that fetches samples from tracks and
    fills the playback RingBuffers.  The "consumer" is the high-latency
    PortAudio thread that drains the RingBuffers.  The atomics in the
    RingBuffer implement lock-free synchronization.

    This other structure relies on the RingBuffer's synchronization, and adds
    other information to the stream of samples:  which track times they
    correspond to.

    The consumer thread uses that information, and also makes known to the main
    thread, what the last consumed track time is.  The main thread can use that
    for other purposes such as refreshing the display of the play head position.

    Producer and consumer also call back to AudioIOExt objects which might
    manage additional queues of other information.
    */
   class TimeQueue {
   public:
      //! @name Called only by the main thread
      //! @{
      void Clear();
      void Resize(size_t size);
      //! @}

      //! @name Called by the AudioIO::TrackBufferExchange thread
      //! @{

      //! Enqueue track time values
      /*! Time is advanced by `slice` according to `schedule`'s PlaybackPolicy
       */
      void Producer( PlaybackSchedule &schedule, PlaybackSlice slice );

      //! Return the last time saved by Producer
      double GetLastTime() const;

      void SetLastTime(double time);
      //! @}

      //! @name Called by the PortAudio callback thread
      //! @{

      //! Find the track time value `nSamples` after the last consumed sample
      double Consumer( size_t nSamples, double rate,
         unsigned long pauseFrames, bool hasSolo );
      //! @}

      //! @name Called by any thread while producer and consumer are suspended
      //! @{
      //! Empty the queue and reassign the last produced time
      /*! Assumes producer and consumer are suspended */
      void Prime( double time );
      //! @}

   private:
      void ProduceExt(std::pair<double, double> newTrackTimes, size_t nFrames);

      struct Record {
         double timeValue;
         // More fields to come
      };
      using Records = std::vector<Record>;
      Records mData;
      double mLastTime {};
      struct Cursor {
         size_t mIndex {};
         size_t mRemainder {};
      };
      //! Aligned to avoid false sharing
      NonInterfering<Cursor> mHead, mTail;
   } mTimeQueue;

   PlaybackPolicy &GetPolicy();
   const PlaybackPolicy &GetPolicy() const;

   void Init(
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

   /** \brief True if the end time is before the start time */
   bool ReversedTime() const
   {
      return mT1 < mT0;
   }

   /** \brief Get current track time value, unadjusted
    *
    * Returns a time in seconds.
    */
   double GetTrackTime() const
   { return mTime.load(std::memory_order_relaxed); }

   /** \brief Set current track time value, unadjusted
    */
   void SetTrackTime( double time )
   { mTime.store(time, std::memory_order_relaxed); }

   void ResetMode() {
      mPolicyValid.store(false, std::memory_order_release);
   }

   // Convert time between mT0 and argument to real duration, according to
   // time track if one is given; result is always nonnegative
   double RealDuration(double trackTime1) const;

   // Convert time between mT0 and argument to real duration, according to
   // time track if one is given; may be negative
   double RealDurationSigned(double trackTime1) const;

   // How much real time left?
   double RealTimeRemaining() const;

   // Advance the real time position
   void RealTimeAdvance( double increment );

   // Determine starting duration within the first pass -- sometimes not
   // zero
   void RealTimeInit( double trackTime );
   
   void RealTimeRestart();

private:
   std::unique_ptr<PlaybackPolicy> mpPlaybackPolicy;
   std::atomic<bool> mPolicyValid{ false };
};

class NewDefaultPlaybackPolicy final
   : public PlaybackPolicy
   , public NonInterferingBase
   , public wxEvtHandler
{
public:
   NewDefaultPlaybackPolicy( AudacityProject &project,
      double trackEndTime, double loopEndTime,
      bool loopEnabled, bool variableSpeed);
   ~NewDefaultPlaybackPolicy() override;

   void Initialize( PlaybackSchedule &schedule, double rate ) override;

   Mixer::WarpOptions MixerWarpOptions(PlaybackSchedule &schedule) override;

   BufferTimes SuggestedBufferTimes(PlaybackSchedule &schedule) override;

   bool Done( PlaybackSchedule &schedule, unsigned long ) override;

   PlaybackSlice GetPlaybackSlice(
      PlaybackSchedule &schedule, size_t available ) override;

   std::pair<double, double>
      AdvancedTrackTime( PlaybackSchedule &schedule,
         double trackTime, size_t nSamples ) override;

   bool RepositionPlayback(
      PlaybackSchedule &schedule, const Mixers &playbackMixers,
      size_t frames, size_t available ) override;

private:
   bool RevertToOldDefault( const PlaybackSchedule &schedule ) const;
   void OnPlayRegionChange(Observer::Message);
   void OnPlaySpeedChange(wxCommandEvent &evt);
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

   Observer::Subscription mSubscription;

   double mLastPlaySpeed{ 1.0 };
   const double mTrackEndTime;
   double mLoopEndTime;
   size_t mRemaining{ 0 };
   bool mProgress{ true };
   bool mLoopEnabled{ true };
   bool mVariableSpeed{ false };
};
#endif
