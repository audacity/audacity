/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 PlaybackSchedule.h
 
 Paul Licameli split from AudioIOBase.h
 
 **********************************************************************/

#ifndef __AUDACITY_PLAYBACK_SCHEDULE__
#define __AUDACITY_PLAYBACK_SCHEDULE__

#include "MemoryX.h"
#include "Mix.h"
#include <atomic>
#include <chrono>
#include <vector>

struct AudioIOStartStreamOptions;
class BoundedEnvelope;
using PRCrossfadeData = std::vector< std::vector < float > >;
class PlayRegionEvent;

constexpr size_t TimeQueueGrainSize = 2000;

//! Communicate data atomically from one writer thread to one reader.
/*!
 This is not a queue: it is not necessary for each write to be read.
 Rather loss of a message is allowed:  writer may overwrite.
 Data must be default-constructible and either copyable or movable.
 */
template<typename Data>
class MessageBuffer {
   struct UpdateSlot {
      std::atomic<bool> mBusy{ false };
      Data mData;
   };
   NonInterfering<UpdateSlot> mSlots[2];

   std::atomic<unsigned char> mLastWrittenSlot{ 0 };

public:
   void Initialize();

   //! Move data out (if available), or else copy it out
   Data Read();
   
   //! Copy data in
   void Write( const Data &data );
   //! Move data in
   void Write( Data &&data );
};

template<typename Data>
void MessageBuffer<Data>::Initialize()
{
   for (auto &slot : mSlots)
      // Lock both slots first, maybe spinning a little
      while ( slot.mBusy.exchange( true, std::memory_order_acquire ) )
         {}

   mSlots[0].mData = {};
   mSlots[1].mData = {};
   mLastWrittenSlot.store( 0, std::memory_order_relaxed );

   for (auto &slot : mSlots)
      slot.mBusy.exchange( false, std::memory_order_release );
}

template<typename Data>
Data MessageBuffer<Data>::Read()
{
   // Whichever slot was last written, prefer to read that.
   auto idx = mLastWrittenSlot.load( std::memory_order_relaxed );
   idx = 1 - idx;
   bool wasBusy = false;
   do {
      // This loop is unlikely to execute twice, but it might because the
      // producer thread is writing a slot.
      idx = 1 - idx;
      wasBusy = mSlots[idx].mBusy.exchange( true, std::memory_order_acquire );
   } while ( wasBusy );

   // Copy the slot
   auto result = std::move( mSlots[idx].mData );

   mSlots[idx].mBusy.store( false, std::memory_order_release );

   return result;
}

template<typename Data>
void MessageBuffer<Data>::Write( const Data &data )
{
   // Whichever slot was last written, prefer to write the other.
   auto idx = mLastWrittenSlot.load( std::memory_order_relaxed );
   bool wasBusy = false;
   do {
      // This loop is unlikely to execute twice, but it might because the
      // consumer thread is reading a slot.
      idx = 1 - idx;
      wasBusy = mSlots[idx].mBusy.exchange( true, std::memory_order_acquire );
   } while ( wasBusy );

   mSlots[idx].mData = data;
   mLastWrittenSlot.store( idx, std::memory_order_relaxed );

   mSlots[idx].mBusy.store( false, std::memory_order_release );
}

template<typename Data>
void MessageBuffer<Data>::Write( Data &&data )
{
   // Whichever slot was last written, prefer to write the other.
   auto idx = mLastWrittenSlot.load( std::memory_order_relaxed );
   bool wasBusy = false;
   do {
      // This loop is unlikely to execute twice, but it might because the
      // consumer thread is reading a slot.
      idx = 1 - idx;
      wasBusy = mSlots[idx].mBusy.exchange( true, std::memory_order_acquire );
   } while ( wasBusy );

   mSlots[idx].mData = std::move( data );
   mLastWrittenSlot.store( idx, std::memory_order_relaxed );

   mSlots[idx].mBusy.store( false, std::memory_order_release );
}

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
   //! @section Called by the main thread

   virtual ~PlaybackPolicy() = 0;

   //! Called before starting an audio stream
   virtual void Initialize( PlaybackSchedule &schedule, double rate );

   //! Called after stopping of an audio stream or an unsuccessful start
   virtual void Finalize( PlaybackSchedule &schedule );

   //! Options to use when constructing mixers for each playback track
   virtual Mixer::WarpOptions MixerWarpOptions(PlaybackSchedule &schedule);

   //! Times are in seconds
   struct BufferTimes {
      double batchSize; //!< Try to put at least this much into the ring buffer in each pass
      double latency; //!< Try not to let ring buffer contents fall below this
      double ringBufferDelay; //!< Length of ring buffer in seconds
   };
   //! Provide hints for construction of playback RingBuffer objects
   virtual BufferTimes SuggestedBufferTimes(PlaybackSchedule &schedule);

   //! @section Called by the PortAudio callback thread

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

   //! @section Called by the AudioIO::TrackBufferExchange thread

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

   //! @section To be removed

   virtual bool Looping( const PlaybackSchedule &schedule ) const;

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
   /*
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
    */
   class TimeQueue {
   public:

      //! @section called by main thread

      void Clear();
      void Resize(size_t size);

      //! @section Called by the AudioIO::TrackBufferExchange thread

      //! Enqueue track time value advanced by `nSamples` according to `schedule`'s PlaybackPolicy
      void Producer( PlaybackSchedule &schedule, size_t nSamples );

      //! Return the last time saved by Producer
      double GetLastTime() const;

      //! @section called by PortAudio callback thread

      //! Find the track time value `nSamples` after the last consumed sample
      double Consumer( size_t nSamples, double rate );

      //! @section called by any thread while producer and consumer are suspended

      //! Empty the queue and reassign the last produced time
      /*! Assumes producer and consumer are suspended */
      void Prime( double time );

   private:
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

   // The main thread writes changes in response to user events, and
   // the audio thread later reads, and changes the playback.
   struct SlotData {
      double mT0;
      double mT1;
   };
   MessageBuffer<SlotData> mMessageChannel;

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

   void MessageProducer( PlayRegionEvent &evt );
   void MessageConsumer();

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

   /** \brief Clamps argument to be between mT0 and mT1
    *
    * Returns the bound if the value is out of bounds; does not wrap.
    * Returns a time in seconds.
    */
   double ClampTrackTime( double trackTime ) const;

   void ResetMode() {
      mPolicyValid.store(false, std::memory_order_release);
   }

   // Convert time between mT0 and argument to real duration, according to
   // time track if one is given; result is always nonnegative
   double RealDuration(double trackTime1) const;

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

class LoopingPlaybackPolicy final : public PlaybackPolicy {
public:
   ~LoopingPlaybackPolicy() override;

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

   bool Looping( const PlaybackSchedule & ) const override;

private:
   size_t mRemaining{ 0 };
   bool mProgress{ true };
};
#endif
