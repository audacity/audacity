/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file TimeQueue.h
 
 Paul Licameli split from PlaybackSchedule.h
 
 **********************************************************************/
#ifndef __AUDACITY_TIME_QUEUE__
#define __AUDACITY_TIME_QUEUE__

struct PlaybackSchedule;
class PlaybackState;
struct PlaybackSlice;

#include <optional>

#include "MemoryX.h"
#include <vector>

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
class AUDIO_IO_API TimeQueue {
public:

   //! @section called by main thread

   void Reset(double *pLastTime);
   void Resize(size_t size);

   //! @section Called by the AudioIO::SequenceBufferExchange thread

   //! Enqueue track time value advanced by the slice according to `schedule`'s PlaybackPolicy
   /*!
    @pre `this` was last `Reset` with a non-null argument
    @param state was made by `schedule.GetPolicy().CreateState()`
    */
   void Producer(PlaybackSchedule &schedule,
      PlaybackState &state, PlaybackSlice slice);

   //! Return the last time saved by Producer
   double GetLastTime() const;

   void SetLastTime(double time);

   //! @section called by PortAudio callback thread

   //! Find the track time value `nSamples` after the last consumed sample
   /*!
    @pre `this` was last `Reset` with a non-null argument
    @return nullopt when playing but no time queue grain boundary was crossed
    */
   std::optional<double> Consumer(size_t nSamples, double rate);

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
   double *mpLastTime;
   struct Cursor {
      size_t mIndex {};
      size_t mRemainder {};
   };
   //! Aligned to avoid false sharing
   NonInterfering<Cursor> mHead, mTail;
};

#endif
