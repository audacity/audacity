/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 PlaybackSchedule.h
 
 Paul Licameli split from AudioIOBase.h
 
 **********************************************************************/

#ifndef __AUDACITY_PLAYBACK_SCHEDULE__
#define __AUDACITY_PLAYBACK_SCHEDULE__

#include <atomic>
#include <vector>

struct AudioIOStartStreamOptions;
class BoundedEnvelope;
using PRCrossfadeData = std::vector< std::vector < float > >;

struct RecordingSchedule {
   double mPreRoll{};
   double mLatencyCorrection{}; // negative value usually
   double mDuration{};
   PRCrossfadeData mCrossfadeData;

   // These are initialized by the main thread, then updated
   // only by the thread calling FillBuffers:
   double mPosition{};
   bool mLatencyCorrected{};

   double TotalCorrection() const { return mLatencyCorrection - mPreRoll; }
   double ToConsume() const;
   double Consumed() const;
   double ToDiscard() const;
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
   
   volatile enum {
      PLAY_STRAIGHT,
      PLAY_LOOPED,
#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
      PLAY_SCRUB,
      PLAY_AT_SPEED, // a version of PLAY_SCRUB.
      PLAY_KEYBOARD_SCRUB,
#endif
   }                   mPlayMode { PLAY_STRAIGHT };
   double              mCutPreviewGapStart;
   double              mCutPreviewGapLen;

   void Init(
      double t0, double t1,
      const AudioIOStartStreamOptions &options,
      const RecordingSchedule *pRecordingSchedule );

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

   /** \brief Clamps mTime to be between mT0 and mT1
    *
    * Returns the bound if the value is out of bounds; does not wrap.
    * Returns a time in seconds.
    */
   double LimitTrackTime() const;

   /** \brief Normalizes mTime, clamping it and handling gaps from cut preview.
    *
    * Clamps the time (unless scrubbing), and skips over the cut section.
    * Returns a time in seconds.
    */
   double NormalizeTrackTime() const;

   void ResetMode() { mPlayMode = PLAY_STRAIGHT; }

   bool PlayingStraight() const { return mPlayMode == PLAY_STRAIGHT; }
   bool Looping() const         { return mPlayMode == PLAY_LOOPED; }
   bool Scrubbing() const       { return mPlayMode == PLAY_SCRUB || mPlayMode == PLAY_KEYBOARD_SCRUB; }
   bool PlayingAtSpeed() const  { return mPlayMode == PLAY_AT_SPEED; }
   bool Interactive() const     { return Scrubbing() || PlayingAtSpeed(); }

   // Returns true if a loop pass, or the sole pass of straight play,
   // is completed at the current value of mTime
   bool PassIsComplete() const;

   // Returns true if time equals t1 or is on opposite side of t1, to t0
   bool Overruns( double trackTime ) const;

   // Compute the NEW track time for the given one and a real duration,
   // taking into account whether the schedule is for looping
   double AdvancedTrackTime(
      double trackTime, double realElapsed, double speed) const;

   // Use the function above in the callback after consuming samples from the
   // playback ring buffers, during usual straight or looping play
   void TrackTimeUpdate(double realElapsed);

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

};

#endif
