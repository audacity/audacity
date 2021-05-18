/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 PlaybackSchedule.cpp
 
 Paul Licameli split from AudioIOBase.cpp
 
 **********************************************************************/

#include "PlaybackSchedule.h"

#include "AudioIOBase.h"
#include "Envelope.h"

void PlaybackSchedule::Init(
   const double t0, const double t1,
   const AudioIOStartStreamOptions &options,
   const RecordingSchedule *pRecordingSchedule )
{
   if ( pRecordingSchedule )
      // It does not make sense to apply the time warp during overdub recording,
      // which defeats the purpose of making the recording synchronized with
      // the existing audio.  (Unless we figured out the inverse warp of the
      // captured samples in real time.)
      // So just quietly ignore the time track.
      mEnvelope = nullptr;
   else
      mEnvelope = options.envelope;

   mT0      = t0;
   if (pRecordingSchedule)
      mT0 -= pRecordingSchedule->mPreRoll;

   mT1      = t1;
   if (pRecordingSchedule)
      // adjust mT1 so that we don't give paComplete too soon to fill up the
      // desired length of recording
      mT1 -= pRecordingSchedule->mLatencyCorrection;

   // Main thread's initialization of mTime
   SetTrackTime( mT0 );

   mPlayMode = options.playLooped
      ? PlaybackSchedule::PLAY_LOOPED
      : PlaybackSchedule::PLAY_STRAIGHT;
   mCutPreviewGapStart = options.cutPreviewGapStart;
   mCutPreviewGapLen = options.cutPreviewGapLen;

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   bool scrubbing = (options.pScrubbingOptions != nullptr);

   // Scrubbing is not compatible with looping or recording or a time track!
   if (scrubbing)
   {
      const auto &scrubOptions = *options.pScrubbingOptions;
      if (pRecordingSchedule ||
          Looping() ||
          mEnvelope ||
          scrubOptions.maxSpeed < ScrubbingOptions::MinAllowedScrubSpeed()) {
         wxASSERT(false);
         scrubbing = false;
      }
      else {
         if (scrubOptions.isPlayingAtSpeed)
            mPlayMode = PLAY_AT_SPEED;
         else if (scrubOptions.isKeyboardScrubbing)
            mPlayMode = PLAY_KEYBOARD_SCRUB;
         else
            mPlayMode = PLAY_SCRUB;
      }
   }
#endif

   mWarpedTime = 0.0;
#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   if (Scrubbing())
      mWarpedLength = 0.0f;
   else
#endif
      mWarpedLength = RealDuration(mT1);
}

double PlaybackSchedule::LimitTrackTime() const
{
   // Track time readout for the main thread
   // Allows for forward or backward play
   return ClampTrackTime( GetTrackTime() );
}

double PlaybackSchedule::ClampTrackTime( double trackTime ) const
{
   if (ReversedTime())
      return std::max(mT1, std::min(mT0, trackTime));
   else
      return std::max(mT0, std::min(mT1, trackTime));
}

double PlaybackSchedule::NormalizeTrackTime() const
{
   // Track time readout for the main thread

   // dmazzoni: This function is needed for two reasons:
   // One is for looped-play mode - this function makes sure that the
   // position indicator keeps wrapping around.  The other reason is
   // more subtle - it's because PortAudio can query the hardware for
   // the current stream time, and this query is not always accurate.
   // Sometimes it's a little behind or ahead, and so this function
   // makes sure that at least we clip it to the selection.
   //
   // msmeyer: There is also the possibility that we are using "cut preview"
   //          mode. In this case, we should jump over a defined "gap" in the
   //          audio.

   double absoluteTime;

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   // Limit the time between t0 and t1 if not scrubbing.
   // Should the limiting be necessary in any play mode if there are no bugs?
   if (Interactive())
      absoluteTime = GetTrackTime();
   else
#endif
      absoluteTime = LimitTrackTime();

   if (mCutPreviewGapLen > 0)
   {
      // msmeyer: We're in cut preview mode, so if we are on the right
      // side of the gap, we jump over it.
      if (absoluteTime > mCutPreviewGapStart)
         absoluteTime += mCutPreviewGapLen;
   }

   return absoluteTime;
}

bool PlaybackSchedule::PassIsComplete() const
{
   // Test mTime within the PortAudio callback
   if (Scrubbing())
      return false; // but may be true if playing at speed
   return Overruns( GetTrackTime() );
}

bool PlaybackSchedule::Overruns( double trackTime ) const
{
   return (ReversedTime() ? trackTime <= mT1 : trackTime >= mT1);
}

namespace
{
/** @brief Compute the duration (in seconds at playback) of the specified region of the track.
 *
 * Takes a region of the time track (specified by the unwarped time points in the project), and
 * calculates how long it will actually take to play this region back, taking the time track's
 * warping effects into account.
 * @param t0 unwarped time to start calculation from
 * @param t1 unwarped time to stop calculation at
 * @return the warped duration in seconds
 */
double ComputeWarpedLength(const Envelope &env, double t0, double t1)
{
   return env.IntegralOfInverse(t0, t1);
}

/** @brief Compute how much unwarped time must have elapsed if length seconds of warped time has
 * elapsed
 *
 * @param t0 The unwarped time (seconds from project start) at which to start
 * @param length How many seconds of warped time went past.
 * @return The end point (in seconds from project start) as unwarped time
 */
double SolveWarpedLength(const Envelope &env, double t0, double length)
{
   return env.SolveIntegralOfInverse(t0, length);
}
}

double PlaybackSchedule::AdvancedTrackTime(
   double time, double realElapsed, double speed ) const
{
   if (ReversedTime())
      realElapsed *= -1.0;

   // Defense against cases that might cause loops not to terminate
   if ( fabs(mT0 - mT1) < 1e-9 )
      return mT0;

   if (mEnvelope) {
       wxASSERT( speed == 1.0 );

      double total=0.0;
      bool foundTotal = false;
      do {
         auto oldTime = time;
         if (foundTotal && fabs(realElapsed) > fabs(total))
            // Avoid SolveWarpedLength
            time = mT1;
         else
            time = SolveWarpedLength(*mEnvelope, time, realElapsed);

         if (!Looping() || !Overruns( time ))
            break;

         // Bug1922:  The part of the time track outside the loop should not
         // influence the result
         double delta;
         if (foundTotal && oldTime == mT0)
            // Avoid integrating again
            delta = total;
         else {
            delta = ComputeWarpedLength(*mEnvelope, oldTime, mT1);
            if (oldTime == mT0)
               foundTotal = true, total = delta;
         }
         realElapsed -= delta;
         time = mT0;
      } while ( true );
   }
   else {
      time += realElapsed * fabs(speed);

      // Wrap to start if looping
      if (Looping()) {
         while ( Overruns( time ) ) {
            // LL:  This is not exactly right, but I'm at my wits end trying to
            //      figure it out.  Feel free to fix it.  :-)
            // MB: it's much easier than you think, mTime isn't warped at all!
            time -= mT1 - mT0;
         }
      }
   }

   return time;
}

void PlaybackSchedule::TrackTimeUpdate(double realElapsed)
{
   // Update mTime within the PortAudio callback

   if (Interactive())
      return;

   auto time = GetTrackTime();
   auto newTime = AdvancedTrackTime( time, realElapsed, 1.0 );
   SetTrackTime( newTime );
}

double PlaybackSchedule::RealDuration(double trackTime1) const
{
   double duration;
   if (mEnvelope)
      duration = ComputeWarpedLength(*mEnvelope, mT0, trackTime1);
   else
      duration = trackTime1 - mT0;
   return fabs(duration);
}

double PlaybackSchedule::RealTimeRemaining() const
{
   return mWarpedLength - mWarpedTime;
}

void PlaybackSchedule::RealTimeAdvance( double increment )
{
   mWarpedTime += increment;
}

void PlaybackSchedule::RealTimeInit( double trackTime )
{
   if (Scrubbing())
      mWarpedTime = 0.0;
   else
      mWarpedTime = RealDuration( trackTime );
}

void PlaybackSchedule::RealTimeRestart()
{
   mWarpedTime = 0;
}

double RecordingSchedule::ToConsume() const
{
   return mDuration - Consumed();
}

double RecordingSchedule::Consumed() const
{
   return std::max( 0.0, mPosition + TotalCorrection() );
}

double RecordingSchedule::ToDiscard() const
{
   return std::max(0.0, -( mPosition + TotalCorrection() ) );
}
