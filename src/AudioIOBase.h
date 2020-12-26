/**********************************************************************

Audacity: A Digital Audio Editor

AudioIOBase.h

Paul Licameli split from AudioIO.h

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_BASE__
#define __AUDACITY_AUDIO_IO_BASE__

#include "Audacity.h" // for USE_* macros
#include "Experimental.h"

#include <atomic>
#include <cfloat>
#include <functional>
#include <memory>
#include <vector>
#include <wx/string.h>
#include <wx/weakref.h> // member variable

struct PaDeviceInfo;
typedef void PaStream;

#if USE_PORTMIXER
typedef void PxMixer;
#endif

class AudioIOBase;

class AudacityProject;
class AudioIOListener;
class BoundedEnvelope;
class MeterPanelBase;
using PRCrossfadeData = std::vector< std::vector < float > >;

#define BAD_STREAM_TIME (-DBL_MAX)

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

// To avoid growing the argument list of StartStream, add fields here
struct AudioIOStartStreamOptions
{
   explicit
   AudioIOStartStreamOptions(AudacityProject *pProject_, double rate_)
      : pProject{ pProject_ }
      , envelope(nullptr)
      , rate(rate_)
      , playLooped(false)
      , cutPreviewGapStart(0.0)
      , cutPreviewGapLen(0.0)
      , pStartTime(NULL)
      , preRoll(0.0)
   {}

   AudacityProject *pProject{};
   MeterPanelBase *captureMeter{}, *playbackMeter{};
   const BoundedEnvelope *envelope; // for time warping
   std::shared_ptr< AudioIOListener > listener;
   double rate;
   bool playLooped;
   double cutPreviewGapStart;
   double cutPreviewGapLen;
   double * pStartTime;
   double preRoll;

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   // Non-null value indicates that scrubbing will happen
   // (do not specify a time track, looping, or recording, which
   //  are all incompatible with scrubbing):
   ScrubbingOptions *pScrubbingOptions {};
#endif

   // contents may get swapped with empty vector
   PRCrossfadeData      *pCrossfadeData{};

   // An unfortunate thing needed just to make scrubbing work on Linux when
   // we can't use a separate polling thread.
   // The return value is a number of milliseconds to sleep before calling again
   std::function< unsigned long() > playbackStreamPrimer;
};

///\brief A singleton object supporting queries of the state of any active
/// audio streams, and audio device capabilities
class AUDACITY_DLL_API AudioIOBase /* not final */
{
public:
   static AudioIOBase *Get();

   virtual ~AudioIOBase();

   void SetCaptureMeter(AudacityProject *project, MeterPanelBase *meter);
   void SetPlaybackMeter(AudacityProject *project, MeterPanelBase *meter);

   /** \brief update state after changing what audio devices are selected
    *
    * Called when the devices stored in the preferences are changed to update
    * the audio mixer capabilities
    *
    * \todo: Make this do a sample rate query and store the result in the
    * AudioIO object to avoid doing it later? Would simplify the
    * GetSupported*Rate functions considerably */
   void HandleDeviceChange();

   /** \brief Get a list of sample rates the output (playback) device
    * supports.
    *
    * If no information about available sample rates can be fetched,
    * an empty list is returned.
    *
    * You can explicitly give the index of the device.  If you don't
    * give it, the currently selected device from the preferences will be used.
    *
    * You may also specify a rate for which to check in addition to the
    * standard rates.
    */
   static std::vector<long> GetSupportedPlaybackRates(int DevIndex = -1,
                                                double rate = 0.0);

   /** \brief Get a list of sample rates the input (recording) device
    * supports.
    *
    * If no information about available sample rates can be fetched,
    * an empty list is returned.
    *
    * You can explicitly give the index of the device.  If you don't
    * give it, the currently selected device from the preferences will be used.
    *
    * You may also specify a rate for which to check in addition to the
    * standard rates.
    */
   static std::vector<long> GetSupportedCaptureRates(int devIndex = -1,
                                               double rate = 0.0);

   /** \brief Get a list of sample rates the current input/output device
    * combination supports.
    *
    * Since there is no concept (yet) for different input/output
    * sample rates, this currently returns only sample rates that are
    * supported on both the output and input device. If no information
    * about available sample rates can be fetched, it returns a default
    * list.
    * You can explicitly give the indexes of the playDevice/recDevice.
    * If you don't give them, the selected devices from the preferences
    * will be used.
    * You may also specify a rate for which to check in addition to the
    * standard rates.
    */
   static std::vector<long> GetSupportedSampleRates(int playDevice = -1,
                                              int recDevice = -1,
                                       double rate = 0.0);

   /** \brief Get a supported sample rate which can be used a an optimal
    * default.
    *
    * Currently, this uses the first supported rate in the list
    * [44100, 48000, highest sample rate]. Used in Project as a default value
    * for project rates if one cannot be retrieved from the preferences.
    * So all in all not that useful or important really
    */
   static int GetOptimalSupportedSampleRate();

   /** \brief During playback, the track time most recently played
    *
    * When playing looped, this will start from t0 again,
    * too. So the returned time should be always between
    * t0 and t1
    */
   double GetStreamTime();

   /** \brief Array of common audio sample rates
    *
    * These are the rates we will always support, regardless of hardware support
    * for them (by resampling in audacity if needed) */
   static const int StandardRates[];
   /** \brief How many standard sample rates there are */
   static const int NumStandardRates;

   /** \brief Get diagnostic information on all the available audio I/O devices
    *
    */
   wxString GetDeviceInfo();

#ifdef EXPERIMENTAL_MIDI_OUT
   /** \brief Get diagnostic information on all the available MIDI I/O devices */
   wxString GetMidiDeviceInfo();
#endif

   /** \brief Find out if playback / recording is currently paused */
   bool IsPaused() const;

   virtual void StopStream() = 0;

   /** \brief  Returns true if audio i/o is busy starting, stopping, playing,
    * or recording.
    *
    * When this is false, it's safe to start playing or recording */
   bool IsBusy() const;

   /** \brief Returns true if the audio i/o is running at all, but not during
    * cleanup
    *
    * Doesn't return true if the device has been closed but some disk i/o or
    * cleanup is still going on. If you want to know if it's safe to start a
    * NEW stream, use IsBusy() */
   bool IsStreamActive() const;
   bool IsStreamActive(int token) const;

   /** \brief Returns true if the stream is active, or even if audio I/O is
    * busy cleaning up its data or writing to disk.
    *
    * This is used by TrackPanel to determine when a track has been completely
    * recorded, and it's safe to flush to disk. */
   bool IsAudioTokenActive(int token) const;

   /** \brief Returns true if we're monitoring input (but not recording or
    * playing actual audio) */
   bool IsMonitoring() const;

   /* Mixer services are always available.  If no stream is running, these
    * methods use whatever device is specified by the preferences.  If a
    * stream *is* running, naturally they manipulate the mixer associated
    * with that stream.  If no mixer is available, output is emulated and
    * input is stuck at 1.0f (a gain is applied to output samples).
    */
   void SetMixer(int inputSource);

protected:
   static std::unique_ptr<AudioIOBase> ugAudioIO;
   static wxString DeviceName(const PaDeviceInfo* info);
   static wxString HostName(const PaDeviceInfo* info);

   AudacityProject    *mOwningProject;

   /// True if audio playback is paused
   bool                mPaused;

   /// True when output reaches mT1
   bool             mMidiOutputComplete{ true };

   /// mMidiStreamActive tells when mMidiStream is open for output
   bool             mMidiStreamActive;

   volatile int        mStreamToken;

   /// Audio playback rate in samples per second
   double              mRate;

   PaStream           *mPortStreamV19;

   wxWeakRef<MeterPanelBase> mInputMeter{};
   wxWeakRef<MeterPanelBase> mOutputMeter{};

   #if USE_PORTMIXER
   PxMixer            *mPortMixer;
   float               mPreviousHWPlaythrough;
   #endif /* USE_PORTMIXER */

   bool                mEmulateMixerOutputVol;
   /** @brief Can we control the hardware input level?
    *
    * This flag is set to true if using portmixer to control the
    * input volume seems to be working (and so we offer the user the control),
    * and to false (locking the control out) otherwise. This avoids stupid
    * scaled clipping problems when trying to do software emulated input volume
    * control */
   bool                mInputMixerWorks;
   float               mMixerOutputVol;

   // For cacheing supported sample rates
   static int mCachedPlaybackIndex;
   static std::vector<long> mCachedPlaybackRates;
   static int mCachedCaptureIndex;
   static std::vector<long> mCachedCaptureRates;
   static std::vector<long> mCachedSampleRates;
   static double mCachedBestRateIn;

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

   struct PlaybackSchedule {
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

   } mPlaybackSchedule;

   /** \brief get the index of the supplied (named) recording device, or the
    * device selected in the preferences if none given.
    *
    * Pure utility function, but it comes round a number of times in the code
    * and would be neater done once. If the device isn't found, return the
    * default device index.
    */
   static int getRecordDevIndex(const wxString &devName = {});

   /** \brief get the index of the device selected in the preferences.
    *
    * If the device isn't found, returns -1
    */
#if USE_PORTMIXER
   static int getRecordSourceIndex(PxMixer *portMixer);
#endif

   /** \brief get the index of the supplied (named) playback device, or the
    * device selected in the preferences if none given.
    *
    * Pure utility function, but it comes round a number of times in the code
    * and would be neater done once. If the device isn't found, return the
    * default device index.
    */
   static int getPlayDevIndex(const wxString &devName = {});

   /** \brief Array of audio sample rates to try to use
    *
    * These are the rates we will check if a device supports, and is as long
    * as I can think of (to try and work out what the card can do) */
   static const int RatesToTry[];
   /** \brief How many sample rates to try */
   static const int NumRatesToTry;
};

#endif
