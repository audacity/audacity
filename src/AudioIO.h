/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.h

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO__
#define __AUDACITY_AUDIO_IO__

#include "Audacity.h" // for USE_* macros

#include "Experimental.h"

#include "portaudio.h"

#include "MemoryX.h"
#include <atomic>
#include <utility>
#include <vector>
#include <wx/atomic.h>
#include <wx/weakref.h>

#ifdef USE_MIDI

// TODO: Put the relative paths into automake.

#ifdef EXPERIMENTAL_MIDI_OUT
#include "../lib-src/portmidi/pm_common/portmidi.h"
#include "../lib-src/portmidi/porttime/porttime.h"
#include <cstring> // Allegro include fails if this header isn't included do to no memcpy
#include "../lib-src/header-substitutes/allegro.h"

class NoteTrack;
using NoteTrackArray = std::vector < std::shared_ptr< NoteTrack > >;
using NoteTrackConstArray = std::vector < std::shared_ptr< const NoteTrack > >;

#endif // EXPERIMENTAL_MIDI_OUT

#endif // USE_MIDI

#if USE_PORTMIXER
#include "../lib-src/portmixer/include/portmixer.h"
#endif

#include <wx/event.h> // to declare custom event types
#include <wx/thread.h>

#include "SampleFormat.h"

class wxArrayString;
class AudioIO;
class RingBuffer;
class Mixer;
class Resample;
class TimeTrack;
class AudioThread;
class MeterPanel;
class SelectedRegion;

class AudacityProject;

class WaveTrack;
using WaveTrackArray = std::vector < std::shared_ptr < WaveTrack > >;
using WaveTrackConstArray = std::vector < std::shared_ptr < const WaveTrack > >;

extern AUDACITY_DLL_API AudioIO *gAudioIO;

void InitAudioIO();
void DeinitAudioIO();
wxString DeviceName(const PaDeviceInfo* info);
wxString HostName(const PaDeviceInfo* info);
bool ValidateDeviceNames();

class AudioIOListener;

// #include <cfloat> if you need this constant
#define BAD_STREAM_TIME (-DBL_MAX)

#define MAX_MIDI_BUFFER_SIZE 5000
#define DEFAULT_SYNTH_LATENCY 5

#define DEFAULT_LATENCY_DURATION 100.0
#define DEFAULT_LATENCY_CORRECTION -130.0

#define AUDIO_PRE_ROLL_KEY (wxT("/AudioIO/PreRoll"))
#define DEFAULT_PRE_ROLL_SECONDS 5.0

#define AUDIO_ROLL_CROSSFADE_KEY (wxT("/AudioIO/Crossfade"))
#define DEFAULT_ROLL_CROSSFADE_MS 10.0

#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   #define AILA_DEF_TARGET_PEAK 92
   #define AILA_DEF_DELTA_PEAK 2
   #define AILA_DEF_ANALYSIS_TIME 1000
   #define AILA_DEF_NUMBER_ANALYSIS 5
#endif

wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_AUDIOIO_PLAYBACK, wxCommandEvent);
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_AUDIOIO_CAPTURE, wxCommandEvent);
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_AUDIOIO_MONITOR, wxCommandEvent);

// PRL:
// If we always run a portaudio output stream (even just to produce silence)
// whenever we play Midi, then we might use just one thread for both.
// I thought this would improve MIDI synch problems on Linux/ALSA, but RBD
// convinced me it was neither a necessary nor sufficient fix.  Perhaps too the
// MIDI thread might block in some error situations but we should then not
// also block the audio thread.
// So leave the separate thread ENABLED.
#define USE_MIDI_THREAD

struct ScrubbingOptions;

using PRCrossfadeData = std::vector< std::vector < float > >;

// To avoid growing the argument list of StartStream, add fields here
struct AudioIOStartStreamOptions
{
   explicit
   AudioIOStartStreamOptions(double rate_)
      : timeTrack(NULL)
      , listener(NULL)
      , rate(rate_)
      , playLooped(false)
      , cutPreviewGapStart(0.0)
      , cutPreviewGapLen(0.0)
      , pStartTime(NULL)
      , preRoll(0.0)
   {}

   TimeTrack *timeTrack;
   AudioIOListener* listener;
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
};

struct TransportTracks {
   WaveTrackArray playbackTracks;
   WaveTrackArray captureTracks;
#ifdef EXPERIMENTAL_MIDI_OUT
   NoteTrackConstArray midiTracks;
#endif

   // This is a subset of playbackTracks
   WaveTrackConstArray prerollTracks;
};

// This workaround makes pause and stop work when output is to GarageBand,
// which seems not to implement the notes-off message correctly.
#define AUDIO_IO_GB_MIDI_WORKAROUND

/** brief The function which is called from PortAudio's callback thread
 * context to collect and deliver audio for / from the sound device.
 *
 * This covers recording, playback, and doing both simultaneously. It is
 * also invoked to do monitoring and software playthrough. Note that dealing
 * with the two buffers needs some care to ensure that the right things
 * happen for all possible cases.
 * @param inputBuffer Buffer of length framesPerBuffer containing samples
 * from the sound card, or null if not capturing audio. Note that the data
 * type will depend on the format of audio data that was chosen when the
 * stream was created (so could be floats or various integers)
 * @param outputBuffer Uninitialised buffer of length framesPerBuffer which
 * will be sent to the sound card after the callback, or null if not playing
 * audio back.
 * @param framesPerBuffer The length of the playback and recording buffers
 * @param PaStreamCallbackTimeInfo Pointer to PortAudio time information
 * structure, which tells us how long we have been playing / recording
 * @param statusFlags PortAudio stream status flags
 * @param userData pointer to user-defined data structure. Provided for
 * flexibility by PortAudio, but not used by Audacity - the data is stored in
 * the AudioIO class instead.
 */
int audacityAudioCallback(
   const void *inputBuffer, void *outputBuffer,
   unsigned long framesPerBuffer,
   const PaStreamCallbackTimeInfo *timeInfo,
   PaStreamCallbackFlags statusFlags, void *userData );

// Communicate data from one writer to one reader.
// This is not a queue: it is not necessary for each write to be read.
// Rather loss of a message is allowed:  writer may overwrite.
// Data must be default-constructible and either copyable or movable.
template<typename Data>
class MessageBuffer {
   struct alignas( 64
      //std::hardware_destructive_interference_size // C++17
   ) UpdateSlot {
      std::atomic<bool> mBusy{ false };
      Data mData;
   } mSlots[2];

   std::atomic<unsigned char> mLastWrittenSlot{ 0 };

public:
   void Initialize();

   // Move data out (if available), or else copy it out
   Data Read();
   
   // Copy data in
   void Write( const Data &data );
   // Move data in
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

class AUDACITY_DLL_API AudioIoCallback {
public:
   AudioIoCallback();
   ~AudioIoCallback();

public:
   // This function executes in a thread spawned by the PortAudio library
   int AudioCallback(
      const void *inputBuffer, void *outputBuffer,
      unsigned long framesPerBuffer,
      const PaStreamCallbackTimeInfo *timeInfo,
      const PaStreamCallbackFlags statusFlags, void *userData);

   // Part of the callback
   PaStreamCallbackResult CallbackDoSeek();

   // Part of the callback
   void CallbackCheckCompletion(
      int &callbackReturn, unsigned long len);

   int mbHasSoloTracks;
   int mCallbackReturn;
   // Helpers to determine if tracks have already been faded out.
   unsigned  CountSoloingTracks();
   bool TrackShouldBeSilent( const WaveTrack &wt );
   bool TrackHasBeenFadedOut( const WaveTrack &wt );
   bool AllTracksAlreadySilent();

   // These eight functions do different parts of AudioCallback().
   void ComputeMidiTimings(
      const PaStreamCallbackTimeInfo *timeInfo,
      unsigned long framesPerBuffer);
   void CheckSoundActivatedRecordingLevel(const void *inputBuffer);
   void AddToOutputChannel( unsigned int chan,
      float * outputMeterFloats,
      float * outputFloats,
      float * tempFloats,
      float * tempBuf,
      bool drop,
      unsigned long len,
      WaveTrack *vt
      );
   bool FillOutputBuffers(
      void *outputBuffer,
      unsigned long framesPerBuffer,
      float * tempFloats, float *outputMeterFloats
   );
   void FillInputBuffers(
      const void *inputBuffer, 
      unsigned long framesPerBuffer,
      const PaStreamCallbackFlags statusFlags,
      float * tempFloats
   );
   void UpdateTimePosition(
      unsigned long framesPerBuffer
   );
   void DoPlaythrough(
      const void *inputBuffer, 
      void *outputBuffer,
      unsigned long framesPerBuffer,
      float *outputMeterFloats
   );
   void SendVuInputMeterData(
      float *tempFloats,
      const void *inputBuffer,
      unsigned long framesPerBuffer
   );
   void SendVuOutputMeterData(
      float *outputMeterFloats,
      unsigned long framesPerBuffer
   );


// Required by these functions...
#ifdef EXPERIMENTAL_MIDI_OUT
   double AudioTime() { return mPlaybackSchedule.mT0 + mNumFrames / mRate; }
#endif

   /** \brief Find out if playback / recording is currently paused */
   bool IsPaused() const;


   /** \brief Get the number of audio samples ready in all of the playback
   * buffers.
   *
   * Returns the smallest of the buffer ready space values in the event that
   * they are different. */
   size_t GetCommonlyReadyPlayback();


#ifdef EXPERIMENTAL_MIDI_OUT
   //   MIDI_PLAYBACK:
   PmStream        *mMidiStream;
   PmError          mLastPmError;

   /// Latency of MIDI synthesizer
   long             mSynthLatency; // ms

   // These fields are used to synchronize MIDI with audio:

   /// PortAudio's clock time
   volatile double  mAudioCallbackClockTime;

   /// Number of frames output, including pauses
   volatile long    mNumFrames;
   /// How many frames of zeros were output due to pauses?
   volatile long    mNumPauseFrames;
   /// total of backward jumps
   volatile int     mMidiLoopPasses;
   inline double MidiLoopOffset() {
      return mMidiLoopPasses * (mPlaybackSchedule.mT1 - mPlaybackSchedule.mT0);
   }

   volatile long    mAudioFramesPerBuffer;
   /// Used by Midi process to record that pause has begun,
   /// so that AllNotesOff() is only delivered once
   volatile bool    mMidiPaused;
   /// The largest timestamp written so far, used to delay
   /// stream closing until last message has been delivered
   PmTimestamp mMaxMidiTimestamp;

   /// Offset from ideal sample computation time to system time,
   /// where "ideal" means when we would get the callback if there
   /// were no scheduling delays or computation time
   double mSystemMinusAudioTime;
   /// audio output latency reported by PortAudio
   /// (initially; for Alsa, we adjust it to the largest "observed" value)
   double mAudioOutLatency;

   // Next two are used to adjust the previous two, if
   // PortAudio does not provide the info (using ALSA):

   /// time of first callback
   /// used to find "observed" latency
   double mStartTime;
   /// number of callbacks since stream start
   long mCallbackCount;

   /// Make just one variable to communicate from audio to MIDI thread,
   /// to avoid problems of atomicity of updates
   volatile double mSystemMinusAudioTimePlusLatency;

   Alg_seq_ptr      mSeq;
   std::unique_ptr<Alg_iterator> mIterator;
   /// The next event to play (or null)
   Alg_event_ptr    mNextEvent;

#ifdef AUDIO_IO_GB_MIDI_WORKAROUND
   std::vector< std::pair< int, int > > mPendingNotesOff;
#endif

   /// Real time at which the next event should be output, measured in seconds.
   /// Note that this could be a note's time+duration for note offs.
   double           mNextEventTime;
   /// Track of next event
   NoteTrack        *mNextEventTrack;
   /// True when output reaches mT1
   bool             mMidiOutputComplete{ true };
   /// Is the next event a note-on?
   bool             mNextIsNoteOn;
   /// mMidiStreamActive tells when mMidiStream is open for output
   bool             mMidiStreamActive;
   /// when true, mSendMidiState means send only updates, not note-on's,
   /// used to send state changes that precede the selected notes
   bool             mSendMidiState;
   NoteTrackConstArray mMidiPlaybackTracks;
#endif

#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   bool           mAILAActive;
   bool           mAILAClipped;
   int            mAILATotalAnalysis;
   int            mAILAAnalysisCounter;
   double         mAILAMax;
   double         mAILAGoalPoint;
   double         mAILAGoalDelta;
   double         mAILAAnalysisTime;
   double         mAILALastStartTime;
   double         mAILAChangeFactor;
   double         mAILATopLevel;
   double         mAILAAnalysisEndTime;
   double         mAILAAbsolutStartTime;
   unsigned short mAILALastChangeType;  //0 - no change, 1 - increase change, 2 - decrease change
#endif

   std::unique_ptr<AudioThread> mThread;
#ifdef EXPERIMENTAL_MIDI_OUT
#ifdef USE_MIDI_THREAD
   std::unique_ptr<AudioThread> mMidiThread;
#endif
#endif
   ArrayOf<std::unique_ptr<Resample>> mResample;
   ArrayOf<std::unique_ptr<RingBuffer>> mCaptureBuffers;
   WaveTrackArray      mCaptureTracks;
   ArrayOf<std::unique_ptr<RingBuffer>> mPlaybackBuffers;
   WaveTrackArray      mPlaybackTracks;

   ArrayOf<std::unique_ptr<Mixer>> mPlaybackMixers;
   volatile int        mStreamToken;
   static int          mNextStreamToken;
   double              mFactor;
   /// Audio playback rate in samples per second
   double              mRate;
   unsigned long       mMaxFramesOutput; // The actual number of frames output.
   bool                mbMicroFades; 

   double              mSeek;
   double              mPlaybackRingBufferSecs;
   double              mCaptureRingBufferSecs;

   /// Preferred batch size for replenishing the playback RingBuffer
   size_t              mPlaybackSamplesToCopy;
   /// Occupancy of the queue we try to maintain, with bigger batches if needed
   size_t              mPlaybackQueueMinimum;

   double              mMinCaptureSecsToCopy;
   /// True if audio playback is paused
   bool                mPaused;
   PaStream           *mPortStreamV19;
   bool                mSoftwarePlaythrough;
   /// True if Sound Activated Recording is enabled
   bool                mPauseRec;
   float               mSilenceLevel;
   unsigned int        mNumCaptureChannels;
   unsigned int        mNumPlaybackChannels;
   sampleFormat        mCaptureFormat;
   unsigned long long  mLostSamples{ 0 };
   volatile bool       mAudioThreadShouldCallFillBuffersOnce;
   volatile bool       mAudioThreadFillBuffersLoopRunning;
   volatile bool       mAudioThreadFillBuffersLoopActive;

   wxLongLong          mLastPlaybackTimeMillis;

#ifdef EXPERIMENTAL_MIDI_OUT
   volatile bool       mMidiThreadFillBuffersLoopRunning;
   volatile bool       mMidiThreadFillBuffersLoopActive;
#endif

   volatile double     mLastRecordingOffset;
   PaError             mLastPaError;

protected:

   AudacityProject    *mOwningProject;
   wxWeakRef<MeterPanel> mInputMeter{};
   MeterPanel         *mOutputMeter;
   bool                mUpdateMeters;
   volatile bool       mUpdatingMeters;

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

   AudioIOListener*    mListener;

   friend class AudioThread;
#ifdef EXPERIMENTAL_MIDI_OUT
   friend class MidiThread;
#endif

   friend void InitAudioIO();

   bool mUsingAlsa { false };

   // For cacheing supported sample rates
   static int mCachedPlaybackIndex;
   static std::vector<long> mCachedPlaybackRates;
   static int mCachedCaptureIndex;
   static std::vector<long> mCachedCaptureRates;
   static std::vector<long> mCachedSampleRates;
   static double mCachedBestRateIn;
   static double mCachedBestRateOut;
   static bool mCachedBestRatePlaying;
   static bool mCachedBestRateCapturing;

   // Serialize main thread and PortAudio thread's attempts to pause and change
   // the state used by the third, Audio thread.
   wxMutex mSuspendAudioThread;

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
public:
   struct ScrubState;
   std::unique_ptr<ScrubState> mScrubState;

   bool mSilentScrub;
   double mScrubSpeed;
   sampleCount mScrubDuration;
#endif

protected:
   // A flag tested and set in one thread, cleared in another.  Perhaps
   // this guarantee of atomicity is more cautious than necessary.
   wxAtomicInt mRecordingException {};
   void SetRecordingException()
      { wxAtomicInc( mRecordingException ); }
   void ClearRecordingException()
      { if (mRecordingException) wxAtomicDec( mRecordingException ); }

   std::vector< std::pair<double, double> > mLostCaptureIntervals;
   bool mDetectDropouts{ true };

public:
   // Pairs of starting time and duration
   const std::vector< std::pair<double, double> > &LostCaptureIntervals()
   { return mLostCaptureIntervals; }

   // Used only for testing purposes in alpha builds
   bool mSimulateRecordingErrors{ false };

   // Whether to check the error code passed to audacityAudioCallback to
   // detect more dropouts
   bool mDetectUpstreamDropouts{ true };

protected:
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
   } mRecordingSchedule{};

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
      
      const TimeTrack *mTimeTrack;
      
      volatile enum {
         PLAY_STRAIGHT,
         PLAY_LOOPED,
#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
         PLAY_SCRUB,
         PLAY_AT_SPEED, // a version of PLAY_SCRUB.
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
      bool Scrubbing() const       { return mPlayMode == PLAY_SCRUB; }
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

      // Convert a nonnegative real duration to an increment of track time
      // relative to mT0.
      double TrackDuration(double realElapsed) const;

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

   // Another circular buffer
   // Holds track time values corresponding to every nth sample in the playback
   // buffers, for some large n
   struct TimeQueue {
      Doubles mData;
      size_t mSize{ 0 };
      double mLastTime {};
      // These need not be updated atomically, because we rely on the atomics
      // in the playback ring buffers to supply the synchronization.  Still,
      // align them to avoid false sharing.
      alignas(64) struct Cursor {
         size_t mIndex {};
         size_t mRemainder {};
      } mHead, mTail;

      void Producer(
         const PlaybackSchedule &schedule, double rate, double scrubSpeed,
         size_t nSamples );
      double Consumer( size_t nSamples, double rate );
   } mTimeQueue;

};

class AUDACITY_DLL_API AudioIO final : public AudioIoCallback {

 public:
   AudioIO();
   ~AudioIO();


public:

   AudioIOListener* GetListener() { return mListener; }
   void SetListener(AudioIOListener* listener);

   /** \brief Start up Portaudio for capture and recording as needed for
    * input monitoring and software playthrough only
    *
    * This uses the Default project sample format, current sample rate, and
    * selected number of input channels to open the recording device and start
    * reading input data. If software playthrough is enabled, it also opens
    * the output device in stereo to play the data through */
   void StartMonitoring(double sampleRate);

   /** \brief Start recording or playing back audio
    *
    * Allocates buffers for recording and playback, gets the Audio thread to
    * fill them, and sets the stream rolling.
    * If successful, returns a token identifying this particular stream
    * instance.  For use with IsStreamActive() below */

   int StartStream(const TransportTracks &tracks,
                   double t0, double t1,
                   const AudioIOStartStreamOptions &options);

   /** \brief Stop recording, playback or input monitoring.
    *
    * Does quite a bit of housekeeping, including switching off monitoring,
    * flushing recording buffers out to wave tracks, and applies latency
    * correction to recorded tracks if necessary */
   void StopStream();
   /** \brief Move the playback / recording position of the current stream
    * by the specified amount from where it is now */
   void SeekStream(double seconds) { mSeek = seconds; }

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   bool IsScrubbing() const { return IsBusy() && mScrubState != 0; }

   /** \brief Notify scrubbing engine of desired position or speed.
   * If options.adjustStart is true, then when mouse movement exceeds maximum
   * scrub speed, adjust the beginning of the scrub interval rather than the
   * end, so that the scrub skips or "stutters" to stay near the cursor.
   */
   void UpdateScrub(double endTimeOrSpeed, const ScrubbingOptions &options);

   void StopScrub();

   /** \brief return the ending time of the last scrub interval.
   */
   double GetLastScrubTime() const;
#endif

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

public:
   wxString LastPaErrorString();

   wxLongLong GetLastPlaybackTime() const { return mLastPlaybackTimeMillis; }
   AudacityProject *GetOwningProject() const { return mOwningProject; }

#ifdef EXPERIMENTAL_MIDI_OUT
   /** \brief Compute the current PortMidi timestamp time.
    *
    * This is used by PortMidi to synchronize midi time to audio samples
    */
   PmTimestamp MidiTime();

   // Note: audio code solves the problem of soloing/muting tracks by scanning
   // all playback tracks on every call to the audio buffer fill routine.
   // We do the same for Midi, but it seems wasteful for at least two
   // threads to be frequently polling to update status. This could be
   // eliminated (also with a reduction in code I think) by updating mHasSolo
   // each time a solo button is activated or deactivated. For now, I'm
   // going to do this polling in the FillMidiBuffer routine to localize
   // changes for midi to the midi code, but I'm declaring the variable
   // here so possibly in the future, Audio code can use it too. -RBD
 private:
   bool  mHasSolo; // is any playback solo button pressed?
 public:
   bool SetHasSolo(bool hasSolo);
   bool GetHasSolo() { return mHasSolo; }
#endif

   /** \brief Returns true if the stream is active, or even if audio I/O is
    * busy cleaning up its data or writing to disk.
    *
    * This is used by TrackPanel to determine when a track has been completely
    * recorded, and it's safe to flush to disk. */
   bool IsAudioTokenActive(int token) const;

   /** \brief Returns true if we're monitoring input (but not recording or
    * playing actual audio) */
   bool IsMonitoring() const;

   /** \brief Pause and un-pause playback and recording */
   void SetPaused(bool state);

   /* Mixer services are always available.  If no stream is running, these
    * methods use whatever device is specified by the preferences.  If a
    * stream *is* running, naturally they manipulate the mixer associated
    * with that stream.  If no mixer is available, output is emulated and
    * input is stuck at 1.0f (a gain is applied to output samples).
    */
   void SetMixer(int inputSource);
   void SetMixer(int inputSource, float inputVolume,
                 float playbackVolume);
   void GetMixer(int *inputSource, float *inputVolume,
                 float *playbackVolume);
   /** @brief Find out if the input hardware level control is available
    *
    * Checks the mInputMixerWorks variable, which is set up in
    * AudioIO::HandleDeviceChange(). External people care, because we want to
    * disable the UI if it doesn't work.
    */
   bool InputMixerWorks();

   /** @brief Find out if the output level control is being emulated via software attenuation
    *
    * Checks the mEmulateMixerOutputVol variable, which is set up in
    * AudioIO::HandleDeviceChange(). External classes care, because we want to
    * modify the UI if it doesn't work.
    */
   bool OutputMixerEmulated();

   /** \brief Get the list of inputs to the current mixer device
    *
    * Returns an array of strings giving the names of the inputs to the
    * soundcard mixer (driven by PortMixer) */
   wxArrayString GetInputSourceNames();

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
    * You can explicitely give the index of the device.  If you don't
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
    * You can explicitely give the index of the device.  If you don't
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
    * You can explicitely give the indexes of the playDevice/recDevice.
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

   sampleFormat GetCaptureFormat() { return mCaptureFormat; }
   unsigned GetNumPlaybackChannels() const { return mNumPlaybackChannels; }
   unsigned GetNumCaptureChannels() const { return mNumCaptureChannels; }

   // Meaning really capturing, not just pre-rolling
   bool IsCapturing() const;

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

   /** \brief Ensure selected device names are valid
    *
    */
   static bool ValidateDeviceNames(const wxString &play, const wxString &rec);

   /** \brief Function to automatically set an acceptable volume
    *
    */
   #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
      void AILAInitialize();
      void AILADisable();
      bool AILAIsActive();
      void AILAProcess(double maxPeak);
      void AILASetStartTime();
      double AILAGetLastDecisionTime();
   #endif

   bool IsAvailable(AudacityProject *projecT) const;
   void SetCaptureMeter(AudacityProject *project, MeterPanel *meter);
   void SetPlaybackMeter(AudacityProject *project, MeterPanel *meter);

   /** \brief Return a valid sample rate that is supported by the current I/O
   * device(s).
   *
   * The return from this function is used to determine the sample rate that
   * audacity actually runs the audio I/O stream at. if there is no suitable
   * rate available from the hardware, it returns 0.
   * The sampleRate argument gives the desired sample rate (the rate of the
   * audio to be handeled, i.e. the currently Project Rate).
   * capturing is true if the stream is capturing one or more audio channels,
   * and playing is true if one or more channels are being played. */
   double GetBestRate(bool capturing, bool playing, double sampleRate);

   friend class AudioThread;
#ifdef EXPERIMENTAL_MIDI_OUT
   friend class MidiThread;
#endif

   friend void InitAudioIO();




private:
   /** \brief Set the current VU meters - this should be done once after
    * each call to StartStream currently */
   void SetMeters();


   /** \brief Opens the portaudio stream(s) used to do playback or recording
    * (or both) through.
    *
    * The sampleRate passed is the Project Rate of the active project. It may
    * or may not be actually supported by playback or recording hardware
    * currently in use (for many reasons). The number of Capture and Playback
    * channels requested includes an allocation for doing software playthrough
    * if necessary. The captureFormat is used for recording only, the playback
    * being floating point always. Returns true if the stream opened sucessfully
    * and false if it did not. */
   bool StartPortAudioStream(double sampleRate,
                             unsigned int numPlaybackChannels,
                             unsigned int numCaptureChannels,
                             sampleFormat captureFormat);
   void FillBuffers();

#ifdef EXPERIMENTAL_MIDI_OUT
   void PrepareMidiIterator(bool send = true, double offset = 0);
   bool StartPortMidiStream();

   // Compute nondecreasing real time stamps, accounting for pauses, but not the
   // synth latency.
   double UncorrectedMidiEventTime();

   void OutputEvent();
   void FillMidiBuffers();
   void GetNextEvent();
   double PauseTime();
   void AllNotesOff(bool looping = false);
#endif

   /** \brief Get the number of audio samples free in all of the playback
   * buffers.
   *
   * Returns the smallest of the buffer free space values in the event that
   * they are different. */
   size_t GetCommonlyFreePlayback();

   /** \brief Get the number of audio samples ready in all of the recording
    * buffers.
    *
    * Returns the smallest of the number of samples available for storage in
    * the recording buffers (i.e. the number of samples that can be read from
    * all record buffers without underflow). */
   size_t GetCommonlyAvailCapture();

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

   /** \brief Allocate RingBuffer structures, and others, needed for playback
     * and recording.
     *
     * Returns true iff successful.
     */
   bool AllocateBuffers(
      const AudioIOStartStreamOptions &options,
      const TransportTracks &tracks, double t0, double t1, double sampleRate,
      bool scrubbing );

   /** \brief Clean up after StartStream if it fails.
     *
     * If bOnlyBuffers is specified, it only cleans up the buffers. */
   void StartStreamCleanup(bool bOnlyBuffers = false);
};

#endif
