/**********************************************************************

Audacity: A Digital Audio Editor

AudioIOBase.h

Paul Licameli split from AudioIO.h

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_BASE__
#define __AUDACITY_AUDIO_IO_BASE__




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
// Windows build needs complete type for parameter of wxWeakRef
// class MeterPanelBase;
#include "widgets/MeterPanelBase.h"
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

protected:
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

#include "Prefs.h"

extern AUDACITY_DLL_API StringSetting AudioIOHost;
extern AUDACITY_DLL_API DoubleSetting AudioIOLatencyCorrection;
extern AUDACITY_DLL_API DoubleSetting AudioIOLatencyDuration;
extern AUDACITY_DLL_API StringSetting AudioIOPlaybackDevice;
extern AUDACITY_DLL_API IntSetting    AudioIORecordChannels;
extern AUDACITY_DLL_API StringSetting AudioIORecordingDevice;
extern AUDACITY_DLL_API StringSetting AudioIORecordingSource;
extern AUDACITY_DLL_API IntSetting    AudioIORecordingSourceIndex;
