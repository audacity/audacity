/**********************************************************************

Audacity: A Digital Audio Editor

AudioIOBase.h

Paul Licameli split from AudioIO.h

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_BASE__
#define __AUDACITY_AUDIO_IO_BASE__




#include <cfloat>
#include <functional>
#include <vector>
#include <wx/string.h>
#include "MemoryX.h"

struct PaDeviceInfo;
typedef void PaStream;

#if USE_PORTMIXER
typedef void PxMixer;
#endif

class AudioIOBase;

class AudacityProject;
class AudioIOListener;
class BoundedEnvelope;
class Meter;
using PRCrossfadeData = std::vector< std::vector < float > >;

#define BAD_STREAM_TIME (-DBL_MAX)

class PlaybackPolicy;

// To avoid growing the argument list of StartStream, add fields here
struct AudioIOStartStreamOptions
{
   explicit
   AudioIOStartStreamOptions(
      const std::shared_ptr<AudacityProject> &pProject, double rate_)
      : pProject{ pProject }
      , envelope(nullptr)
      , rate(rate_)
      , pStartTime(NULL)
      , preRoll(0.0)
   {}

   std::shared_ptr<AudacityProject> pProject;
   std::weak_ptr<Meter> captureMeter, playbackMeter;
   const BoundedEnvelope *envelope; // for time warping
   std::shared_ptr< AudioIOListener > listener;
   double rate;
   double * pStartTime;
   double preRoll;

   bool playNonWaveTracks{ true };

   // contents may get swapped with empty vector
   PRCrossfadeData      *pCrossfadeData{};

   // An unfortunate thing needed just to make scrubbing work on Linux when
   // we can't use a separate polling thread.
   // The return value is a number of milliseconds to sleep before calling again
   std::function< unsigned long() > playbackStreamPrimer;

   using PolicyFactory = std::function< std::unique_ptr<PlaybackPolicy>() >;
   PolicyFactory policyFactory;
};

struct AudioIODiagnostics{
   wxString filename;    // For crash report bundle
   wxString text;        // One big string, may be localized
   wxString description; // Non-localized short description
};

//! Abstract interface to alternative, concurrent playback with the main audio (such as MIDI events)
class AUDIO_DEVICES_API AudioIOExtBase
{
public:
   virtual ~AudioIOExtBase();

   // Formerly in AudioIOBase
   virtual bool IsOtherStreamActive() const = 0;

   //! Get diagnostic information for audio devices and also for extensions
   virtual AudioIODiagnostics Dump() const = 0;
};

///\brief A singleton object supporting queries of the state of any active
/// audio streams, and audio device capabilities
class AUDIO_DEVICES_API AudioIOBase /* not final */
   : public NonInterferingBase
{
public:
   static AudioIOBase *Get();

   AudioIOBase();
   virtual ~AudioIOBase();

   AudioIOBase(const AudioIOBase &) = delete;
   AudioIOBase &operator=(const AudioIOBase &) = delete;

   void SetCaptureMeter(
      const std::shared_ptr<AudacityProject> &project, const std::weak_ptr<Meter> &meter);
   void SetPlaybackMeter(
      const std::shared_ptr<AudacityProject> &project, const std::weak_ptr<Meter> &meter);

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
   wxString GetDeviceInfo() const;

   //! Get diagnostic information for audio devices and also for extensions
   std::vector<AudioIODiagnostics> GetAllDeviceInfo();

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

   std::weak_ptr<AudacityProject> mOwningProject;

   /// True if audio playback is paused
   bool                mPaused;

   volatile int        mStreamToken;

   /// Audio playback rate in samples per second
   double              mRate;

   PaStream           *mPortStreamV19;

   std::weak_ptr<Meter> mInputMeter{};
   std::weak_ptr<Meter> mOutputMeter{};

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

   /*! This class needs to iterate this array for one limited purpose but does
    not populate it and does not give access to it except to subclasses
    */
   std::vector<std::unique_ptr<AudioIOExtBase>> mAudioIOExt;
};

#include "Prefs.h"

extern AUDIO_DEVICES_API StringSetting AudioIOHost;
extern AUDIO_DEVICES_API DoubleSetting AudioIOLatencyCorrection;
extern AUDIO_DEVICES_API DoubleSetting AudioIOLatencyDuration;
extern AUDIO_DEVICES_API StringSetting AudioIOPlaybackDevice;
extern AUDIO_DEVICES_API IntSetting    AudioIORecordChannels;
extern AUDIO_DEVICES_API StringSetting AudioIORecordingDevice;
extern AUDIO_DEVICES_API StringSetting AudioIORecordingSource;
extern AUDIO_DEVICES_API IntSetting    AudioIORecordingSourceIndex;

#endif
