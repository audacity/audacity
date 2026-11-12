/**********************************************************************

Audacity: A Digital Audio Editor

AudioIOBase.h

Paul Licameli split from AudioIO.h

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_BASE__
#define __AUDACITY_AUDIO_IO_BASE__

#include <atomic>
#include <cfloat>
#include <chrono>
#include <functional>
#include <map>
#include <optional>
#include <vector>
#include <utility>
#include <wx/string.h>
#include "au3-utility/MemoryX.h"
#include "IMeterSender.h"
#include "AudioDeviceSettings.h"

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

class PlaybackPolicy;

// To avoid growing the argument list of StartStream, add fields here
struct AudioIOStartTransportOptions
{
    explicit
    AudioIOStartTransportOptions(
        const std::shared_ptr<AudacityProject>& pProject = {},
        double rate_ = 44100.0)
        : pProject{pProject},
        rate(rate_)
    {
    }

    std::shared_ptr<AudacityProject> pProject;
    std::weak_ptr<IMeterSender> captureMeter;
    std::weak_ptr<IMeterSender> playbackMeter;
    const BoundedEnvelope* envelope{}; // for time warping
    std::shared_ptr< AudioIOListener > listener;
    double rate;
    mutable std::optional<double> pStartTime;
    double leadInTime{ 0.0 };

    bool playNonWaveTracks{ true };
    bool inputMonitoring{ false };

    // contents may get swapped with empty vector
    PRCrossfadeData* pCrossfadeData{};

    // An unfortunate thing needed just to make scrubbing work on Linux when
    // we can't use a separate polling thread.
    // The return value is duration to sleep before calling again
    std::function< std::chrono::milliseconds() > playbackStreamPrimer;

    using PolicyFactory = std::function<
        std::unique_ptr<PlaybackPolicy>(const AudioIOStartTransportOptions&) >;
    PolicyFactory policyFactory;

    bool loopEnabled{ false };
    bool variableSpeed{ false };
};

struct AudioIODiagnostics {
    wxString filename;   // For crash report bundle
    wxString text;       // One big string, may be localized
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
class AUDIO_DEVICES_API AudioIOBase /* not final */ : public NonInterferingBase
{
public:
    static AudioIOBase* Get();

    AudioIOBase();
    virtual ~AudioIOBase();

    AudioIOBase(const AudioIOBase&) = delete;
    AudioIOBase& operator=(const AudioIOBase&) = delete;

    /**
     * \brief Start the current VU meters
     */
    void StartMeters();

    /**
     * \brief Stop the current VU meters
     */
    void StopMeters();

    /**
     * \brief Reset the current VU meters
     */
    void ResetMeters();

    void SetCaptureMeter(
        const std::shared_ptr<AudacityProject>& project, const std::weak_ptr<IMeterSender>& meter);
    void SetPlaybackMeter(
        const std::shared_ptr<AudacityProject>& project, const std::weak_ptr<IMeterSender>& meter);

    /** \brief update state after changing what audio devices are selected
     *
     * Called when the devices stored in the preferences are changed to update
     * the audio mixer capabilities
     *
     * \todo: Make this do a sample rate query and store the result in the
     * AudioIO object to avoid doing it later? Would simplify the
     * GetSupported*Rate functions considerably */
    void HandleDeviceChange();

    /** \brief Get diagnostic information on all the available audio I/O devices
     *
     */
    wxString GetDeviceInfo() const;

    //! Get diagnostic information for audio devices and also for extensions
    std::vector<AudioIODiagnostics> GetAllDeviceInfo();

    /** \brief Find out if playback / recording is currently paused */
    bool IsPaused() const;

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

    /** \brief Stop input monitoring */
    virtual void StopMonitoring() = 0;

    /* Mixer services are always available.  If no stream is running, these
     * methods use whatever device is specified by the preferences.  If a
     * stream *is* running, naturally they manipulate the mixer associated
     * with that stream.  If no mixer is available, output is emulated and
     * input is stuck at 1.0f (a volume gain is applied to output samples).
     */
    void SetMixer(int inputSource);

    double GetPlaybackSampleRate() const { return mRate; }

protected:
    static std::unique_ptr<AudioIOBase> ugAudioIO;
    static wxString DeviceName(const PaDeviceInfo* info);
    static wxString HostName(const PaDeviceInfo* info);

    std::weak_ptr<AudacityProject> mOwningProject;

    /// True if audio playback is paused
    std::atomic<bool> mPaused{ false };

    /*! Read by worker threads but unchanging during playback */
    std::atomic<int> mStreamToken{ 0 };

    /// Audio playback rate in samples per second
    /*! Read by worker threads but unchanging during playback */
    double mRate;

    PaStream* mPortStreamV19;

    std::weak_ptr<IMeterSender> mInputMeter{};
    std::weak_ptr<IMeterSender> mOutputMeter{};

   #if USE_PORTMIXER
    PxMixer* mPortMixer;
    float mPreviousHWPlaythrough;
   #endif /* USE_PORTMIXER */

    /** @brief Can we control the hardware input level?
     *
     * This flag is set to true if using portmixer to control the
     * input volume seems to be working (and so we offer the user the control),
     * and to false (locking the control out) otherwise. This avoids stupid
     * scaled clipping problems when trying to do software emulated input volume
     * control */
    bool mInputMixerWorks;

    // The devices that HandleDeviceChange() last handled
    static int mCurrentPlaybackIndex;
    static int mCurrentCaptureIndex;
    static double mCachedBestRateIn;

protected:
    /** \brief get the index of the device selected in the preferences.
     *
     * If the device isn't found, returns -1
     */
#if USE_PORTMIXER
    static int getRecordSourceIndex(PxMixer* portMixer);
#endif

    /*! This class needs to iterate this array for one limited purpose but does
     not populate it and does not give access to it except to subclasses
     */
    std::vector<std::unique_ptr<AudioIOExtBase> > mAudioIOExt;
};

#endif
