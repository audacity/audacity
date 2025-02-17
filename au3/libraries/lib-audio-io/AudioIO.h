/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.h

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO__
#define __AUDACITY_AUDIO_IO__

#include "AudioIOBase.h" // to inherit
#include "AudioIOSequences.h"
#include "PlaybackSchedule.h" // member variable

#include <functional>
#include <memory>
#include <mutex>
#include <thread>
#include <utility>
#include <wx/atomic.h> // member variable
#include <wx/thread.h>

#include "PluginProvider.h" // for PluginID
#include "Observer.h"
#include "SampleCount.h"
#include "SampleFormat.h"

class wxArrayString;
class AudioIOBase;
class AudioIO;
class RingBuffer;
class Mixer;
class OtherPlayableSequence;
class RealtimeEffectState;
class Resample;

class AudacityProject;

struct PaStreamCallbackTimeInfo;
typedef unsigned long PaStreamCallbackFlags;
typedef int PaError;

namespace RealtimeEffects {
class ProcessingScope;
}

bool ValidateDeviceNames();

enum class Acknowledge {
    eNone = 0, eStart, eStop
};

/*!
 Emitted by the global AudioIO object when play, recording, or monitoring
 starts or stops
*/
struct AudioIOEvent {
    AudacityProject* pProject;
    enum Type {
        PLAYBACK,
        CAPTURE,
        MONITOR,
        PAUSE,
    } type;
    bool on;
};

struct AUDIO_IO_API TransportSequences final {
    ConstPlayableSequences playbackSequences;
    RecordableSequences captureSequences;
    std::vector<std::shared_ptr<const OtherPlayableSequence> >
    otherPlayableSequences;

    // This is a subset of playbackSequences
    ConstPlayableSequences prerollSequences;
};

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
 * @param timeInfo Pointer to PortAudio time information
 * structure, which tells us how long we have been playing / recording
 * @param statusFlags PortAudio stream status flags
 * @param userData pointer to user-defined data structure. Provided for
 * flexibility by PortAudio, but not used by Audacity - the data is stored in
 * the AudioIO class instead.
 */
int audacityAudioCallback(
    const void* inputBuffer, void* outputBuffer, unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags, void* userData);

class AudioIOExt;

class AUDIO_IO_API AudioIoCallback /* not final */ : public AudioIOBase
{
public:
    AudioIoCallback();
    ~AudioIoCallback() override;

public:
    // This function executes in a thread spawned by the PortAudio library
    int AudioCallback(
        constSamplePtr inputBuffer, float* outputBuffer, unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* timeInfo,
        const PaStreamCallbackFlags statusFlags, void* userData);

    //! @name iteration over extensions, supporting range-for syntax
    //! @{
    class AUDIO_IO_API AudioIOExtIterator
    {
    public:
        using difference_type = ptrdiff_t;
        using value_type = AudioIOExt&;
        using pointer = AudioIOExt*;
        using reference = AudioIOExt&;
        using iterator_category = std::forward_iterator_tag;

        explicit AudioIOExtIterator(AudioIoCallback& audioIO, bool end)
            : mIterator{end
                        ? audioIO.mAudioIOExt.end()
                        : audioIO.mAudioIOExt.begin()}
        {}
        AudioIOExtIterator& operator ++() { ++mIterator; return *this; }
        auto operator *() const -> AudioIOExt &;
        friend inline bool operator ==(
            const AudioIOExtIterator& xx, const AudioIOExtIterator& yy)
        {
            return xx.mIterator == yy.mIterator;
        }

        friend inline bool operator !=(
            const AudioIOExtIterator& xx, const AudioIOExtIterator& yy)
        {
            return !(xx == yy);
        }

    private:
        std::vector<std::unique_ptr<AudioIOExtBase> >::const_iterator mIterator;
    };
    struct AudioIOExtRange {
        AudioIOExtIterator first;
        AudioIOExtIterator second;
        AudioIOExtIterator begin() const { return first; }
        AudioIOExtIterator end() const { return second; }
    };

    AudioIOExtRange Extensions()
    {
        return {
            AudioIOExtIterator{ *this, false },
            AudioIOExtIterator{ *this, true }
        };
    }

    //! @}

    std::shared_ptr< AudioIOListener > GetListener() const
    { return mListener.lock(); }
    void SetListener(const std::shared_ptr< AudioIOListener >& listener);

    // Part of the callback
    int CallbackDoSeek();

    // Part of the callback
    void CallbackCheckCompletion(
        int& callbackReturn, unsigned long len);

    int mbHasSoloSequences;
    int mCallbackReturn;
    // Helpers to determine if sequences have already been faded out.
    unsigned  CountSoloingSequences();

    bool SequenceShouldBeSilent(const PlayableSequence& ps);

    void CheckSoundActivatedRecordingLevel(
        float* inputSamples, unsigned long framesPerBuffer);

    bool FillOutputBuffers(
        float* outputFloats, unsigned long framesPerBuffer, float* outputMeterFloats);
    void DrainInputBuffers(
        constSamplePtr inputBuffer, unsigned long framesPerBuffer, const PaStreamCallbackFlags statusFlags, float* tempFloats);
    void UpdateTimePosition(
        unsigned long framesPerBuffer);
    void DoPlaythrough(
        constSamplePtr inputBuffer, float* outputBuffer, unsigned long framesPerBuffer, float* outputMeterFloats);
    void SendVuInputMeterData(
        const float* inputSamples, unsigned long framesPerBuffer);
    void SendVuOutputMeterData(
        const float* outputMeterFloats, unsigned long framesPerBuffer);

    /** \brief Get the number of audio samples ready in all of the playback
    * buffers.
    *
    * Returns the smallest of the buffer ready space values in the event that
    * they are different. */
    size_t GetCommonlyReadyPlayback();

    size_t GetCommonlyWrittenForPlayback();

    /// How many frames of zeros were output due to pauses?
    long mNumPauseFrames;

    std::thread mAudioThread;
    std::atomic<bool> mFinishAudioThread{ false };

    std::vector<std::unique_ptr<Resample> > mResample;

    using RingBuffers = std::vector<std::unique_ptr<RingBuffer> >;
    RingBuffers mCaptureBuffers;
    RecordableSequences mCaptureSequences;
    //!Buffers that hold outcome of transformations applied to each individual sample source.
    //!Number of buffers equals to the sum of number all source channels.
    std::vector<std::vector<float> > mProcessingBuffers;
    //!These buffers are used to mix and process the result of processed source channels.
    //!Number of buffers equals to number of output channels.
    std::vector<std::vector<float> > mMasterBuffers;
    /*! Read by worker threads but unchanging during playback */
    RingBuffers mPlaybackBuffers;
    ConstPlayableSequences mPlaybackSequences;
    // Old volume is used in playback in linearly interpolating
    // the volume.
    float mOldPlaybackVolume;
    // Temporary buffers, each as large as the playback buffers
    std::vector<SampleBuffer> mScratchBuffers;
    std::vector<float*> mScratchPointers; //!< pointing into mScratchBuffers

    std::vector<std::unique_ptr<Mixer> > mPlaybackMixers;

    std::atomic<float> mMixerOutputVol{ 1.0 };
    static int mNextStreamToken;
    double mFactor;
    unsigned long mMaxFramesOutput;      // The actual number of frames output.
    /*! Read by a worker thread but unchanging during playback */
    bool mbMicroFades;

    double mSeek;
    PlaybackPolicy::Duration mPlaybackRingBufferSecs;
    double mCaptureRingBufferSecs;

    /// Preferred batch size for replenishing the playback RingBuffer
    size_t mPlaybackSamplesToCopy;
    /// Hardware output latency in frames
    size_t mHardwarePlaybackLatencyFrames {};
    /// Occupancy of the queue we try to maintain, with bigger batches if needed
    size_t mPlaybackQueueMinimum;

    double mMinCaptureSecsToCopy;
    /*! Read by a worker thread but unchanging during playback */
    bool mSoftwarePlaythrough;
    /// True if Sound Activated Recording is enabled
    /*! Read by a worker thread but unchanging during playback */
    bool mPauseRec;
    float mSilenceLevel;
    /*! Read by a worker thread but unchanging during playback */
    size_t mNumCaptureChannels;
    /*! Read by a worker thread but unchanging during playback */
    size_t mNumPlaybackChannels;
    sampleFormat mCaptureFormat;
    double mCaptureRate{};
    unsigned long long mLostSamples{ 0 };
    std::atomic<bool> mAudioThreadShouldCallSequenceBufferExchangeOnce;
    std::atomic<bool> mAudioThreadSequenceBufferExchangeLoopRunning;
    std::atomic<bool> mAudioThreadSequenceBufferExchangeLoopActive;

    std::atomic<Acknowledge> mAudioThreadAcknowledge;

    // Async start/stop + wait of AudioThread processing.
    // Provided to allow more flexibility, however use with caution:
    // never call Stop between Start and the wait for Started (and the converse)
    void StartAudioThread();
    void WaitForAudioThreadStarted();
    void StopAudioThread();
    void WaitForAudioThreadStopped();

    void ProcessOnceAndWait(std::chrono::milliseconds sleepTime = std::chrono::milliseconds(50));

    std::atomic<bool> mForceFadeOut{ false };

    wxLongLong mLastPlaybackTimeMillis;

    //! Not (yet) used; should perhaps be atomic when it is
    double mLastRecordingOffset;
    PaError mLastPaError;

protected:
    static size_t MinValue(
        const RingBuffers& buffers, size_t (RingBuffer::*pmf)() const);

    float GetMixerOutputVol()
    {
        return mMixerOutputVol.load(std::memory_order_relaxed);
    }

    void SetMixerOutputVol(float value)
    {
        mMixerOutputVol.store(value, std::memory_order_relaxed);
    }

    /*! Pointer is read by a worker thread but unchanging during playback.
     (Whether its overriding methods are race-free is not for AudioIO to ensure.)
     */
    std::weak_ptr< AudioIOListener > mListener;

    bool mUsingAlsa { false };
    bool mUsingJack { false };

    // For cacheing supported sample rates
    static double mCachedBestRateOut;
    static bool mCachedBestRatePlaying;
    static bool mCachedBestRateCapturing;

    // Serialize main thread and PortAudio thread's attempts to pause and change
    // the state used by the third, Audio thread.
    wxMutex mSuspendAudioThread;

public:
    // Whether an exception (as for exhaustion of resource space) was detected
    // in recording, and not yet cleared at the end of the procedure to stop
    // recording.
    bool HasRecordingException() const
    { return mRecordingException; }

protected:
    // A flag tested and set in one thread, cleared in another.  Perhaps
    // this guarantee of atomicity is more cautious than necessary.
    wxAtomicInt mRecordingException {};
    void SetRecordingException()
    { wxAtomicInc(mRecordingException); }
    void ClearRecordingException()
    {
        if (mRecordingException) {
            wxAtomicDec(mRecordingException);
        }
    }

    std::vector< std::pair<double, double> > mLostCaptureIntervals;
    /*! Read by a worker thread but unchanging during playback */
    bool mDetectDropouts{ true };

public:
    // Pairs of starting time and duration
    const std::vector< std::pair<double, double> >& LostCaptureIntervals()
    { return mLostCaptureIntervals; }

    // Used only for testing purposes in alpha builds
    bool mSimulateRecordingErrors{ false };

    // Whether to check the error code passed to audacityAudioCallback to
    // detect more dropouts
    std::atomic<bool> mDetectUpstreamDropouts{ true };

protected:
    RecordingSchedule mRecordingSchedule{};
    PlaybackSchedule mPlaybackSchedule;

    struct TransportState;
    //! Holds some state for duration of playback or recording
    std::unique_ptr<TransportState> mpTransportState;

private:
    /*!
     Privatize the inherited array but give access by Extensions().
     This class guarantees that this array is populated only with non-null
     pointers to the subtype AudioIOExt
     */
    using AudioIOBase::mAudioIOExt;
};

struct PaStreamInfo;

class AUDIO_IO_API AudioIO final : public AudioIoCallback, public Observer::Publisher<AudioIOEvent>
{
    AudioIO();
    ~AudioIO() override;
    void StartThread();

public:
    // This might return null during application startup or shutdown
    static AudioIO* Get();

    //! Forwards to RealtimeEffectManager::AddState with proper init scope
    /*!
     @post result: `!result || result->GetEffect() != nullptr`
     */
    std::shared_ptr<RealtimeEffectState>
    AddState(AudacityProject& project, ChannelGroup* pGroup, const PluginID& id);

    //! Forwards to RealtimeEffectManager::ReplaceState with proper init scope
    /*!
     @post result: `!result || result->GetEffect() != nullptr`
     */
    std::shared_ptr<RealtimeEffectState>
    ReplaceState(AudacityProject& project, ChannelGroup* pGroup, size_t index, const PluginID& id);

    //! Forwards to RealtimeEffectManager::RemoveState with proper init scope
    void RemoveState(AudacityProject& project, ChannelGroup* pGroup, std::shared_ptr<RealtimeEffectState> pState);

    /** \brief Start up Portaudio for capture and recording as needed for
     * input monitoring and software playthrough only
     *
     * This uses the Default project sample format, current sample rate, and
     * selected number of input channels to open the recording device and start
     * reading input data. If software playthrough is enabled, it also opens
     * the output device in stereo to play the data through */
    void StartMonitoring(const AudioIOStartStreamOptions& options);

    /** \brief Start recording or playing back audio
     *
     * Allocates buffers for recording and playback, gets the Audio thread to
     * fill them, and sets the stream rolling.
     * If successful, returns a token identifying this particular stream
     * instance.  For use with IsStreamActive()
     *
     * @pre `p && p->FindChannelGroup()` for all pointers `p` in
     *    `sequences.playbackSequences`
     * @pre `p != nullptr` for all pointers `p` in
     *    `sequences.captureSequences`
     */

    int StartStream(const TransportSequences& sequences, double t0, double t1, double mixerLimit, //!< Time at which mixer stops producing, maybe > t1
                    const AudioIOStartStreamOptions& options);

    /** \brief Stop recording, playback or input monitoring.
     *
     * Does quite a bit of housekeeping, including switching off monitoring,
     * flushing recording buffers out to RecordableSequences, and applies latency
     * correction to recorded sequences if necessary */
    void StopStream() override;
    /** \brief Move the playback / recording position of the current stream
     * by the specified amount from where it is now */
    void SeekStream(double seconds);

    using PostRecordingAction = std::function<void ()>;

    //! Enqueue action for main thread idle time, not before the end of any recording in progress
    /*! This may be called from non-main threads */
    void CallAfterRecording(PostRecordingAction action);

public:
    wxString LastPaErrorString();

    wxLongLong GetLastPlaybackTime() const { return mLastPlaybackTimeMillis; }
    std::shared_ptr<AudacityProject> GetOwningProject() const
    { return mOwningProject.lock(); }

    /** \brief Pause and un-pause playback and recording */
    void SetPaused(bool state, bool publish = false);

    /* Mixer services are always available.  If no stream is running, these
     * methods use whatever device is specified by the preferences.  If a
     * stream *is* running, naturally they manipulate the mixer associated
     * with that stream.  If no mixer is available, output is emulated and
     * input is stuck at 1.0f (a volume gain is applied to output samples).
     */
    void SetMixer(int inputSource, float inputVolume, float playbackVolume);
    void GetMixer(int* inputSource, float* inputVolume, float* playbackVolume);
    /** @brief Find out if the input hardware level control is available
     *
     * Checks the mInputMixerWorks variable, which is set up in
     * AudioIOBase::HandleDeviceChange(). External people care, because we want to
     * disable the UI if it doesn't work.
     */
    bool InputMixerWorks();

    /** \brief Get the list of inputs to the current mixer device
     *
     * Returns an array of strings giving the names of the inputs to the
     * soundcard mixer (driven by PortMixer) */
    wxArrayString GetInputSourceNames();

    sampleFormat GetCaptureFormat() const { return mCaptureFormat; }
    size_t GetNumPlaybackChannels() const { return mNumPlaybackChannels; }
    size_t GetNumCaptureChannels() const { return mNumCaptureChannels; }

    // Meaning really capturing, not just pre-rolling
    bool IsCapturing() const;

    /** \brief Ensure selected device names are valid
     *
     */
    static bool ValidateDeviceNames(const wxString& play, const wxString& rec);

    bool IsAvailable(AudacityProject& project) const;

    /** \brief Return a valid sample rate that is supported by the current I/O
    * device(s).
    *
    * The return from this function is used to determine the sample rate that
    * audacity actually runs the audio I/O stream at. if there is no suitable
    * rate available from the hardware, it returns 0.
    * The sampleRate argument gives the desired sample rate (the rate of the
    * audio to be handled, i.e. the currently Project Rate).
    * capturing is true if the stream is capturing one or more audio channels,
    * and playing is true if one or more channels are being played. */
    double GetBestRate(bool capturing, bool playing, double sampleRate);

    /** \brief During playback, the sequence time most recently played
     *
     * When playing looped, this will start from t0 again,
     * too. So the returned time should be always between
     * t0 and t1
     */
    double GetStreamTime();

    static void AudioThread(std::atomic<bool>& finish);

    static void Init();
    static void Deinit();

    /*! For purposes of CallAfterRecording, treat time from now as if
     recording (when argument is true) or not necessarily so (false) */
    void DelayActions(bool recording);

private:

    bool DelayingActions() const;

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
     * being floating point always. Returns true if the stream opened successfully
     * and false if it did not. */
    bool StartPortAudioStream(const AudioIOStartStreamOptions& options, unsigned int numPlaybackChannels, unsigned int numCaptureChannels);

    void SetOwningProject(const std::shared_ptr<AudacityProject>& pProject);
    void ResetOwningProject();

    /*!
     Called in a loop from another worker thread that does not have the low-latency constraints
     of the PortAudio callback thread.  Does less frequent and larger batches of work that may
     include memory allocations and database operations.  RingBuffer objects mediate the transfer
     between threads, to overcome the mismatch of their batch sizes.
     */
    void SequenceBufferExchange();

    //! First part of SequenceBufferExchange
    void FillPlayBuffers();

    bool ProcessPlaybackSlices(
        std::optional<RealtimeEffects::ProcessingScope>& pScope, size_t available);

    //! Second part of SequenceBufferExchange
    void DrainRecordBuffers();

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

    /** \brief Allocate RingBuffer structures, and others, needed for playback
      * and recording.
      *
      * Returns true iff successful.
      */
    bool AllocateBuffers(
        const AudioIOStartStreamOptions& options, const TransportSequences& sequences, double t0, double t1, double sampleRate);

    /** \brief Clean up after StartStream if it fails.
      *
      * If bOnlyBuffers is specified, it only cleans up the buffers. */
    void StartStreamCleanup(bool bOnlyBuffers = false);

    std::mutex mPostRecordingActionMutex;
    PostRecordingAction mPostRecordingAction;

    bool mDelayingActions{ false };
};

AUDIO_IO_API extern BoolSetting SoundActivatedRecord;

#endif
