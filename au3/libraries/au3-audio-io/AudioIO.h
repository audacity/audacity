/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.h

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO__
#define __AUDACITY_AUDIO_IO__

#include "au3-audio-devices/AudioIOBase.h" // to inherit
#include "au3-mixer/AudioIOSequences.h"
#include "PlaybackSchedule.h" // member variable
#include "RingBuffer.h"
#include "au3-utility/LockFreeQueue.h"

#include <condition_variable>
#include <functional>
#include <memory>
#include <optional>
#include <mutex>
#include <thread>
#include <utility>
#include <array>
#include <wx/atomic.h> // member variable
#include <wx/thread.h>

#include "au3-components/PluginProvider.h" // for PluginID
#include "au3-utility/Observer.h"
#include "au3-math/SampleFormat.h"

class wxArrayString;
class AudioIOBase;
class AudioIO;
class Mixer;
class OtherPlayableSequence;
class RealtimeEffectState;
class Resample;

class AudacityProject;

struct PaStreamCallbackTimeInfo;
typedef unsigned long PaStreamCallbackFlags;
typedef int PaError;

namespace RealtimeEffects {
class InitializationScope;
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

//! Result of a round-trip measurement through a loopback (cable, or speaker to microphone)
struct AudioIOLoopbackResult {
    enum class Status {
        Measured,
        Busy, //!< Another stream owns the audio device
        DeviceError,
        NoSignal, //!< Too few bursts came back to trust the result
    };
    Status status = Status::DeviceError;
    double sampleRate = 0.0;
    long roundTripFrames = -1;
    //! What automatic latency compensation would use for this stream
    double reportedInputLatencySecs = 0.0;
    double reportedOutputLatencySecs = 0.0;
    size_t burstsFound = 0;
    size_t burstsTotal = 0;
    long spreadFrames = 0;
    double minConfidence = 0.0;
    float inputPeak = 0.0f;
    bool inverted = false;
    int inputChannel = -1;
};

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

    static constexpr size_t MaxPlaybackChannels = 2;
    struct Track {
        std::shared_ptr<const PlayableSequence> mSequence;
        //! Dry audio, one ring per channel of the sequence
        std::array<std::unique_ptr<RingBuffer>, MaxPlaybackChannels> mBuffers;
        //! Fader gain per output channel at the end of the last callback;
        //! negative until the first one. Callback only
        std::array<float, MaxPlaybackChannels> mLastGains { -1.0f, -1.0f };
        //! Callback only: audio after the track effects, not mixed yet,
        //! CallbackChunk frames per channel. Effects drop their latency from
        //! the start of their output, so a track with latent effects reads
        //! its rings that much further ahead and stays aligned with the others
        std::array<std::vector<float>, MaxPlaybackChannels> mProcessed;
        size_t mProcessedFrames { 0 };
        uint64_t mRingFramesRead { 0 };
        size_t mLatencyFrames { 0 };
        //! Callback only: after a seek the effects still hold mLatencyFrames
        //! of the old position; this much of their output is dropped
        size_t mDropFrames { 0 };

        Track(std::shared_ptr<const PlayableSequence> sequence);
        ~Track();

        int64_t trackId() const;
    };

    //! @}

    std::shared_ptr< AudioIOListener > GetListener() const
    { return mListener.lock(); }
    void SetListener(const std::shared_ptr< AudioIOListener >& listener);

    struct AudioCallbackInfo {
        TimePoint dacTime;
        int numSamples = 0;
    };
    using AudioCallbackInfoQueue = LockFreeQueue<AudioCallbackInfo>;

    AudioCallbackInfoQueue& GetAudioCallbackInfoQueue() { return mAudioCallbackInfoQueue; }

    //! Callback: drops audio from before a completed seek, see HandleSeekRequest
    void ApplyCompletedSeek();

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
        float* outputFloats, unsigned long framesPerBuffer, float* outputMeterFloats, const TimePoint& meterTime);
    //! Runs the track effects until `frames` are ready to mix or the rings
    //! run out; returns how many are ready
    size_t ProcessTrack(Track& track, size_t frames, std::optional<RealtimeEffects::ProcessingScope>& scope);
    //! Fader, pan, mute and solo of each track, summed into `mix`, with track meters
    void MixTracks(float* const* mix, size_t frames, const IMeterSenderPtr& meter, const TimePoint& meterTime);
    constSamplePtr ApplyRecordGain(
        constSamplePtr inputBuffer, float gain, size_t numSamples, samplePtr scratch);
    unsigned long DrainInputBuffers(
        constSamplePtr inputBuffer, unsigned long framesPerBuffer, const PaStreamCallbackFlags statusFlags, float* tempFloats);
    void UpdateTimePosition(
        unsigned long framesPerBuffer);
    void DoPlaythrough(
        constSamplePtr inputBuffer, float* outputBuffer, unsigned long framesPerBuffer, float* outputMeterFloats);
    void SendVuInputMeterData(const float* inputSamples, unsigned long framesPerBuffer, const TimePoint& dacTime);
    void SendVuOutputMeterData(const float* outputMeterFloats, unsigned long framesPerBuffer, const TimePoint& dacTime);
    void PushMasterOutputMeterValues(const IMeterSenderPtr& sender, const float* values, uint8_t channels, unsigned long frames,
                                     const TimePoint& dacTime);
    void PushInputMeterValues(const IMeterSenderPtr& sender, const float* values, unsigned long frames, const TimePoint& dacTime);

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
    struct TrackChannelInfo {
        size_t sequenceIndex{};
        size_t channelIndex{};
    };
    std::vector<TrackChannelInfo> mCaptureChannelLayout;
    std::vector<std::vector<size_t> > mTrackChannelSourceMap;
    bool mCaptureNeedsMixdown{ false };
    //!Buffers that hold outcome of transformations applied to each individual sample source.
    //!Number of buffers equals to the sum of number all source channels.
    std::vector<std::vector<float> > mProcessingBuffers;
    //! Callback only: mix, effect scratch and meter buffers, CallbackChunk
    //! frames each; the callback works in chunks of that size
    static constexpr size_t CallbackChunk = 4096;
    enum CallbackBuffer : size_t {
        Mix, // MaxPlaybackChannels of them
        EffectScratch = Mix + MaxPlaybackChannels,
        EffectDummy = EffectScratch + MaxPlaybackChannels,
        TrackMeter,
        CallbackBufferCount
    };
    std::vector<std::vector<float> > mCallbackBuffers;
    //! Set while a stream with realtime effects is open; the callback runs
    //! the effects with it
    RealtimeEffects::InitializationScope* mCallbackRealtimeInit { nullptr };
    /*! Read by worker threads but unchanging during playback */
    RingBuffers mPlaybackBuffers;
    std::vector<Track> mPlaybackTracks;
    ConstPlayableSequences mPlaybackSequences;
    // Old volume is used in playback in linearly interpolating
    // the volume.
    float mOldPlaybackVolume;

    std::vector<std::unique_ptr<Mixer> > mPlaybackMixers;

    std::atomic<float> mMixerOutputVol{ 1.0 };
    std::atomic<float> mSoftwareRecordGain{ 1.0 };
    static int mNextStreamToken;
    double mFactor;
    unsigned long mMaxFramesOutput;      // The actual number of frames output.
    /*! Read by a worker thread but unchanging during playback */
    bool mbMicroFades;

    /*! @name Seeking during playback
     The main thread writes the target and then bumps mSeekRequested. The
     producer repositions the mixers and publishes how many frames it had
     written to the track rings before the new position. The callback drops
     up to there and tells the main thread which time records to skip.
     @{
     */
    std::atomic<double> mSeekTarget { 0.0 };
    std::atomic<uint64_t> mSeekRequested { 0 };
    std::atomic<uint64_t> mSeekDone { 0 };
    std::atomic<uint64_t> mRingFramesAtSeek { 0 };
    //! Producer only
    uint64_t mSeekHandled { 0 };
    uint64_t mRingFramesWritten { 0 };
    //! Callback only
    uint64_t mSeekApplied { 0 };
    //! The rings are empty after a seek until the producer refills them
    bool mRefillingAfterSeek { false };
    uint64_t mFramesMixed { 0 };
    uint64_t mFramesOutput { 0 };
    struct TimeSkip {
        uint64_t seek = 0;
        //! Count of frames output before the dropped ones
        uint64_t atOutputFrame = 0;
        size_t frames = 0;
    };
    LockFreeQueue<TimeSkip> mTimeSkipQueue { 64 };
    //! Main thread only
    std::optional<TimeSkip> mPendingTimeSkip;
    uint64_t mTimeConsumedFrames { 0 };
    //! Shown as the stream time until the new position is heard
    std::optional<double> mSeekTargetShown;
    uint64_t mSeekTargetShownFor { 0 };
    //! @}
    PlaybackPolicy::Duration mPlaybackRingBufferSecs;
    double mCaptureRingBufferSecs;

    /// Preferred batch size for replenishing the playback RingBuffer
    size_t mPlaybackSamplesToCopy;
    /// Hardware output latency in frames
    size_t mHardwarePlaybackLatencyFrames { 0u };
    double mHardwarePlaybackLatencyMs{ 0 };
    double mHardwareCaptureLatencyMs{ 0 };
    /// Occupancy of the queue we try to maintain, with bigger batches if needed
    size_t mPlaybackQueueMinimum;
    //! Largest read-ahead of a track for its latent effects, set by the
    //! callback; the producer keeps that much more audio queued
    std::atomic<size_t> mTrackLatencyFrames { 0 };

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

    //! The flags above are set on one thread and awaited on another; whoever
    //! sets one calls NotifyAudioThreadState()
    std::mutex mAudioThreadStateMutex;
    std::condition_variable mAudioThreadStateChanged;
    void NotifyAudioThreadState();
    template<typename Ready>
    void WaitForAudioThreadState(const Ready& ready)
    {
        std::unique_lock lock(mAudioThreadStateMutex);
        mAudioThreadStateChanged.wait(lock, ready);
    }

    // Async start/stop + wait of AudioThread processing.
    // Provided to allow more flexibility, however use with caution:
    // never call Stop between Start and the wait for Started (and the converse)
    void StartAudioThread();
    void WaitForAudioThreadStarted();
    void StopAudioThread();
    void WaitForAudioThreadStopped();

    void ProcessOnceAndWait();

    std::atomic<bool> mForceFadeOut{ false };

    wxLongLong mLastPlaybackTimeMillis;

    //! Not (yet) used; should perhaps be atomic when it is
    double mLastRecordingOffset;
    PaError mLastPaError;

protected:
    static size_t MinValue(
        const RingBuffers& buffers, size_t (RingBuffer::* pmf)() const);
    //! Over the track rings when there are tracks, else over mPlaybackBuffers
    size_t MinPlaybackValue(size_t (RingBuffer::* pmf)() const) const;

    float GetMixerOutputVol()
    {
        return mMixerOutputVol.load(std::memory_order_relaxed);
    }

    void SetMixerOutputVol(float value)
    {
        mMixerOutputVol.store(value, std::memory_order_relaxed);
    }

    float GetSoftwareRecordGain()
    {
        return mSoftwareRecordGain.load(std::memory_order_relaxed);
    }

    void SetSoftwareRecordGain(float value)
    {
        mSoftwareRecordGain.store(value, std::memory_order_relaxed);
    }

    /*! Pointer is read by a worker thread but unchanging during playback.
     (Whether its overriding methods are race-free is not for AudioIO to ensure.)
     */
    std::weak_ptr< AudioIOListener > mListener;

    bool mUsingAlsa { false };
    bool mUsingJack { false };
    bool mUsingCoreAudio { false };
    //! Automatic compensation takes the round trip from the stream's own
    //! timestamps; producer only after the start
    bool mCompensateFromStreamTimes { false };
    //! Output DAC time minus input ADC time of the last duplex callback, s
    std::atomic<double> mStreamRoundTrip { 0.0 };

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

    // In a jitter-free world, the size of this queue only has to be `ceil(producerRate / consumerRate)`.
    // If we assume a minimum of 30fps for the consumer (this is supposed to be read by a entity occupied with
    // smooth rendering of the playback cursor), and a callback pushing maybe as little as 10 samples per
    // callback (I've just seen 14 samples per callback, on MacOS with a requested buffer size of 10ms),
    // we get ceil(96000 / 10 / 30) = 360. The rounding to the next power of two should accommodate jitter.
    AudioCallbackInfoQueue mAudioCallbackInfoQueue { 512 };

    //! Captured frames the audio callback must skip before feeding the queue
    //! above for recording-only streams; mirrors the leading frames that
    //! DrainInputBuffers discards for latency compensation.
    unsigned long long mCaptureClockDiscardFrames = 0;

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

    /** \brief Stop monitoring */
    void StopMonitoring() override;

    /** \brief Wait for busy state to end */
    void WaitWhileBusy() const;

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
    //! Move the playback position of the current stream to `time`; returns at once
    void SeekStreamTo(double time);

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
    int GetHardwarePlaybackLatencyMs() const { return mHardwarePlaybackLatencyMs; }
    int GetHardwareCaptureLatencyMs() const { return mHardwareCaptureLatencyMs; }

    // Meaning really capturing, not just lead-in time playing
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

    //! Blocks for about `seconds`. Uses the recording devices, rate and buffer
    //! settings; fails with Busy while any stream is open
    AudioIOLoopbackResult MeasureLoopbackLatency(double projectRate, double seconds, float gain);

    static void AudioThread(std::atomic<bool>& finish);

    static void Init();
    static void Deinit();

    /*! For purposes of CallAfterRecording, treat time from now as if
     recording (when argument is true) or not necessarily so (false) */
    void DelayActions(bool recording);

private:
    bool DelayingActions() const;

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

    void ResetCaptureRouting();
    void ConfigureCaptureRouting(size_t requestedChannels);

    //! First part of SequenceBufferExchange
    void FillPlayBuffers();
    //! Producer: repositions for the latest seek request, see mSeekRequested
    //! @return whether it repositioned
    bool HandleSeekRequest();

    bool ProcessPlaybackSlices(size_t available);

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
