/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitInstance.h

  Dominic Mazzoni
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_AUDIO_UNIT_INSTANCE__
#define __AUDACITY_AUDIO_UNIT_INSTANCE__

#include "au3-effects/PerTrackEffect.h"
#include <atomic>
#include <vector>

#include "AudioUnitWrapper.h"
struct AudioUnitEvent;

class AudioUnitInstance : public PerTrackEffect::Instance, public AudioUnitWrapper
{
public:
    using Instance::mProcessor;

    AudioUnitInstance(const PerTrackEffect& effect, AudioComponent component, Parameters& parameters, const wxString& identifier,
                      unsigned audioIns, unsigned audioOuts, bool useLatency);
    ~AudioUnitInstance() override;

    bool Initialize();

    bool IsInitialized() const { return static_cast<bool>(mInitialization); }

    void EventListener(const AudioUnitEvent* inEvent, AudioUnitParameterValue inParameterValue);

    // Override the virtual function to allocate an empty message
    std::unique_ptr<Message> MakeMessage() const override;

    // A non-virtual overload makes a non-empty message
    std::unique_ptr<Message>
    MakeMessage(AudioUnitParameterID id, AudioUnitParameterValue value) const;

private:
    size_t InitialBlockSize() const;

    //! The body of ProcessInitialize and of the realtime initializations
    /*!
     @param realtime distinguishes a realtime processing scope, which may be
     entered repeatedly on an instance that the user keeps alive in its own
     editor, from one-shot processing of a selection
     */
    bool InitializeProcessing(EffectSettings& settings, double sampleRate, bool realtime);

    //! Install the transport callbacks; not fatal if the plug-in refuses them
    bool SetHostCallbacks();

    //! Record a transport transition, to be reported until the next block ends
    void SetTransportPlaying(bool playing);

    //! Render one silent block with the transport reported as stopped
    /*!
     A plug-in that accumulates what it is fed while the host plays needs to be
     told that no more is coming; otherwise it waits forever for the rest of a
     transfer that has already ended.  Does nothing unless the transport was
     playing and the unit is still initialized.
     */
    void NotifyTransportStopped() noexcept;

    //! @name Implementations of AudioUnitUtils::HostCallbacks
    //! Any out parameter may be null.  Called by the plug-in, on the thread
    //! that renders it and possibly also on its own user interface thread.
    //! @{
    static OSStatus GetBeatAndTempoCallback(void* inHostUserData, Float64* outCurrentBeat, Float64* outCurrentTempo);
    static OSStatus GetMusicalTimeLocationCallback(void* inHostUserData, UInt32* outDeltaSampleOffsetToNextBeat,
                                                   Float32* outTimeSigNumerator, UInt32* outTimeSigDenominator,
                                                   Float64* outCurrentMeasureDownBeat);
    static OSStatus GetTransportStateCallback(void* inHostUserData, Boolean* outIsPlaying, Boolean* outTransportStateChanged,
                                              Float64* outCurrentSampleInTimeLine, Boolean* outIsCycling,
                                              Float64* outCycleStartBeat, Float64* outCycleEndBeat);
    static OSStatus GetTransportState2Callback(void* inHostUserData, Boolean* outIsPlaying, Boolean* outIsRecording,
                                               Boolean* outTransportStateChanged, Float64* outCurrentSampleInTimeLine,
                                               Boolean* outIsCycling, Float64* outCycleStartBeat, Float64* outCycleEndBeat);
    //! @}

    OSStatus GetTransportState(Boolean* outIsPlaying, Boolean* outIsRecording, Boolean* outTransportStateChanged,
                               Float64* outCurrentSampleInTimeLine, Boolean* outIsCycling, Float64* outCycleStartBeat,
                               Float64* outCycleEndBeat) const;

    //! Audacity has no tempo map, so a steady default is reported to plug-ins
    //! that insist on a musical timeline
    static constexpr double sDefaultTempo = 120.0;
    static constexpr double sDefaultBeatsPerBar = 4.0;

    SampleCount GetLatency(const EffectSettings& settings, double sampleRate)
    const override;

    size_t GetBlockSize() const override;
    size_t SetBlockSize(size_t maxBlockSize) override;

    unsigned GetAudioInCount() const override;
    unsigned GetAudioOutCount() const override;

    bool ProcessInitialize(EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;
    bool ProcessFinalize() noexcept override;
    std::string GetLastError() const override;
    size_t ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen)
    override;

    bool RealtimeInitialize(EffectSettings& settings, double sampleRate, size_t audioThreadBufferSize)
    override;
    bool RealtimeAddProcessor(EffectSettings& settings, EffectOutputs* pOutputs, unsigned numChannels, float sampleRate) override;
    bool RealtimeFinalize(EffectSettings& settings) noexcept override;
    bool RealtimeSuspend() override;
    bool RealtimeResume() override;

    bool UsesMessages() const noexcept override;
    bool RealtimeProcessStart(MessagePackage& package) override;
    size_t RealtimeProcess(size_t group, EffectSettings& settings, const float* const* inbuf, float* const* outbuf, size_t numSamples)
    override;
    bool RealtimeProcessEnd(EffectSettings& settings) noexcept override;

    static OSStatus RenderCallback(void* inRefCon, AudioUnitRenderActionFlags* inActionFlags, const AudioTimeStamp* inTimeStamp,
                                   UInt32 inBusNumber, UInt32 inNumFrames, AudioBufferList* ioData);
    OSStatus Render(AudioUnitRenderActionFlags* inActionFlags, const AudioTimeStamp* inTimeStamp, UInt32 inBusNumber, UInt32 inNumFrames,
                    AudioBufferList* ioData);

    bool BypassEffect(bool bypass);

private:
    //! Whether the master instance is now allocated to a group number
    bool mRecruited{ false };
    std::vector<std::unique_ptr<AudioUnitInstance> > mSlaves;

    AudioUnitCleanup<AudioUnit, AudioUnitUninitialize> mInitialization;
    AudioTimeStamp mTimeStamp{};
    PackedArray::Ptr<AudioBufferList> mInputList;
    PackedArray::Ptr<AudioBufferList> mOutputList;

    const wxString& mIdentifier; // for debug messages only
    const size_t mBlockSize;
    const bool mUseLatency;
    double mInitializedSampleRate = 0.0;
    std::string mLastError;
    std::atomic<bool> mRealtimeErrorReported{ false };

    //! @name Transport state published through the host callbacks
    //! Written by the thread that drives processing, but the plug-in may poll
    //! it from another thread of its own, so these are atomic.
    //! @{
    std::atomic<bool> mTransportPlaying{ false };
    //! True for the whole of the block in which a transition took effect
    std::atomic<bool> mTransportChanged{ false };
    //! Frames since processing began; Audacity does not give the effect stack
    //! the project time, so this timeline starts at zero for each playback
    std::atomic<double> mTransportSampleTime{ 0.0 };
    //! @}

    //! Silence in, and somewhere for the plug-in to write, for the final block
    //! that NotifyTransportStopped renders; sized once, so that finalizing
    //! allocates nothing
    std::vector<float> mStopBlock;
};
#endif
