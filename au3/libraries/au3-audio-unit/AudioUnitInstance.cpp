/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitInstance.cpp

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.cpp

**********************************************************************/

#include "AudioUnitInstance.h"

#include <AudioToolbox/AudioUnitUtilities.h>
#include <dispatch/dispatch.h>
#include <algorithm>
#include <cmath>
#include "au3-basic-ui/BasicUI.h"
#include "au3-exceptions/AudacityException.h"
#include <wx/log.h>

namespace {
struct AudioUnitMessage : EffectInstance::Message {
    explicit AudioUnitMessage(AudioUnitEffectSettings settings)
        : settings{std::move(settings)}
    {}
    ~AudioUnitMessage() override;
    std::unique_ptr<Message> Clone() const override;
    void Assign(Message&& src) override;
    void Merge(Message&& src) override;

    AudioUnitEffectSettings settings;
};
}

AudioUnitMessage::~AudioUnitMessage() = default;

auto AudioUnitMessage::Clone() const -> std::unique_ptr<Message>
{
    return std::make_unique<AudioUnitMessage>(*this);
}

void AudioUnitMessage::Assign(Message&& src)
{
    auto& dstSettings = this->settings;
    auto& srcSettings = static_cast<AudioUnitMessage&>(src).settings;
    AudioUnitWrapper::MoveSettingsContents(
        std::move(srcSettings), dstSettings, false);
}

void AudioUnitMessage::Merge(Message&& src)
{
    auto& dstSettings = this->settings;
    auto& srcSettings = static_cast<AudioUnitMessage&>(src).settings;
    AudioUnitWrapper::MoveSettingsContents(
        std::move(srcSettings), dstSettings, true);
}

AudioUnitInstance::AudioUnitInstance(const PerTrackEffect& effect,
                                     AudioComponent component, Parameters& parameters,
                                     const wxString& identifier,
                                     unsigned audioIns, unsigned audioOuts, bool useLatency)
    : PerTrackEffect::Instance{effect}
    , AudioUnitWrapper{component, &parameters}
    , mIdentifier{identifier}
    , mBlockSize{InitialBlockSize()}
    , mUseLatency{useLatency}
{
    mAudioIns = audioIns;
    mAudioOuts = audioOuts;
    CreateAudioUnit();
    SetHostCallbacks();
}

AudioUnitInstance::~AudioUnitInstance()
{
    // Tearing an instance down is main-thread work: for an out-of-process
    // plug-in AudioUnitUninitialize dismantles the shared render pipe, and
    // disposing a plug-in removes the run loop timers it registered when it
    // was created. Both trip over a thread that owns neither. The destructor
    // itself can run on any thread, so hand the work to the main queue
    // instead of doing it here. Both handles are released so the base
    // ~AudioUnitWrapper does not dispose a second time.
    const bool wasInitialized = mInitialization.release() != nullptr;
    AudioUnit unit = mUnit.release();
    if (!unit) {
        return;
    }

    dispatch_async(dispatch_get_main_queue(), ^ {
        if (wasInitialized) {
            AudioUnitUninitialize(unit);
        }
        AudioComponentInstanceDispose(unit);
    });
}

size_t AudioUnitInstance::InitialBlockSize() const
{
    // Retrieve the desired number of frames per slice
    UInt32 blockSize{};
    if (GetFixedSizeProperty(
            kAudioUnitProperty_MaximumFramesPerSlice, blockSize)) {
        // Call failed?  Then supply a default:
        return 512;
    } else {
        return blockSize;
    }
}

size_t AudioUnitInstance::SetBlockSize(size_t)
{
    // Ignore the argument!  Too-large block sizes won't work
    return mBlockSize;
}

size_t AudioUnitInstance::GetBlockSize() const
{
    return mBlockSize;
}

unsigned AudioUnitInstance::GetAudioInCount() const
{
    return mAudioIns;
}

unsigned AudioUnitInstance::GetAudioOutCount() const
{
    return mAudioOuts;
}

auto AudioUnitInstance::GetLatency(
    const EffectSettings&, double sampleRate) const -> SampleCount
{
    // Retrieve the latency (can be updated via an event)
    if (mUseLatency) {
        Float64 latency = 0.0;
        if (!GetFixedSizeProperty(kAudioUnitProperty_Latency, latency)) {
            return latency * sampleRate;
        }
    }
    return 0;
}

#if 0
size_t AudioUnitInstance::GetTailSize() const
{
    // Retrieve the tail time
    Float64 tailTime = 0.0;
    if (!GetFixedSizeProperty(kAudioUnitProperty_TailTime, tailTime)) {
        return tailTime * mSampleRate;
    }
    return 0;
}

#endif

bool AudioUnitInstance::Initialize()
{
    assert(CFRunLoopGetCurrent() == CFRunLoopGetMain());

    if (mInitialization) {
        return true;
    }

    if (AudioUnitInitialize(mUnit.get())) {
        wxLogError("Couldn't initialize audio unit\n");
        return false;
    }

    mInitialization.reset(mUnit.get());
    return true;
}

bool AudioUnitInstance::ProcessInitialize(EffectSettings& settings,
                                          double sampleRate, ChannelNames)
{
    return InitializeProcessing(settings, sampleRate, false);
}

bool AudioUnitInstance::InitializeProcessing(EffectSettings& settings,
                                             double sampleRate, bool realtime)
{
    mLastError.clear();

    mInputList
        =PackedArray::AllocateCount<AudioBufferList>(mAudioIns)(mAudioIns);
    mOutputList
        =PackedArray::AllocateCount<AudioBufferList>(mAudioOuts)(mAudioOuts);

    memset(&mTimeStamp, 0, sizeof(AudioTimeStamp));
    mTimeStamp.mSampleTime = 0; // This is a double-precision number that should
                                // accumulate the number of frames processed so far
    mTimeStamp.mFlags = kAudioTimeStampSampleTimeValid;

    //! NOTE Every playback start calls this. Uninitializing and initializing
    //! the unit again resets the plug-in to the values it was last given,
    //! discarding whatever the user has since changed in its own editor, so
    //! only do it when the sample rate makes it necessary.
    const bool needsReinitialize = !mInitialization || mInitializedSampleRate != sampleRate;
    if (needsReinitialize) {
        mInitialization.reset();
        // Redo this with the correct sample rate, not the arbirary 44100 that the
        // effect used
        auto ins = mAudioIns;
        auto outs = mAudioOuts;
        if (!SetRateAndChannels(sampleRate, mIdentifier)) {
            return false;
        }

        // Must be set, not just queried: out-of-process plug-ins size their
        // shared render buffers from this property, and rendering without it
        // fails with kAudioUnitErr_RenderTimeout
        if (SetProperty(kAudioUnitProperty_MaximumFramesPerSlice,
                        static_cast<UInt32>(mBlockSize))) {
            wxLogError("%ls didn't accept maximum frames per slice\n", mIdentifier.wx_str());
            mLastError = TranslatableString("audio-unit",
                                            "The plugin “%1” does not support the required block size")
                         .arg(mProcessor.GetName()).Translation();
            return false;
        }

        // Reinstall the transport callbacks before the unit is initialized:
        // uninitializing may have dropped them along with the rest of its
        // state, and a plug-in that reads them once is likely to read them
        // here
        SetHostCallbacks();

        if (!Initialize()) {
            return false;
        }

        if (ins != mAudioIns || outs != mAudioOuts) {
            // A change of channels with changing rate?  This is unexpected!
            ins = mAudioIns;
            outs = mAudioOuts;
            return false;
        }

        if (SetProperty(kAudioUnitProperty_SetRenderCallback,
                        AudioUnitUtils::RenderCallback { RenderCallback, this },
                        kAudioUnitScope_Input)) {
            wxLogError("Setting input render callback failed.\n");
            return false;
        }

        mInitializedSampleRate = sampleRate;

        // The reinitialization above discarded the unit's parameter state and
        // only this method knows it did, so restore it from the stored
        // settings - and only now, because an out-of-process plug-in also
        // discards values set before AudioUnitInitialize. A unit that stayed
        // initialized is left alone: it is already processing with the values
        // the user set, which the stored copy may lag behind (see
        // RealtimeFinalize).
        if (!StoreSettings(mProcessor, GetSettings(settings))) {
            return false;
        }
    }

    //! NOTE Resetting discards the plug-in's state, which is right when it is
    //! starting from nothing but not when a realtime instance that the user
    //! keeps open in its own editor is merely beginning another playback: a
    //! plug-in that has been armed to capture audio loses that along with the
    //! stale tail this was meant to flush. So flush only for one-shot
    //! processing, or when the unit was just (re)initialized and so has
    //! nothing to lose anyway. Toggling the effect off and on still flushes,
    //! by way of BypassEffect.
    if ((!realtime || needsReinitialize)
        && AudioUnitReset(mUnit.get(), kAudioUnitScope_Global, 0)) {
        return false;
    }

    if (!BypassEffect(false)) {
        // Ignore bad return value.  Some (like Xfer OTT) give a bad status.
    }

    // Size the buffers for the final block now, so that finalizing - which
    // must not throw - allocates nothing
    try {
        mStopBlock.assign((mAudioIns + mAudioOuts) * mBlockSize, 0.0f);
    }
    catch (...) {
        mStopBlock.clear();
    }

    // Tell the plug-in that a transfer is beginning. Until this, and the
    // callbacks installed above, an AudioUnit in Audacity could not tell
    // playback from silence. Report the transition unconditionally rather
    // than through SetTransportPlaying, so that an instance left marked as
    // playing by an abandoned scope still sees a start here.
    mTransportSampleTime.store(0.0, std::memory_order_release);
    mTransportPlaying.store(true, std::memory_order_release);
    mTransportChanged.store(true, std::memory_order_release);

    return true;
}

bool AudioUnitInstance::ProcessFinalize() noexcept
{
    // Before the buffers go away, let the plug-in see the end of the transfer
    NotifyTransportStopped();
    mOutputList.reset();
    mInputList.reset();
    // Release the buffer without risking an allocation in a noexcept function
    std::vector<float>().swap(mStopBlock);
    return true;
}

std::string AudioUnitInstance::GetLastError() const
{
    if (!mLastError.empty()) {
        return mLastError;
    }

    // Realtime processing spreads channel groups over slave instances, so a
    // failure may have been recorded by any of them
    for (const auto& pSlave : mSlaves) {
        std::string slaveError = pSlave->GetLastError();
        if (!slaveError.empty()) {
            return slaveError;
        }
    }

    return {};
}

size_t AudioUnitInstance::ProcessBlock(EffectSettings&,
                                       const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
    // mAudioIns and mAudioOuts don't change after plugin initialization,
    // so ProcessInitialize() made sufficient allocations
    assert(Count(mInputList) >= mAudioIns);
    for (size_t i = 0; i < mAudioIns; ++i) {
        mInputList[i] = { 1, static_cast<UInt32>(sizeof(float) * blockLen),
                          const_cast<float*>(inBlock[i]) };
    }

    // See previous comment
    assert(Count(mOutputList) >= mAudioOuts);
    for (size_t i = 0; i < mAudioOuts; ++i) {
        mOutputList[i] = { 1, static_cast<UInt32>(sizeof(float) * blockLen),
                           outBlock[i] };
    }

    // Publish where this block sits in the timeline before rendering it: the
    // plug-in reads this from inside AudioUnitRender, through the transport
    // callbacks
    mTransportSampleTime.store(mTimeStamp.mSampleTime, std::memory_order_release);

    AudioUnitRenderActionFlags flags = 0;
    OSStatus result;

    result = AudioUnitRender(mUnit.get(),
                             &flags,
                             &mTimeStamp,
                             0,
                             blockLen,
                             mOutputList.get());

    // A transition is reported for the whole of the block it took effect in,
    // however many times the plug-in asks during that block, and no longer
    mTransportChanged.store(false, std::memory_order_release);

    if (result != noErr) {
        wxLogError("Render failed: %d %4.4s\n",
                   static_cast<int>(result), reinterpret_cast<char*>(&result));
        if (result == kAudioComponentErr_InstanceInvalidated
            || result == kAudioComponentErr_InstanceTimedOut) {
            mLastError = TranslatableString("audio-unit",
                                            "The plugin “%1” has crashed while processing audio")
                         .arg(mProcessor.GetName()).Translation();
        }
        return 0;
    }

    mTimeStamp.mSampleTime += blockLen;
    return blockLen;
}

bool AudioUnitInstance::RealtimeInitialize(
    EffectSettings& settings, double sampleRate, size_t)
{
    mRealtimeErrorReported.store(false);
    return InitializeProcessing(settings, sampleRate, true);
}

bool AudioUnitInstance::RealtimeAddProcessor(
    EffectSettings& settings, EffectOutputs*, unsigned, float sampleRate)
{
    if (!mRecruited) {
        // Assign self to the first processor
        mRecruited = true;
        return true;
    }

    // Assign another instance with independent state to other processors
    auto& effect = static_cast<const PerTrackEffect&>(mProcessor);
    auto uProcessor = std::make_unique<AudioUnitInstance>(effect,
                                                          mComponent, mParameters, mIdentifier,
                                                          mAudioIns, mAudioOuts, mUseLatency);
    uProcessor->SetBlockSize(mBlockSize);
    if (!uProcessor->InitializeProcessing(settings, sampleRate, true)) {
        return false;
    }
    mSlaves.push_back(move(uProcessor));
    return true;
}

bool AudioUnitInstance::RealtimeFinalize(EffectSettings& settings) noexcept
{
    return GuardedCall<bool>([&]{
        // Adopt the values the plug-in has been processing with, which include
        // whatever the user changed in its own editor. An instance without
        // messages (UsesMessages()) gets this for free: RealtimeEffectState::
        // Finalize copies the worker settings back into the main settings.
        // For a message-based instance like this one it skips that copy, so
        // the live values have to be brought over here, or the next instance
        // starts from the values these replace.
        FetchSettings(GetSettings(settings), true, true);

        for (auto& pSlave : mSlaves) {
            pSlave->ProcessFinalize();
        }
        mSlaves.clear();
        mRecruited = false;
        return ProcessFinalize();
    });
}

bool AudioUnitInstance::RealtimeSuspend()
{
    if (!BypassEffect(true)) {
        //return false
    }
    for (auto& pSlave : mSlaves) {
        if (!pSlave->BypassEffect(true)) {
            //return false
        }
    }
    return true;
}

bool AudioUnitInstance::RealtimeResume()
{
    if (!BypassEffect(false)) {
        //return false
    }
    for (auto& pSlave: mSlaves) {
        if (!pSlave->BypassEffect(false)) {
            //return false
        }
    }
    return true;
}

auto AudioUnitInstance::MakeMessage() const -> std::unique_ptr<Message>
{
    // Like AudioUnitEffect::MakeSettings, except it only allocates map entries
    // containing nullopt
    AudioUnitEffectSettings settings;
    FetchSettings(settings, false);
    return std::make_unique<AudioUnitMessage>(std::move(settings));
}

auto AudioUnitInstance::
MakeMessage(AudioUnitParameterID id, AudioUnitParameterValue value) const
-> std::unique_ptr<Message>
{
    AudioUnitEffectSettings settings;
    settings.values[id].emplace(wxString {}, value);
    return std::make_unique<AudioUnitMessage>(std::move(settings));
}

bool AudioUnitInstance::UsesMessages() const noexcept
{
    return true;
}

bool AudioUnitInstance::RealtimeProcessStart(MessagePackage& package)
{
    if (!package.pMessage) {
        return true;
    }
    auto& values = static_cast<AudioUnitMessage*>(package.pMessage)
                   ->settings.values;
    auto storeSettings = [&](AudioUnitInstance& instance){
        for (auto&[ID, oPair] : values) {
            if (oPair.has_value()) {
                auto value = oPair->second;
                if (AudioUnitSetParameter(mUnit.get(), ID,
                                          kAudioUnitScope_Global, 0, value, 0)) {
                    // Probably failed because of an invalid parameter when
                    // a plug-in is in a certain mode that doesn't contain
                    // the parameter.  Ignore the failure
                }
            }
        }
    };
    storeSettings(*this);
    for (auto& pSlave : mSlaves) {
        storeSettings(*pSlave);
    }

    // Consume the settings change so we don't repeat setting of parameters
    // until more inter-thread messages arrive
    for (auto&[_, oPair] : values) {
        oPair.reset();
    }

    return true;
}

size_t
AudioUnitInstance::RealtimeProcess(size_t group, EffectSettings& settings,
                                   const float* const* inbuf, float* const* outbuf, size_t numSamples)
{
    wxASSERT(numSamples <= mBlockSize);
    // Interpret the group number consistently with RealtimeAddProcessor
    if (!mRecruited) {
        return 0;
    }
    decltype(this) pSlave{};
    if (group == 0) {
        pSlave = this;
    } else if (--group < mSlaves.size()) {
        pSlave = mSlaves[group].get();
    }
    if (pSlave) {
        const auto processed = pSlave->ProcessBlock(settings, inbuf, outbuf, numSamples);
        if (processed == 0 && !pSlave->mLastError.empty()
            && !mRealtimeErrorReported.exchange(true)) {
            const auto message = pSlave->mLastError;
            BasicUI::CallAfter([message]{
                BasicUI::ShowErrorDialog(
                    {}, TranslatableString("audio-unit", "Realtime effect error"),
                    TranslatableString::untranslatable(message.c_str()), {});
            });
        }
        return processed;
    } else {
        return 0;
    }
}

bool AudioUnitInstance::RealtimeProcessEnd(EffectSettings&) noexcept
{
    return true;
}

OSStatus AudioUnitInstance::Render(
    AudioUnitRenderActionFlags* inActionFlags,
    const AudioTimeStamp* inTimeStamp,
    UInt32 inBusNumber, UInt32 inNumFrames, AudioBufferList* ioData)
{
    size_t i = 0;
    auto size = std::min<size_t>(ioData->mNumberBuffers, Count(mInputList));
    for (; i < size; ++i) {
        ioData->mBuffers[i].mData = mInputList[i].mData;
    }
    // Some defensive code here just in case SDK requests from us an unexpectedly
    // large number of buffers:
    for (; i < ioData->mNumberBuffers; ++i) {
        ioData->mBuffers[i].mData = nullptr;
    }
    return 0;
}

// static
OSStatus AudioUnitInstance::RenderCallback(void* inRefCon,
                                           AudioUnitRenderActionFlags* inActionFlags,
                                           const AudioTimeStamp* inTimeStamp,
                                           UInt32 inBusNumber, UInt32 inNumFrames, AudioBufferList* ioData)
{
    return static_cast<AudioUnitInstance*>(inRefCon)->Render(inActionFlags,
                                                             inTimeStamp, inBusNumber, inNumFrames, ioData);
}

bool AudioUnitInstance::SetHostCallbacks()
{
    // Without this property a plug-in has no way to learn the transport state.
    // AudioUnitRender says nothing about whether the host is playing or where
    // the block belongs, and JUCE implements AudioPlayHead for AudioUnits
    // entirely on top of these callbacks, so a plug-in that waits for playback
    // to start before it does anything waits forever.
    if (!mUnit) {
        return false;
    }
    if (SetProperty(kAudioUnitProperty_HostCallbacks,
                    AudioUnitUtils::HostCallbacks {
        this,
        GetBeatAndTempoCallback,
        GetMusicalTimeLocationCallback,
        GetTransportStateCallback,
        GetTransportState2Callback
    })) {
        // Not fatal: a plug-in is free to refuse callbacks it will never use
        wxLogMessage("%ls did not accept the host transport callbacks\n",
                     mIdentifier.wx_str());
        return false;
    }
    return true;
}

void AudioUnitInstance::SetTransportPlaying(bool playing)
{
    if (mTransportPlaying.exchange(playing, std::memory_order_acq_rel)
        != playing) {
        mTransportChanged.store(true, std::memory_order_release);
    }
}

void AudioUnitInstance::NotifyTransportStopped() noexcept
{
    if (!mTransportPlaying.load(std::memory_order_acquire)) {
        return;
    }
    SetTransportPlaying(false);

    // Nothing left to render through, so the flag alone will have to do
    if (!mUnit || !mInitialization || !mInputList || !mOutputList
        || mStopBlock.size() < (mAudioIns + mAudioOuts) * mBlockSize) {
        mTransportChanged.store(false, std::memory_order_release);
        return;
    }

    // One more block, of silence, with the transport now reported as stopped.
    // A plug-in that has been accumulating what it is fed sees the end of the
    // transfer here and can start doing whatever it does with the audio.
    auto* const data = mStopBlock.data();
    const auto blockBytes = static_cast<UInt32>(sizeof(float) * mBlockSize);
    std::fill_n(data, mAudioIns * mBlockSize, 0.0f);
    for (size_t i = 0; i < mAudioIns; ++i) {
        mInputList[i] = { 1, blockBytes, data + i * mBlockSize };
    }
    for (size_t i = 0; i < mAudioOuts; ++i) {
        mOutputList[i]
            = { 1, blockBytes, data + (mAudioIns + i) * mBlockSize };
    }

    mTransportSampleTime.store(mTimeStamp.mSampleTime,
                               std::memory_order_release);
    AudioUnitRenderActionFlags flags = 0;
    const auto result = AudioUnitRender(mUnit.get(), &flags, &mTimeStamp, 0,
                                        static_cast<UInt32>(mBlockSize),
                                        mOutputList.get());
    if (result != noErr) {
        // The output is discarded anyway; a plug-in that has already gone away
        // just means there is nobody left to tell
        wxLogMessage("Final stopped block failed: %d %4.4s\n",
                     static_cast<int>(result), reinterpret_cast<const char*>(&result));
    } else {
        mTimeStamp.mSampleTime += mBlockSize;
    }
    mTransportChanged.store(false, std::memory_order_release);
}

OSStatus AudioUnitInstance::GetTransportState(
    Boolean* outIsPlaying, Boolean* outIsRecording,
    Boolean* outTransportStateChanged, Float64* outCurrentSampleInTimeLine,
    Boolean* outIsCycling, Float64* outCycleStartBeat,
    Float64* outCycleEndBeat) const
{
    if (outIsPlaying) {
        *outIsPlaying = mTransportPlaying.load(std::memory_order_acquire);
    }
    if (outIsRecording) {
        // Audacity does not put the effect stack in the recording path
        *outIsRecording = false;
    }
    if (outTransportStateChanged) {
        *outTransportStateChanged
            = mTransportChanged.load(std::memory_order_acquire);
    }
    if (outCurrentSampleInTimeLine) {
        *outCurrentSampleInTimeLine
            = mTransportSampleTime.load(std::memory_order_acquire);
    }
    // Audacity's loop region is not visible from here
    if (outIsCycling) {
        *outIsCycling = false;
    }
    if (outCycleStartBeat) {
        *outCycleStartBeat = 0.0;
    }
    if (outCycleEndBeat) {
        *outCycleEndBeat = 0.0;
    }
    return noErr;
}

// static
OSStatus AudioUnitInstance::GetTransportStateCallback(void* inHostUserData,
                                                      Boolean* outIsPlaying, Boolean* outTransportStateChanged,
                                                      Float64* outCurrentSampleInTimeLine, Boolean* outIsCycling,
                                                      Float64* outCycleStartBeat, Float64* outCycleEndBeat)
{
    if (!inHostUserData) {
        return kAudioUnitErr_InvalidParameter;
    }
    return static_cast<const AudioUnitInstance*>(inHostUserData)
           ->GetTransportState(outIsPlaying, nullptr, outTransportStateChanged,
                               outCurrentSampleInTimeLine, outIsCycling,
                               outCycleStartBeat, outCycleEndBeat);
}

// static
OSStatus AudioUnitInstance::GetTransportState2Callback(void* inHostUserData,
                                                       Boolean* outIsPlaying, Boolean* outIsRecording,
                                                       Boolean* outTransportStateChanged, Float64* outCurrentSampleInTimeLine,
                                                       Boolean* outIsCycling, Float64* outCycleStartBeat,
                                                       Float64* outCycleEndBeat)
{
    if (!inHostUserData) {
        return kAudioUnitErr_InvalidParameter;
    }
    return static_cast<const AudioUnitInstance*>(inHostUserData)
           ->GetTransportState(outIsPlaying, outIsRecording,
                               outTransportStateChanged, outCurrentSampleInTimeLine,
                               outIsCycling, outCycleStartBeat, outCycleEndBeat);
}

// static
OSStatus AudioUnitInstance::GetBeatAndTempoCallback(void* inHostUserData,
                                                    Float64* outCurrentBeat, Float64* outCurrentTempo)
{
    if (!inHostUserData) {
        return kAudioUnitErr_InvalidParameter;
    }
    const auto& instance
        = *static_cast<const AudioUnitInstance*>(inHostUserData);
    if (outCurrentTempo) {
        *outCurrentTempo = sDefaultTempo;
    }
    if (outCurrentBeat) {
        const auto rate = instance.mInitializedSampleRate;
        const auto samples
            = instance.mTransportSampleTime.load(std::memory_order_acquire);
        *outCurrentBeat
            = (rate > 0) ? samples / rate * (sDefaultTempo / 60.0) : 0.0;
    }
    return noErr;
}

// static
OSStatus AudioUnitInstance::GetMusicalTimeLocationCallback(
    void* inHostUserData, UInt32* outDeltaSampleOffsetToNextBeat,
    Float32* outTimeSigNumerator, UInt32* outTimeSigDenominator,
    Float64* outCurrentMeasureDownBeat)
{
    if (!inHostUserData) {
        return kAudioUnitErr_InvalidParameter;
    }
    const auto& instance
        = *static_cast<const AudioUnitInstance*>(inHostUserData);
    const auto rate = instance.mInitializedSampleRate;
    const double samplesPerBeat
        = (rate > 0) ? rate * 60.0 / sDefaultTempo : 0.0;
    const double beat = (samplesPerBeat > 0)
                        ? instance.mTransportSampleTime.load(std::memory_order_acquire)
                        / samplesPerBeat
                        : 0.0;
    if (outDeltaSampleOffsetToNextBeat) {
        *outDeltaSampleOffsetToNextBeat = (samplesPerBeat > 0)
                                          ? static_cast<UInt32>((std::ceil(beat) - beat) * samplesPerBeat)
                                          : 0;
    }
    if (outTimeSigNumerator) {
        *outTimeSigNumerator = static_cast<Float32>(sDefaultBeatsPerBar);
    }
    if (outTimeSigDenominator) {
        *outTimeSigDenominator = 4;
    }
    if (outCurrentMeasureDownBeat) {
        *outCurrentMeasureDownBeat
            = std::floor(beat / sDefaultBeatsPerBar) * sDefaultBeatsPerBar;
    }
    return noErr;
}

void AudioUnitInstance::EventListener(const AudioUnitEvent* inEvent,
                                      AudioUnitParameterValue inParameterValue)
{
    // Handle property changes
    if (inEvent->mEventType == kAudioUnitEvent_PropertyChange) {
        // Handle latency changes
        if (inEvent->mArgument.mProperty.mPropertyID
            == kAudioUnitProperty_Latency) {
            // Do what?
        }
        return;
    }

    if (inEvent->mEventType != kAudioUnitEvent_ParameterValueChange) {
        return;
    }

    // Only parameter changes at this point
    const auto parameterStorer = [inParameterValue,
                                  ID = inEvent->mArgument.mParameter.mParameterID
                                 ](AudioUnit pUnit){
        AudioUnitSetParameter(pUnit, ID,
                              kAudioUnitScope_Global, 0, inParameterValue, 0);
    };

    // Save the parameter change in the instance, so it can be
    // fetched into Settings, used to initialize any new slave's state
    // This is like StoreSettings but for just one setting
    parameterStorer(GetAudioUnit());

    // Propagate the parameter
    for (auto& worker : mSlaves) {
        parameterStorer(worker->GetAudioUnit());
    }
}

bool AudioUnitInstance::BypassEffect(bool bypass)
{
    UInt32 value = (bypass ? 1 : 0);
    if (bypass && AudioUnitReset(mUnit.get(), kAudioUnitScope_Global, 0)) {
        return false;
    }
    return !SetProperty(kAudioUnitProperty_BypassEffect, value);
}
