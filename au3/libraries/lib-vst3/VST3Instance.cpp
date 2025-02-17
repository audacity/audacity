#include "VST3Instance.h"

#include <public.sdk/source/vst/utility/uid.h>

#include "AudacityException.h"
#include "VST3Wrapper.h"
#include "ConfigInterface.h"
#include "VST3Utils.h"

namespace {
unsigned CountChannels(Steinberg::Vst::IComponent* component,
                       const Steinberg::Vst::MediaTypes mediaType,
                       const Steinberg::Vst::BusDirection busDirection,
                       const Steinberg::Vst::BusType busType)
{
    using namespace Steinberg;

    unsigned channelsCount{ 0 };

    const auto busCount = component->getBusCount(mediaType, busDirection);
    for (auto i = 0; i < busCount; ++i) {
        Vst::BusInfo busInfo;
        if (component->getBusInfo(mediaType, busDirection, i, busInfo) == kResultOk) {
            if (busInfo.busType == busType) {
                channelsCount += busInfo.channelCount;
            }
        }
    }
    return channelsCount;
}
}

VST3Instance::VST3Instance(const PerTrackEffect& effect, VST3::Hosting::Module& module, const VST3::Hosting::ClassInfo& effectClassInfo)
    : Instance(effect)
{
    ReloadUserOptions();
    mWrapper = std::make_unique<VST3Wrapper>(module, effectClassInfo);
    mWrapper->InitializeComponents();
}

VST3Instance::~VST3Instance() = default;

size_t VST3Instance::GetTailSize() const
{
    return Instance::GetTailSize();
}

bool VST3Instance::RealtimeAddProcessor(EffectSettings& settings,
                                        EffectOutputs*, unsigned, float sampleRate)
{
    if (!mRecruited) {
        // Assign self to the first processor
        mRecruited = true;
        return true;
    }
    // Assign another instance with independent state to other processors
    auto& effect = static_cast<const PerTrackEffect&>(mProcessor);
    auto uProcessor
        =std::make_unique<VST3Instance>(effect, mWrapper->GetModule(), mWrapper->GetEffectClassInfo());
    if (!uProcessor->RealtimeInitialize(settings, sampleRate)) {
        return false;
    }
    mProcessors.push_back(move(uProcessor));
    return true;
}

bool VST3Instance::RealtimeFinalize(EffectSettings& settings) noexcept
{
    return GuardedCall<bool>([&]{
        mRecruited = false;
        mWrapper->Finalize(&settings);
        for (auto& pProcessor : mProcessors) {
            pProcessor->mWrapper->Finalize(nullptr);
        }
        mProcessors.clear();
        return true;
    });
}

bool VST3Instance::RealtimeInitialize(EffectSettings& settings, double sampleRate)
{
    if (mWrapper->Initialize(settings, sampleRate, Steinberg::Vst::kRealtime, mProcessingBlockSize)) {
        mInitialDelay = mWrapper->GetLatencySamples();
        return true;
    }
    return false;
}

size_t VST3Instance::RealtimeProcess(size_t group, EffectSettings& settings, const float* const* inBuf,
                                     float* const* outBuf, size_t numSamples)
{
    if (!mRecruited) {
        // unexpected!
        return 0;
    }
    if (group == 0) {
        return mWrapper->Process(inBuf, outBuf, numSamples);
    } else if (--group < mProcessors.size()) {
        return mProcessors[group]->mWrapper->Process(inBuf, outBuf, numSamples);
    }
    return 0;
}

bool VST3Instance::RealtimeProcessEnd(EffectSettings& settings) noexcept
{
    return true;
}

bool VST3Instance::RealtimeProcessStart(MessagePackage& package)
{
    auto& settings = package.settings;
    mWrapper->ProcessBlockStart(settings);
    for (auto& pProcessor : mProcessors) {
        pProcessor->mWrapper->ProcessBlockStart(settings);
    }
    return true;
}

bool VST3Instance::RealtimeResume()
{
    mWrapper->ResumeProcessing();
    for (auto& pProcessor : mProcessors) {
        pProcessor->mWrapper->ResumeProcessing();
    }
    return true;
}

bool VST3Instance::RealtimeSuspend()
{
    mWrapper->SuspendProcessing();
    for (auto& pProcessor : mProcessors) {
        pProcessor->mWrapper->SuspendProcessing();
    }
    return true;
}

auto VST3Instance::GetLatency(const EffectSettings& settings, double sampleRate)
const -> SampleCount
{
    if (mUseLatency) {
        return mInitialDelay.as_long_long();
    }
    return 0;
}

bool VST3Instance::ProcessFinalize() noexcept
{
    return GuardedCall<bool>([&]
    {
        mWrapper->Finalize(nullptr);
        return true;
    });
}

bool VST3Instance::ProcessInitialize(EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
    if (mWrapper->Initialize(settings, sampleRate, Steinberg::Vst::kOffline, mProcessingBlockSize)) {
        mInitialDelay = mWrapper->GetLatencySamples();
        return true;
    }
    return false;
}

size_t VST3Instance::GetBlockSize() const
{
    return mProcessingBlockSize;
}

size_t VST3Instance::SetBlockSize(size_t maxBlockSize)
{
    mProcessingBlockSize
        =static_cast<Steinberg::int32>(std::min(maxBlockSize, mUserBlockSize));
    return mProcessingBlockSize;
}

size_t VST3Instance::ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock,
                                  size_t blockLen)
{
    return mWrapper->Process(inBlock, outBlock, blockLen);
}

VST3Wrapper& VST3Instance::GetWrapper()
{
    return *mWrapper;
}

unsigned VST3Instance::GetAudioInCount() const
{
    //setupProcessing should be called first
    return CountChannels(
        mWrapper->mEffectComponent,
        Steinberg::Vst::kAudio,
        Steinberg::Vst::kInput,
        Steinberg::Vst::kMain
        );
}

unsigned VST3Instance::GetAudioOutCount() const
{
    //setupProcessing should be called first
    return CountChannels(
        mWrapper->mEffectComponent,
        Steinberg::Vst::kAudio,
        Steinberg::Vst::kOutput,
        Steinberg::Vst::kMain
        );
}

void VST3Instance::ReloadUserOptions()
{
    // Reinitialize configuration settings
    int userBlockSize;
    GetConfig(mProcessor, PluginSettings::Shared, wxT("Options"),
              wxT("BufferSize"), userBlockSize, 8192);
    mUserBlockSize = std::max(1, userBlockSize);
    GetConfig(mProcessor, PluginSettings::Shared, wxT("Options"),
              wxT("UseLatency"), mUseLatency, true);

    SetBlockSize(mUserBlockSize);
}

Steinberg::IPtr<Steinberg::Vst::IEditController> VST3Instance::vstEditController() const
{
    return mWrapper->mEditController;
}

Steinberg::IPtr<Steinberg::Vst::IComponent> VST3Instance::effectComponent() const
{
    return mWrapper->mEffectComponent;
}
