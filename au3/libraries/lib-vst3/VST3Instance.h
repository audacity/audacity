#pragma once

#include <public.sdk/source/vst/utility/uid.h>

#include "PerTrackEffect.h"

namespace Steinberg {
class IPlugView;
class IPlugFrame;

namespace Vst {
class IEditController;
class IComponent;
}
}

class NumericTextCtrl;
class VST3ParametersWindow;

namespace VST3 {
namespace Hosting {
class ClassInfo;
class Module;
}
}

class VST3Wrapper;
class VST3Effect;

class VST3_API VST3Instance : public PerTrackEffect::Instance
{
    std::unique_ptr<VST3Wrapper> mWrapper;

    size_t mUserBlockSize { 8192 };
    size_t mProcessingBlockSize { 8192 };
    bool mUseLatency { true };
    sampleCount mInitialDelay { 0 };

    bool mRecruited{ false };
    std::vector<std::unique_ptr<VST3Instance> > mProcessors;

public:
    VST3Instance(const PerTrackEffect& effect, VST3::Hosting::Module& module, const VST3::Hosting::ClassInfo& effectClassInfo);
    ~VST3Instance() override;

    VST3Instance(const VST3Instance&) = delete;
    VST3Instance& operator=(const VST3Instance&) = delete;

    size_t GetTailSize() const override;
    bool RealtimeAddProcessor(EffectSettings& settings, EffectOutputs* pOutputs, unsigned numChannels, float sampleRate) override;
    bool RealtimeFinalize(EffectSettings& settings) noexcept override;
    bool RealtimeInitialize(EffectSettings& settings, double sampleRate) override;
    bool RealtimeProcessStart(MessagePackage& package) override;
    size_t RealtimeProcess(size_t group, EffectSettings& settings, const float* const* inBuf, float* const* outBuf,
                           size_t numSamples) override;
    bool RealtimeProcessEnd(EffectSettings& settings) noexcept override;
    bool RealtimeResume() override;
    bool RealtimeSuspend() override;
    SampleCount GetLatency(const EffectSettings& settings, double sampleRate)
    const override;
    bool ProcessFinalize() noexcept override;
    bool ProcessInitialize(EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;
    size_t GetBlockSize() const override;
    size_t SetBlockSize(size_t maxBlockSize) override;
    size_t ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

    VST3Wrapper& GetWrapper();

    unsigned GetAudioOutCount() const override;
    unsigned GetAudioInCount() const override;

    void ReloadUserOptions();

    //! NOTE For AU4
    Steinberg::IPtr<Steinberg::Vst::IEditController> vstEditController() const;
    Steinberg::IPtr<Steinberg::Vst::IComponent> effectComponent() const;
};
