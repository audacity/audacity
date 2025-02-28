/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaInstance.h

  Dominic Mazzoni
  Paul Licameli split from LadspaEffect.cpp

**********************************************************************/
#ifndef __AUDACITY_LADSPA_INSTANCE__
#define __AUDACITY_LADSPA_INSTANCE__

#include "PerTrackEffect.h"
#include "ladspa.h"

struct LadspaEffectSettings {
    explicit LadspaEffectSettings(size_t nPorts = 0)
        : controls(nPorts)
    {}

    // Allocate as many slots as there are ports, although some may correspond
    // to audio, not control, ports and so rest unused
    std::vector<float> controls;
};

//! Carry output control port information back to main thread
struct LADSPA_API LadspaEffectOutputs : EffectOutputs {
    ~LadspaEffectOutputs() override;
    std::unique_ptr<EffectOutputs> Clone() const override;

    void Assign(EffectOutputs&& src) override;

    // Allocate as many slots as there are ports, although some may correspond
    // to input and audio ports and remain unused
    std::vector<float> controls;
};

struct LADSPA_API LadspaInstance : PerTrackEffect::Instance, EffectInstanceWithBlockSize
{
    //! Get the preference for using latency
    static bool LoadUseLatency(
        const EffectDefinitionInterface& effect);
    //! Set the preference for using latency
    static bool SaveUseLatency(
        const EffectDefinitionInterface& effect, bool value);

    //! Assume settings originated from MakeSettings() and copies thereof
    static inline LadspaEffectSettings& GetSettings(EffectSettings& settings)
    {
        auto pSettings = settings.cast<LadspaEffectSettings>();
        assert(pSettings);
        return *pSettings;
    }

    //! Assume settings originated from MakeSettings() and copies thereof
    static inline const LadspaEffectSettings&
    GetSettings(const EffectSettings& settings)
    {
        return GetSettings(const_cast<EffectSettings&>(settings));
    }

    LadspaInstance(const PerTrackEffect& processor, const LADSPA_Descriptor* pData, const ArrayOf<unsigned long>& inputPorts,
                   const ArrayOf<unsigned long>& outputPorts, unsigned audioIns, unsigned audioOuts, int latencyPort);
    bool ProcessInitialize(EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;
    bool ProcessFinalize() noexcept override;
    size_t ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen)
    override;

    SampleCount GetLatency(const EffectSettings& settings, double sampleRate)
    const override;

    bool RealtimeInitialize(EffectSettings& settings, double sampleRate)
    override;
    bool RealtimeAddProcessor(EffectSettings& settings, EffectOutputs* pOutputs, unsigned numChannels, float sampleRate)
    override;
    bool RealtimeSuspend() override;
    bool RealtimeResume() override;
    bool RealtimeProcessStart(MessagePackage& package) override;
    size_t RealtimeProcess(size_t group, EffectSettings& settings, const float* const* inBuf, float* const* outBuf, size_t numSamples)
    override;
    bool RealtimeProcessEnd(EffectSettings& settings) noexcept override;
    bool RealtimeFinalize(EffectSettings& settings) noexcept override;

    unsigned GetAudioInCount() const override;
    unsigned GetAudioOutCount() const override;

    LADSPA_Handle InitInstance(
        float sampleRate, LadspaEffectSettings& settings, LadspaEffectOutputs* pOutputs) const;
    void FreeInstance(LADSPA_Handle handle) const;

    const LADSPA_Descriptor* const mData;
    const ArrayOf<unsigned long>& mInputPorts;
    const ArrayOf<unsigned long>& mOutputPorts;

    bool mReady{ false };
    LADSPA_Handle mMaster{};

    // Realtime processing
    std::vector<LADSPA_Handle> mSlaves;

    const unsigned mAudioIns;
    const unsigned mAudioOuts;
    const int mLatencyPort;
    const bool mUseLatency;
};

#endif
