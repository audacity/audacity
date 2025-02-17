/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaInstance.cpp

  Dominic Mazzoni

  Paul Licameli split from LadspaEffect.cpp

*//*******************************************************************/

#include "LadspaInstance.h"       // This class's header file
#include "ConfigInterface.h"
#include "AudacityException.h"

static const wchar_t* OptionsKey = L"Options";
static const wchar_t* UseLatencyKey = L"UseLatency";

bool LadspaInstance::LoadUseLatency(const EffectDefinitionInterface& effect)
{
    bool result{};
    GetConfig(effect, PluginSettings::Shared,
              OptionsKey, UseLatencyKey, result, true /* default value */);
    return result;
}

bool LadspaInstance::SaveUseLatency(
    const EffectDefinitionInterface& effect, bool value)
{
    return SetConfig(
        effect, PluginSettings::Shared, OptionsKey, UseLatencyKey, value);
}

LadspaEffectOutputs::~LadspaEffectOutputs() = default;

auto LadspaEffectOutputs::Clone() const -> std::unique_ptr<EffectOutputs>
{
    return std::make_unique<LadspaEffectOutputs>(*this);
}

void LadspaEffectOutputs::Assign(EffectOutputs&& src)
{
    // Don't really need to modify src
    const auto& srcValues = static_cast<LadspaEffectOutputs&>(src).controls;
    auto& dstValues = controls;
    assert(srcValues.size() == dstValues.size());
    copy(srcValues.begin(), srcValues.end(), dstValues.data());
}

namespace {
std::pair<float, float>
InputControlPortBounds(const LADSPA_PortRangeHint& hint, double sampleRate)
{
    // Find lower and upper bound values for ths hint
    const auto multiplier
        =LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor) ? sampleRate : 1.0;
    return { hint.LowerBound * multiplier, hint.UpperBound * multiplier };
}

float ClampInputControlValue(
    const LADSPA_PortRangeHint& hint, float val, float lower, float upper)
{
    if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor) && val < lower) {
        val = lower;
    }
    if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor) && val > upper) {
        val = upper;
    }
    return val;
}

float InputControlPortDefaultValue(
    const LADSPA_PortRangeHint& hint, double sampleRate)
{
    // See comments in library header ladspa.h about interpretation of macros
    const auto bounds = InputControlPortBounds(hint, sampleRate);

    // Function to find low, middle, or high default values
    const auto combine = [bounds,
                          logarithmic = LADSPA_IS_HINT_LOGARITHMIC(hint.HintDescriptor)
                         ](float lowWeight, float highWeight){
        auto [lower, upper] = bounds;
        return logarithmic
               ? exp(log(lower) * lowWeight + log(upper) * highWeight)
               : lower* lowWeight + upper* highWeight;
    };

    auto [lower, upper] = bounds;
    auto val = 1.0f;
    // Four bits of the descriptor describe mutually exclusive cases
    switch (hint.HintDescriptor & LADSPA_HINT_DEFAULT_MASK) {
    case LADSPA_HINT_DEFAULT_NONE:
    default:
        break;
    case LADSPA_HINT_DEFAULT_MINIMUM:
        val = lower;
        break;
    case LADSPA_HINT_DEFAULT_LOW:
        val = combine(0.75, 0.25);
        break;
    case LADSPA_HINT_DEFAULT_MIDDLE:
        val = combine(0.5, 0.5);
        break;
    case LADSPA_HINT_DEFAULT_HIGH:
        val = combine(0.25, 0.75);
        break;
    case LADSPA_HINT_DEFAULT_MAXIMUM:
        val = upper;
        break;
    case LADSPA_HINT_DEFAULT_0:
        val = 0.0f;
        break;
    case LADSPA_HINT_DEFAULT_1:
        val = 1.0f;
        break;
    case LADSPA_HINT_DEFAULT_100:
        val = 100.0f;
        break;
    case LADSPA_HINT_DEFAULT_440:
        val = 440.0f;
        break;
    }

    return ClampInputControlValue(hint, val, lower, upper);
}
}

LadspaInstance::LadspaInstance(const PerTrackEffect& processor,
                               const LADSPA_Descriptor* pData,
                               const ArrayOf<unsigned long>& inputPorts,
                               const ArrayOf<unsigned long>& outputPorts,
                               unsigned audioIns, unsigned audioOuts, int latencyPort)
    : PerTrackEffect::Instance{processor}
    , mData{pData}, mInputPorts{inputPorts}, mOutputPorts{outputPorts}
    , mAudioIns{audioIns}, mAudioOuts{audioOuts}
    , mLatencyPort{latencyPort}
    , mUseLatency{LoadUseLatency(processor)}
{
}

auto LadspaInstance::GetLatency(
    const EffectSettings& settings, double) const -> SampleCount
{
    auto& controls = GetSettings(settings).controls;
    if (mUseLatency && mLatencyPort >= 0) {
        return controls[mLatencyPort];
    }
    return 0;
}

bool LadspaInstance::ProcessInitialize(
    EffectSettings& settings, double sampleRate, ChannelNames)
{
    /* Instantiate the plugin */
    if (!mReady) {
        auto& ladspaSettings = GetSettings(settings);
        // Destructive effect processing doesn't need output ports
        mMaster = InitInstance(sampleRate, ladspaSettings, nullptr);
        if (!mMaster) {
            return false;
        }
        mReady = true;
    }
    return true;
}

bool LadspaInstance::ProcessFinalize() noexcept
{
    return GuardedCall<bool>([&]{
        if (mReady) {
            mReady = false;
            FreeInstance(mMaster);
            mMaster = nullptr;
        }

        return true;
    });
}

size_t LadspaInstance::ProcessBlock(EffectSettings&,
                                    const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
    for (unsigned i = 0; i < mAudioIns; ++i) {
        mData->connect_port(mMaster, mInputPorts[i],
                            const_cast<float*>(inBlock[i]));
    }

    for (unsigned i = 0; i < mAudioOuts; ++i) {
        mData->connect_port(mMaster, mOutputPorts[i], outBlock[i]);
    }

    mData->run(mMaster, blockLen);
    return blockLen;
}

bool LadspaInstance::RealtimeInitialize(EffectSettings&, double)
{
    return true;
}

bool LadspaInstance::RealtimeAddProcessor(
    EffectSettings& settings, EffectOutputs* pOutputs, unsigned, float sampleRate)
{
    auto& ladspaSettings = GetSettings(settings);
    // Connect to outputs only if this is the first processor for the track.
    // (What's right when a mono effect is on a stereo channel?  Unclear, but
    // this definitely causes connection with the first channel.)
    auto pLadspaOutputs = mSlaves.empty()
                          ? static_cast<LadspaEffectOutputs*>(pOutputs) : nullptr;
    auto slave = InitInstance(sampleRate, ladspaSettings, pLadspaOutputs);
    if (!slave) {
        return false;
    }
    mSlaves.push_back(slave);
    return true;
}

unsigned LadspaInstance::GetAudioOutCount() const
{
    return mAudioOuts;
}

unsigned LadspaInstance::GetAudioInCount() const
{
    return mAudioIns;
}

bool LadspaInstance::RealtimeFinalize(EffectSettings&) noexcept
{
    return GuardedCall<bool>([&]{
        for (size_t i = 0, cnt = mSlaves.size(); i < cnt; ++i) {
            FreeInstance(mSlaves[i]);
        }
        mSlaves.clear();

        return true;
    });
}

bool LadspaInstance::RealtimeSuspend()
{
    if (auto fn = mData->deactivate) {
        for (auto& slave : mSlaves) {
            fn(slave);
        }
    }
    return true;
}

bool LadspaInstance::RealtimeResume()
{
    if (auto fn = mData->activate) {
        for (auto& slave : mSlaves) {
            fn(slave);
        }
    }
    return true;
}

bool LadspaInstance::RealtimeProcessStart(MessagePackage&)
{
    return true;
}

size_t LadspaInstance::RealtimeProcess(size_t group, EffectSettings&,
                                       const float* const* inbuf, float* const* outbuf, size_t numSamples)
{
    if (group >= mSlaves.size()) {
        return 0;
    }

    for (unsigned i = 0; i < mAudioIns; ++i) {
        mData->connect_port(mSlaves[group], mInputPorts[i],
                            const_cast<float*>(inbuf[i]));
    }

    for (unsigned i = 0; i < mAudioOuts; ++i) {
        mData->connect_port(
            mSlaves[group], mOutputPorts[i], outbuf[i]);
    }

    mData->run(mSlaves[group], numSamples);

    return numSamples;
}

bool LadspaInstance::RealtimeProcessEnd(EffectSettings&) noexcept
{
    return true;
}

LADSPA_Handle LadspaInstance::InitInstance(
    float sampleRate, LadspaEffectSettings& settings,
    LadspaEffectOutputs* pOutputs) const
{
    /* Instantiate the plugin */
    LADSPA_Handle handle = mData->instantiate(mData, sampleRate);
    if (!handle) {
        return nullptr;
    }

    auto& controls = settings.controls;
    for (unsigned long p = 0; p < mData->PortCount; ++p) {
        LADSPA_PortDescriptor d = mData->PortDescriptors[p];
        if (LADSPA_IS_PORT_CONTROL(d)) {
            if (LADSPA_IS_PORT_INPUT(d)) {
                mData->connect_port(handle, p, &controls[p]);
            } else {
                static LADSPA_Data sink;
                mData->connect_port(handle, p,
                                    pOutputs ? &pOutputs->controls[p] : &sink);
            }
        }
    }
    if (mData->activate) {
        mData->activate(handle);
    }

    return handle;
}

void LadspaInstance::FreeInstance(LADSPA_Handle handle) const
{
    if (mData->deactivate) {
        mData->deactivate(handle);
    }

    mData->cleanup(handle);
}
