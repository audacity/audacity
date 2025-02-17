/**********************************************************************

  Audacity: A Digital Audio Editor

  WahwahBase.cpp

  Nasca Octavian Paul (Paul Nasca)

*******************************************************************//**

\class WahwahBase
\brief An Effect that adds a 'spectral glide'.

*//*******************************************************************/
#include "WahWahBase.h"

const EffectParameterMethods& WahWahBase::Parameters() const
{
    static CapturedParameters<
        WahWahBase, Freq, Phase, Depth, Res, FreqOfs, OutGain>
    parameters;
    return parameters;
}

// How many samples are processed before recomputing the lfo value again
#define lfoskipsamples 30

//
// WahWahBase
//

const ComponentInterfaceSymbol WahWahBase::Symbol { XO("Wahwah") };

std::shared_ptr<EffectInstance> WahWahBase::MakeInstance() const
{
    return std::make_shared<Instance>(*this);
}

WahWahBase::WahWahBase()
{
    SetLinearEffectFlag(true);
}

WahWahBase::~WahWahBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol WahWahBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString WahWahBase::GetDescription() const
{
    return XO(
        "Rapid tone quality variations, like that guitar sound so popular in the 1970's");
}

ManualPageID WahWahBase::ManualPage() const
{
    return L"Wahwah";
}

// EffectDefinitionInterface implementation

EffectType WahWahBase::GetType() const
{
    return EffectTypeProcess;
}

auto WahWahBase::RealtimeSupport() const -> RealtimeSince
{
    return RealtimeSince::After_3_1;
}

bool WahWahBase::Instance::ProcessInitialize(
    EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
    InstanceInit(settings, mState, sampleRate);
    if (chanMap[0] == ChannelNameFrontRight) {
        mState.phase += M_PI;
    }
    return true;
}

size_t WahWahBase::Instance::ProcessBlock(
    EffectSettings& settings, const float* const* inBlock,
    float* const* outBlock, size_t blockLen)
{
    return InstanceProcess(settings, mState, inBlock, outBlock, blockLen);
}

bool WahWahBase::Instance::RealtimeInitialize(EffectSettings&, double)
{
    SetBlockSize(512);
    mSlaves.clear();
    return true;
}

bool WahWahBase::Instance::RealtimeAddProcessor(
    EffectSettings& settings, EffectOutputs*, unsigned, float sampleRate)
{
    WahWahBase::Instance slave(mProcessor);

    InstanceInit(settings, slave.mState, sampleRate);

    mSlaves.push_back(slave);

    return true;
}

bool WahWahBase::Instance::RealtimeFinalize(EffectSettings&) noexcept
{
    mSlaves.clear();

    return true;
}

size_t WahWahBase::Instance::RealtimeProcess(
    size_t group, EffectSettings& settings, const float* const* inbuf,
    float* const* outbuf, size_t numSamples)
{
    if (group >= mSlaves.size()) {
        return 0;
    }
    return InstanceProcess(
        settings, mSlaves[group].mState, inbuf, outbuf, numSamples);
}

// WahWahBase implementation

void WahWahBase::Instance::InstanceInit(
    EffectSettings& settings, EffectWahwahState& data, float sampleRate)
{
    auto& ms = GetSettings(settings);

    data.samplerate = sampleRate;
    data.lfoskip = ms.mFreq * 2 * M_PI / sampleRate;
    data.skipcount = 0;
    data.xn1 = 0;
    data.xn2 = 0;
    data.yn1 = 0;
    data.yn2 = 0;
    data.b0 = 0;
    data.b1 = 0;
    data.b2 = 0;
    data.a0 = 0;
    data.a1 = 0;
    data.a2 = 0;

    data.depth = ms.mDepth / 100.0;
    data.freqofs = ms.mFreqOfs / 100.0;
    data.phase = ms.mPhase * M_PI / 180.0;
    data.outgain = DB_TO_LINEAR(ms.mOutGain);
}

size_t WahWahBase::Instance::InstanceProcess(
    EffectSettings& settings, EffectWahwahState& data,
    const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
    auto& ms = GetSettings(settings);

    const float* ibuf = inBlock[0];
    float* obuf = outBlock[0];
    double frequency, omega, sn, cs, alpha;
    double in, out;

    data.lfoskip = ms.mFreq * 2 * M_PI / data.samplerate;
    data.depth = ms.mDepth / 100.0;
    data.freqofs = ms.mFreqOfs / 100.0;

    data.phase = ms.mPhase * M_PI / 180.0;
    data.outgain = DB_TO_LINEAR(ms.mOutGain);

    for (decltype(blockLen) i = 0; i < blockLen; i++) {
        in = (double)ibuf[i];

        if ((data.skipcount++) % lfoskipsamples == 0) {
            frequency = (1 + cos(data.skipcount * data.lfoskip + data.phase)) / 2;
            frequency = frequency * data.depth * (1 - data.freqofs) + data.freqofs;
            frequency = exp((frequency - 1) * 6);
            omega = M_PI * frequency;
            sn = sin(omega);
            cs = cos(omega);
            alpha = sn / (2 * ms.mRes);
            data.b0 = (1 - cs) / 2;
            data.b1 = 1 - cs;
            data.b2 = (1 - cs) / 2;
            data.a0 = 1 + alpha;
            data.a1 = -2 * cs;
            data.a2 = 1 - alpha;
        }
        out = (data.b0 * in + data.b1 * data.xn1 + data.b2 * data.xn2
               - data.a1 * data.yn1 - data.a2 * data.yn2)
              / data.a0;
        data.xn2 = data.xn1;
        data.xn1 = in;
        data.yn2 = data.yn1;
        data.yn1 = out;
        out *= data.outgain;

        obuf[i] = (float)out;
    }

    return blockLen;
}

unsigned WahWahBase::Instance::GetAudioOutCount() const
{
    return 1;
}

unsigned WahWahBase::Instance::GetAudioInCount() const
{
    return 1;
}
