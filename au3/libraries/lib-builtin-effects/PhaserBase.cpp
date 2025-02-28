/**********************************************************************

  Audacity: A Digital Audio Editor

  PhaserBase.cpp

  Nasca Octavian Paul (Paul Nasca)

*******************************************************************//**

\class PhaserBase
\brief An Effect that changes frequencies in a time varying manner.

*//*******************************************************************/
#include "PhaserBase.h"
#include "EffectInterface.h"

const EffectParameterMethods& PhaserBase::Parameters() const
{
    static CapturedParameters<
        PhaserBase, Stages, DryWet, Freq, Phase, Depth, Feedback, OutGain>
    parameters {
        [](PhaserBase&, EffectSettings&, EffectPhaserSettings& e,
           bool updating) {
            if (updating) {
                e.mStages &= ~1; // must be even, but don't complain about it
            }
            return true;
        },
    };
    return parameters;
}

//
#define phaserlfoshape 4.0

// How many samples are processed before recomputing the lfo value again
#define lfoskipsamples 20

//
// PhaserBase
//

const ComponentInterfaceSymbol PhaserBase::Symbol { XO("Phaser") };

std::shared_ptr<EffectInstance> PhaserBase::MakeInstance() const
{
    return std::make_shared<Instance>(*this);
}

PhaserBase::PhaserBase()
{
    SetLinearEffectFlag(true);
}

PhaserBase::~PhaserBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol PhaserBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString PhaserBase::GetDescription() const
{
    return XO("Combines phase-shifted signals with the original signal");
}

ManualPageID PhaserBase::ManualPage() const
{
    return L"Phaser";
}

// EffectDefinitionInterface implementation

EffectType PhaserBase::GetType() const
{
    return EffectTypeProcess;
}

auto PhaserBase::RealtimeSupport() const -> RealtimeSince
{
    return RealtimeSince::After_3_1;
}

unsigned PhaserBase::Instance::GetAudioInCount() const
{
    return 1;
}

unsigned PhaserBase::Instance::GetAudioOutCount() const
{
    return 1;
}

bool PhaserBase::Instance::ProcessInitialize(
    EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
    InstanceInit(settings, mState, sampleRate);
    if (chanMap[0] == ChannelNameFrontRight) {
        mState.phase += M_PI;
    }
    return true;
}

size_t PhaserBase::Instance::ProcessBlock(
    EffectSettings& settings, const float* const* inBlock,
    float* const* outBlock, size_t blockLen)
{
    return InstanceProcess(settings, mState, inBlock, outBlock, blockLen);
}

bool PhaserBase::Instance::RealtimeInitialize(EffectSettings&, double)
{
    SetBlockSize(512);
    mSlaves.clear();
    return true;
}

bool PhaserBase::Instance::RealtimeAddProcessor(
    EffectSettings& settings, EffectOutputs*, unsigned, float sampleRate)
{
    PhaserBase::Instance slave(mProcessor);

    InstanceInit(settings, slave.mState, sampleRate);

    mSlaves.push_back(slave);

    return true;
}

bool PhaserBase::Instance::RealtimeFinalize(EffectSettings&) noexcept
{
    mSlaves.clear();

    return true;
}

size_t PhaserBase::Instance::RealtimeProcess(
    size_t group, EffectSettings& settings, const float* const* inbuf,
    float* const* outbuf, size_t numSamples)
{
    if (group >= mSlaves.size()) {
        return 0;
    }
    return InstanceProcess(
        settings, mSlaves[group].mState, inbuf, outbuf, numSamples);
}

// PhaserBase implementation

void PhaserBase::Instance::InstanceInit(
    EffectSettings& settings, EffectPhaserState& data, float sampleRate)
{
    auto& ms = GetSettings(settings);

    data.samplerate = sampleRate;

    for (int j = 0; j < ms.mStages; j++) {
        data.old[j] = 0;
    }

    data.skipcount = 0;
    data.gain = 0;
    data.fbout = 0;
    data.laststages = 0;
    data.outgain = 0;

    return;
}

size_t PhaserBase::Instance::InstanceProcess(
    EffectSettings& settings, EffectPhaserState& data,
    const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
    auto& ms = GetSettings(settings);

    const float* ibuf = inBlock[0];
    float* obuf = outBlock[0];

    for (int j = data.laststages; j < ms.mStages; j++) {
        data.old[j] = 0;
    }
    data.laststages = ms.mStages;

    data.lfoskip = ms.mFreq * 2 * M_PI / data.samplerate;
    data.phase = ms.mPhase * M_PI / 180;
    data.outgain = DB_TO_LINEAR(ms.mOutGain);

    for (decltype(blockLen) i = 0; i < blockLen; i++) {
        double in = ibuf[i];

        double m
            =in + data.fbout * ms.mFeedback
              / 101;  // Feedback must be less than 100% to avoid infinite gain.

        if (((data.skipcount++) % lfoskipsamples) == 0) {
            // compute sine between 0 and 1
            data.gain
                =(1.0
                  + cos(data.skipcount.as_double() * data.lfoskip + data.phase))
                  / 2.0;

            // change lfo shape
            data.gain = expm1(data.gain * phaserlfoshape) / expm1(phaserlfoshape);

            // attenuate the lfo
            data.gain = 1.0 - data.gain / 255.0 * ms.mDepth;
        }

        // phasing routine
        for (int j = 0; j < ms.mStages; j++) {
            double tmp = data.old[j];
            data.old[j] = data.gain * tmp + m;
            m = tmp - data.gain * data.old[j];
        }
        data.fbout = m;

        obuf[i]
            =(float)(data.outgain * (m * ms.mDryWet + in * (255 - ms.mDryWet)) / 255);
    }

    return blockLen;
}
