/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2016 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   BassTreble.cpp
   Steve Daulton

**********************************************************************/
#include "BassTrebleBase.h"
#include "ShuttleAutomation.h"

const EffectParameterMethods& BassTrebleBase::Parameters() const
{
    static CapturedParameters<BassTrebleBase, Bass, Treble, Gain, Link>
    parameters;
    return parameters;
}

const ComponentInterfaceSymbol BassTrebleBase::Symbol { XO("Bass and Treble") };

BassTrebleBase::BassTrebleBase()
{
    SetLinearEffectFlag(true);
}

BassTrebleBase::~BassTrebleBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol BassTrebleBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString BassTrebleBase::GetDescription() const
{
    return XO("Simple tone control effect");
}

ManualPageID BassTrebleBase::ManualPage() const
{
    return L"Bass_and_Treble";
}

// EffectDefinitionInterface implementation

EffectType BassTrebleBase::GetType() const
{
    return EffectTypeProcess;
}

auto BassTrebleBase::RealtimeSupport() const -> RealtimeSince
{
    return RealtimeSince::After_3_1;
}

unsigned BassTrebleBase::Instance::GetAudioInCount() const
{
    return 1;
}

unsigned BassTrebleBase::Instance::GetAudioOutCount() const
{
    return 1;
}

bool BassTrebleBase::Instance::ProcessInitialize(
    EffectSettings& settings, double sampleRate, ChannelNames)
{
    InstanceInit(settings, mState, sampleRate);
    return true;
}

size_t BassTrebleBase::Instance::ProcessBlock(
    EffectSettings& settings, const float* const* inBlock,
    float* const* outBlock, size_t blockLen)
{
    return InstanceProcess(settings, mState, inBlock, outBlock, blockLen);
}

bool BassTrebleBase::Instance::RealtimeInitialize(EffectSettings&, double)
{
    SetBlockSize(512);
    mSlaves.clear();
    return true;
}

bool BassTrebleBase::Instance::RealtimeAddProcessor(
    EffectSettings& settings, EffectOutputs* pOutputs, unsigned numChannels,
    float sampleRate)
{
    BassTrebleBase::Instance slave(mProcessor);

    InstanceInit(settings, slave.mState, sampleRate);

    mSlaves.push_back(slave);

    return true;
}

bool BassTrebleBase::Instance::RealtimeFinalize(EffectSettings&) noexcept
{
    mSlaves.clear();

    return true;
}

size_t BassTrebleBase::Instance::RealtimeProcess(
    size_t group, EffectSettings& settings, const float* const* inbuf,
    float* const* outbuf, size_t numSamples)
{
    if (group >= mSlaves.size()) {
        return 0;
    }
    return InstanceProcess(
        settings, mSlaves[group].mState, inbuf, outbuf, numSamples);
}

size_t BassTrebleBase::Instance::InstanceProcess(
    EffectSettings& settings, BassTrebleState& data, const float* const* inBlock,
    float* const* outBlock, size_t blockLen)
{
    auto& ms = GetSettings(settings);

    const float* ibuf = inBlock[0];
    float* obuf = outBlock[0];

    // Set value to ensure correct rounding
    double oldBass = DB_TO_LINEAR(ms.mBass);
    double oldTreble = DB_TO_LINEAR(ms.mTreble);

    data.gain = DB_TO_LINEAR(ms.mGain);

    // Compute coefficients of the low shelf biquand IIR filter
    if (data.bass != oldBass) {
        Coefficients(
            data.hzBass, data.slope, ms.mBass, data.samplerate, kBass, data.a0Bass,
            data.a1Bass, data.a2Bass, data.b0Bass, data.b1Bass, data.b2Bass);
    }

    // Compute coefficients of the high shelf biquand IIR filter
    if (data.treble != oldTreble) {
        Coefficients(
            data.hzTreble, data.slope, ms.mTreble, data.samplerate, kTreble,
            data.a0Treble, data.a1Treble, data.a2Treble, data.b0Treble,
            data.b1Treble, data.b2Treble);
    }

    for (decltype(blockLen) i = 0; i < blockLen; i++) {
        obuf[i] = DoFilter(data, ibuf[i]) * data.gain;
    }

    return blockLen;
}

void BassTrebleBase::Instance::Coefficients(
    double hz, double slope, double gain, double samplerate, int type,
    double& a0, double& a1, double& a2, double& b0, double& b1, double& b2)
{
    double w = 2 * M_PI * hz / samplerate;
    double a = exp(log(10.0) * gain / 40);
    double b = sqrt((a * a + 1) / slope - (pow((a - 1), 2)));

    if (type == kBass) {
        b0 = a * ((a + 1) - (a - 1) * cos(w) + b * sin(w));
        b1 = 2 * a * ((a - 1) - (a + 1) * cos(w));
        b2 = a * ((a + 1) - (a - 1) * cos(w) - b * sin(w));
        a0 = ((a + 1) + (a - 1) * cos(w) + b * sin(w));
        a1 = -2 * ((a - 1) + (a + 1) * cos(w));
        a2 = (a + 1) + (a - 1) * cos(w) - b * sin(w);
    } else { // assumed kTreble
        b0 = a * ((a + 1) + (a - 1) * cos(w) + b * sin(w));
        b1 = -2 * a * ((a - 1) + (a + 1) * cos(w));
        b2 = a * ((a + 1) + (a - 1) * cos(w) - b * sin(w));
        a0 = ((a + 1) - (a - 1) * cos(w) + b * sin(w));
        a1 = 2 * ((a - 1) - (a + 1) * cos(w));
        a2 = (a + 1) - (a - 1) * cos(w) - b * sin(w);
    }
}

float BassTrebleBase::Instance::DoFilter(BassTrebleState& data, float in)
{
    // Bass filter
    float out = (data.b0Bass * in + data.b1Bass * data.xn1Bass
                 + data.b2Bass * data.xn2Bass - data.a1Bass * data.yn1Bass
                 - data.a2Bass * data.yn2Bass)
                / data.a0Bass;
    data.xn2Bass = data.xn1Bass;
    data.xn1Bass = in;
    data.yn2Bass = data.yn1Bass;
    data.yn1Bass = out;

    // Treble filter
    in = out;
    out = (data.b0Treble * in + data.b1Treble * data.xn1Treble
           + data.b2Treble * data.xn2Treble - data.a1Treble * data.yn1Treble
           - data.a2Treble * data.yn2Treble)
          / data.a0Treble;
    data.xn2Treble = data.xn1Treble;
    data.xn1Treble = in;
    data.yn2Treble = data.yn1Treble;
    data.yn1Treble = out;

    return out;
}

void BassTrebleBase::Instance::InstanceInit(
    EffectSettings& settings, BassTrebleState& data, float sampleRate)
{
    auto& ms = GetSettings(settings);

    data.samplerate = sampleRate;
    data.slope = 0.4f;      // same slope for both filters
    data.hzBass = 250.0f;   // could be tunable in a more advanced version
    data.hzTreble = 4000.0f; // could be tunable in a more advanced version

    data.a0Bass = 1;
    data.a1Bass = 0;
    data.a2Bass = 0;
    data.b0Bass = 0;
    data.b1Bass = 0;
    data.b2Bass = 0;

    data.a0Treble = 1;
    data.a1Treble = 0;
    data.a2Treble = 0;
    data.b0Treble = 0;
    data.b1Treble = 0;
    data.b2Treble = 0;

    data.xn1Bass = 0;
    data.xn2Bass = 0;
    data.yn1Bass = 0;
    data.yn2Bass = 0;

    data.xn1Treble = 0;
    data.xn2Treble = 0;
    data.yn1Treble = 0;
    data.yn2Treble = 0;

    data.bass = -1;
    data.treble = -1;
    data.gain = DB_TO_LINEAR(ms.mGain);
}

bool BassTrebleBase::CheckWhetherSkipEffect(
    const EffectSettings& settings) const
{
    auto& ms = GetSettings(settings);

    return ms.mBass == 0.0 && ms.mTreble == 0.0 && ms.mGain == 0.0;
}
