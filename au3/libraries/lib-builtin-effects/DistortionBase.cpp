/**********************************************************************

  Audacity: A Digital Audio Editor

  DistortionBase.cpp

  Steve Daulton

  // TODO: Add a graph display of the waveshaper equation.
  // TODO: Allow the user to draw the graph.

******************************************************************//**

\class DistortionBase
\brief A WaveShaper distortion effect.

*//*******************************************************************/
#include "DistortionBase.h"
#include "ShuttleAutomation.h"
#include <utility>

#define STEPS 1024 // number of +ve or -ve steps in lookup tabe

const EnumValueSymbol DistortionBase::kTableTypeStrings[nTableTypes] = {
    { XO("Hard Clipping") },    { XO("Soft Clipping") },
    { XO("Soft Overdrive") },   { XO("Medium Overdrive") },
    { XO("Hard Overdrive") },   { XO("Cubic Curve (odd harmonics)") },
    { XO("Even Harmonics") },   { XO("Expand and Compress") },
    { XO("Leveller") },         { XO("Rectifier Distortion") },
    { XO("Hard Limiter 1413") }
};

const EffectParameterMethods& DistortionBase::Parameters() const
{
    static CapturedParameters<
        DistortionBase, TableTypeIndx, DCBlock, Threshold_dB, NoiseFloor, Param1,
        Param2, Repeats>
    parameters;
    return parameters;
}

// How many samples are processed before recomputing the lookup table again
#define skipsamples 1000

static const struct
{
    const TranslatableString name;
    EffectDistortionSettings params;
} FactoryPresets[]
    =// clang-format off
    {
    //                                           Table    DCBlock  threshold   floor       Param1   Param2   Repeats
    // Defaults:                                   0       false   -6.0       -70.0(off)     50.0     50.0     1
    //
    // xgettext:no-c-format
    { XO("Hard clip -12dB, 80% make-up gain"),     { 0,        0,      -12.0,      -70.0,      0.0,     80.0,    0 } },
    // xgettext:no-c-format
    { XO("Soft clip -12dB, 80% make-up gain"),     { 1,        0,      -12.0,      -70.0,      50.0,    80.0,    0 } },
    { XO("Fuzz Box"),                              { 1,        0,      -30.0,      -70.0,      80.0,    80.0,    0 } },
    { XO("Walkie-talkie"),                         { 1,        0,      -50.0,      -70.0,      60.0,    80.0,    0 } },
    { XO("Blues drive sustain"),                   { 2,        0,       -6.0,      -70.0,      30.0,    80.0,    0 } },
    { XO("Light Crunch Overdrive"),                { 3,        0,       -6.0,      -70.0,      20.0,    80.0,    0 } },
    { XO("Heavy Overdrive"),                       { 4,        0,       -6.0,      -70.0,      90.0,    80.0,    0 } },
    { XO("3rd Harmonic (Perfect Fifth)"),          { 5,        0,       -6.0,      -70.0,     100.0,    60.0,    0 } },
    { XO("Valve Overdrive"),                       { 6,        1,       -6.0,      -70.0,      30.0,    40.0,    0 } },
    { XO("2nd Harmonic (Octave)"),                 { 6,        1,       -6.0,      -70.0,      50.0,     0.0,    0 } },
    { XO("Gated Expansion Distortion"),            { 7,        0,       -6.0,      -70.0,      30.0,    80.0,    0 } },
    { XO("Leveller, Light, -70dB noise floor"),    { 8,        0,       -6.0,      -70.0,       0.0,    50.0,    1 } },
    { XO("Leveller, Moderate, -70dB noise floor"), { 8,        0,       -6.0,      -70.0,       0.0,    50.0,    2 } },
    { XO("Leveller, Heavy, -70dB noise floor"),    { 8,        0,       -6.0,      -70.0,       0.0,    50.0,    3 } },
    { XO("Leveller, Heavier, -70dB noise floor"),  { 8,        0,       -6.0,      -70.0,       0.0,    50.0,    4 } },
    { XO("Leveller, Heaviest, -70dB noise floor"), { 8,        0,       -6.0,      -70.0,       0.0,    50.0,    5 } },
    { XO("Half-wave Rectifier"),                   { 9,        0,       -6.0,      -70.0,      50.0,    50.0,    0 } },
    { XO("Full-wave Rectifier"),                   { 9,        0,       -6.0,      -70.0,     100.0,    50.0,    0 } },
    { XO("Full-wave Rectifier (DC blocked)"),      { 9,        1,       -6.0,      -70.0,     100.0,    50.0,    0 } },
    { XO("Percussion Limiter"),                    { 10,        0,      -12.0,      -70.0,     100.0,    30.0,    0 } },
    };
// clang-format on

const ComponentInterfaceSymbol DistortionBase::Symbol { XO("Distortion") };

std::shared_ptr<EffectInstance> DistortionBase::MakeInstance() const
{
    return std::make_shared<Instance>(*this);
}

DistortionBase::DistortionBase()
{
    wxASSERT(nTableTypes == WXSIZEOF(kTableTypeStrings));

    SetLinearEffectFlag(false);
}

DistortionBase::~DistortionBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol DistortionBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString DistortionBase::GetDescription() const
{
    return XO("Waveshaping distortion effect");
}

ManualPageID DistortionBase::ManualPage() const
{
    return L"Distortion";
}

// EffectDefinitionInterface implementation

EffectType DistortionBase::GetType() const
{
    return EffectTypeProcess;
}

auto DistortionBase::RealtimeSupport() const -> RealtimeSince
{
    return RealtimeSince::After_3_1;
}

unsigned DistortionBase::Instance::GetAudioInCount() const
{
    return 1;
}

unsigned DistortionBase::Instance::GetAudioOutCount() const
{
    return 1;
}

bool DistortionBase::Instance::ProcessInitialize(
    EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
    InstanceInit(mMaster, settings, sampleRate);
    return true;
}

size_t DistortionBase::Instance::ProcessBlock(
    EffectSettings& settings, const float* const* inBlock,
    float* const* outBlock, size_t blockLen)
{
    return InstanceProcess(settings, mMaster, inBlock, outBlock, blockLen);
}

bool DistortionBase::Instance::RealtimeInitialize(EffectSettings&, double)
{
    SetBlockSize(512);
    mSlaves.clear();
    return true;
}

bool DistortionBase::Instance::RealtimeAddProcessor(
    EffectSettings& settings, EffectOutputs*, unsigned, float sampleRate)
{
    EffectDistortionState slave;

    InstanceInit(slave, settings, sampleRate);

    mSlaves.push_back(slave);

    return true;
}

bool DistortionBase::Instance::RealtimeFinalize(EffectSettings&) noexcept
{
    mSlaves.clear();

    return true;
}

size_t DistortionBase::Instance::RealtimeProcess(
    size_t group, EffectSettings& settings, const float* const* inbuf,
    float* const* outbuf, size_t numSamples)
{
    if (group >= mSlaves.size()) {
        return 0;
    }
    return InstanceProcess(settings, mSlaves[group], inbuf, outbuf, numSamples);
}

RegistryPaths DistortionBase::GetFactoryPresets() const
{
    RegistryPaths names;

    for (size_t i = 0; i < WXSIZEOF(FactoryPresets); i++) {
        names.push_back(FactoryPresets[i].name.Translation());
    }

    return names;
}

OptionalMessage
DistortionBase::LoadFactoryPreset(int id, EffectSettings& settings) const
{
    // To do: externalize state so const_cast isn't needed
    return const_cast<DistortionBase*>(this)->DoLoadFactoryPreset(id, settings);
}

OptionalMessage
DistortionBase::DoLoadFactoryPreset(int id, EffectSettings& settings)
{
    if (id < 0 || id >= (int)WXSIZEOF(FactoryPresets)) {
        return {};
    }

    GetSettings(settings) = FactoryPresets[id].params;

    return { nullptr };
}

void DistortionBase::Instance::InstanceInit(
    EffectDistortionState& data, EffectSettings& settings, float sampleRate)
{
    auto& ms = GetSettings(settings);

    data.samplerate = sampleRate;
    data.skipcount = 0;
    data.tablechoiceindx = ms.mTableChoiceIndx;
    data.dcblock = ms.mDCBlock;
    data.threshold = ms.mThreshold_dB;
    data.noisefloor = ms.mNoiseFloor;
    data.param1 = ms.mParam1;
    data.param2 = ms.mParam2;
    data.repeats = ms.mRepeats;

    // DC block filter variables
    data.queuetotal = 0.0;

    // std::queue<float>().swap(data.queuesamples);
    while (!data.queuesamples.empty()) {
        data.queuesamples.pop();
    }

    MakeTable(data, ms);

    return;
}

size_t DistortionBase::Instance::InstanceProcess(
    EffectSettings& settings, EffectDistortionState& data,
    const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
    auto& ms = GetSettings(settings);

    const float* ibuf = inBlock[0];
    float* obuf = outBlock[0];

    bool update
        =(ms.mTableChoiceIndx == data.tablechoiceindx
          && ms.mNoiseFloor == data.noisefloor
          && ms.mThreshold_dB == data.threshold && ms.mParam1 == data.param1
          && ms.mParam2 == data.param2 && ms.mRepeats == data.repeats)
          ? false
          : true;

    double p1 = ms.mParam1 / 100.0;
    double p2 = ms.mParam2 / 100.0;

    data.tablechoiceindx = ms.mTableChoiceIndx;
    data.threshold = ms.mThreshold_dB;
    data.noisefloor = ms.mNoiseFloor;
    data.param1 = ms.mParam1;
    data.repeats = ms.mRepeats;

    for (decltype(blockLen) i = 0; i < blockLen; i++) {
        if (update && ((data.skipcount++) % skipsamples == 0)) {
            MakeTable(data, ms);
        }

        switch (ms.mTableChoiceIndx) {
        case kHardClip:
            // Param2 = make-up gain.
            obuf[i]
                =WaveShaper(ibuf[i], ms) * ((1 - p2) + (data.mMakeupGain * p2));
            break;
        case kSoftClip:
            // Param2 = make-up gain.
            obuf[i]
                =WaveShaper(ibuf[i], ms) * ((1 - p2) + (data.mMakeupGain * p2));
            break;
        case kHalfSinCurve:
            obuf[i] = WaveShaper(ibuf[i], ms) * p2;
            break;
        case kExpCurve:
            obuf[i] = WaveShaper(ibuf[i], ms) * p2;
            break;
        case kLogCurve:
            obuf[i] = WaveShaper(ibuf[i], ms) * p2;
            break;
        case kCubic:
            obuf[i] = WaveShaper(ibuf[i], ms) * p2;
            break;
        case kEvenHarmonics:
            obuf[i] = WaveShaper(ibuf[i], ms);
            break;
        case kSinCurve:
            obuf[i] = WaveShaper(ibuf[i], ms) * p2;
            break;
        case kLeveller:
            obuf[i] = WaveShaper(ibuf[i], ms);
            break;
        case kRectifier:
            obuf[i] = WaveShaper(ibuf[i], ms);
            break;
        case kHardLimiter:
            // Mix equivalent to LADSPA effect's "Wet / Residual" mix
            obuf[i] = (WaveShaper(ibuf[i], ms) * (p1 - p2)) + (ibuf[i] * p2);
            break;
        default:
            obuf[i] = WaveShaper(ibuf[i], ms);
        }
        if (ms.mDCBlock) {
            obuf[i] = DCFilter(data, obuf[i]);
        }
    }

    return blockLen;
}

void DistortionBase::Instance::MakeTable(
    EffectDistortionState& state, const EffectDistortionSettings& ms)
{
    switch (ms.mTableChoiceIndx) {
    case kHardClip:
        HardClip(state, ms);
        break;
    case kSoftClip:
        SoftClip(state, ms);
        break;
    case kHalfSinCurve:
        HalfSinTable(ms);
        break;
    case kExpCurve:
        ExponentialTable(ms);
        break;
    case kLogCurve:
        LogarithmicTable(ms);
        break;
    case kCubic:
        CubicTable(ms);
        break;
    case kEvenHarmonics:
        EvenHarmonicTable(ms);
        break;
    case kSinCurve:
        SineTable(ms);
        break;
    case kLeveller:
        Leveller(ms);
        break;
    case kRectifier:
        Rectifier(ms);
        break;
    case kHardLimiter:
        HardLimiter(state, ms);
        break;
    }
}

//
// Preset tables for gain lookup
//

void DistortionBase::Instance::HardClip(
    EffectDistortionState& state, const EffectDistortionSettings& ms)
{
    const double threshold = DB_TO_LINEAR(ms.mThreshold_dB);

    double lowThresh = 1 - threshold;
    double highThresh = 1 + threshold;

    for (int n = 0; n < TABLESIZE; n++) {
        if (n < (STEPS * lowThresh)) {
            mTable[n] = -threshold;
        } else if (n > (STEPS * highThresh)) {
            mTable[n] = threshold;
        } else {
            mTable[n] = n / (double)STEPS - 1;
        }

        state.mMakeupGain = 1.0 / threshold;
    }
}

void DistortionBase::Instance::SoftClip(
    EffectDistortionState& state, const EffectDistortionSettings& ms)
{
    const double thresholdLinear = DB_TO_LINEAR(ms.mThreshold_dB);

    double threshold = 1 + thresholdLinear;
    double amount = std::pow(2.0, 7.0 * ms.mParam1 / 100.0); // range 1 to 128
    double peak = LogCurve(thresholdLinear, 1.0, amount);
    state.mMakeupGain = 1.0 / peak;
    mTable[STEPS] = 0.0; // origin

    // positive half of table
    for (int n = STEPS; n < TABLESIZE; n++) {
        if (n < (STEPS * threshold)) { // origin to threshold
            mTable[n] = n / (float)STEPS - 1;
        } else {
            mTable[n] = LogCurve(thresholdLinear, n / (double)STEPS - 1, amount);
        }
    }
    CopyHalfTable();
}

float DistortionBase::Instance::LogCurve(
    double threshold, float value, double ratio)
{
    return threshold + ((std::exp(ratio * (threshold - value)) - 1) / -ratio);
}

void DistortionBase::Instance::ExponentialTable(
    const EffectDistortionSettings& ms)
{
    double amount
        =std::min(0.999, DB_TO_LINEAR(-1 * ms.mParam1)); // avoid divide by zero

    for (int n = STEPS; n < TABLESIZE; n++) {
        double linVal = n / (float)STEPS;
        double scale = -1.0 / (1.0 - amount); // unity gain at 0dB
        double curve = std::exp((linVal - 1) * std::log(amount));
        mTable[n] = scale * (curve - 1);
    }
    CopyHalfTable();
}

void DistortionBase::Instance::LogarithmicTable(
    const EffectDistortionSettings& ms)
{
    double amount = ms.mParam1;
    double stepsize = 1.0 / STEPS;
    double linVal = 0;

    if (amount == 0) {
        for (int n = STEPS; n < TABLESIZE; n++) {
            mTable[n] = linVal;
            linVal += stepsize;
        }
    } else {
        for (int n = STEPS; n < TABLESIZE; n++) {
            mTable[n] = std::log(1 + (amount * linVal)) / std::log(1 + amount);
            linVal += stepsize;
        }
    }
    CopyHalfTable();
}

void DistortionBase::Instance::HalfSinTable(const EffectDistortionSettings& ms)
{
    int iter = std::floor(ms.mParam1 / 20.0);
    double fractionalpart = (ms.mParam1 / 20.0) - iter;
    double stepsize = 1.0 / STEPS;
    double linVal = 0;

    for (int n = STEPS; n < TABLESIZE; n++) {
        mTable[n] = linVal;
        for (int i = 0; i < iter; i++) {
            mTable[n] = std::sin(mTable[n] * M_PI_2);
        }
        mTable[n]
            +=((std::sin(mTable[n] * M_PI_2) - mTable[n]) * fractionalpart);
        linVal += stepsize;
    }
    CopyHalfTable();
}

void DistortionBase::Instance::CubicTable(const EffectDistortionSettings& ms)
{
    double amount = ms.mParam1 * std::sqrt(3.0) / 100.0;
    double gain = 1.0;
    if (amount != 0.0) {
        gain = 1.0 / Cubic(ms, std::min(amount, 1.0));
    }

    double stepsize = amount / STEPS;
    double x = -amount;

    if (amount == 0) {
        for (int i = 0; i < TABLESIZE; i++) {
            mTable[i] = (i / (double)STEPS) - 1.0;
        }
    } else {
        for (int i = 0; i < TABLESIZE; i++) {
            mTable[i] = gain * Cubic(ms, x);
            for (int j = 0; j < ms.mRepeats; j++) {
                mTable[i] = gain * Cubic(ms, mTable[i] * amount);
            }
            x += stepsize;
        }
    }
}

double
DistortionBase::Instance::Cubic(const EffectDistortionSettings& ms, double x)
{
    if (ms.mParam1 == 0.0) {
        return x;
    }

    return x - (std::pow(x, 3.0) / 3.0);
}

void DistortionBase::Instance::EvenHarmonicTable(
    const EffectDistortionSettings& ms)
{
    double amount = ms.mParam1 / -100.0;
    // double C = std::sin(std::max(0.001, mParams.mParam2) / 100.0) * 10.0;
    double C = std::max(0.001, ms.mParam2) / 10.0;

    double step = 1.0 / STEPS;
    double xval = -1.0;

    for (int i = 0; i < TABLESIZE; i++) {
        mTable[i] = ((1 + amount) * xval)
                    - (xval * (amount / std::tanh(C)) * std::tanh(C * xval));
        xval += step;
    }
}

void DistortionBase::Instance::SineTable(const EffectDistortionSettings& ms)
{
    int iter = std::floor(ms.mParam1 / 20.0);
    double fractionalpart = (ms.mParam1 / 20.0) - iter;
    double stepsize = 1.0 / STEPS;
    double linVal = 0.0;

    for (int n = STEPS; n < TABLESIZE; n++) {
        mTable[n] = linVal;
        for (int i = 0; i < iter; i++) {
            mTable[n] = (1.0 + std::sin((mTable[n] * M_PI) - M_PI_2)) / 2.0;
        }
        mTable[n]
            +=(((1.0 + std::sin((mTable[n] * M_PI) - M_PI_2)) / 2.0) - mTable[n])
               * fractionalpart;
        linVal += stepsize;
    }
    CopyHalfTable();
}

void DistortionBase::Instance::Leveller(const EffectDistortionSettings& ms)
{
    double noiseFloor = DB_TO_LINEAR(ms.mNoiseFloor);
    int numPasses = ms.mRepeats;
    double fractionalPass = ms.mParam1 / 100.0;

    const int numPoints = 6;
    const double gainFactors[numPoints] = { 0.80, 1.00, 1.20, 1.20, 1.00, 0.80 };
    double gainLimits[numPoints] = { 0.0001, 0.0, 0.1, 0.3, 0.5, 1.0 };
    double addOnValues[numPoints];

    gainLimits[1] = noiseFloor;
    /* In the original Leveller effect, behaviour was undefined for threshold >
     * 20 dB. If we want to support > 20 dB we need to scale the points to keep
     * them non-decreasing.
     *
     * if (noiseFloor > gainLimits[2]) {
     *    for (int i = 3; i < numPoints; i++) {
     *    gainLimits[i] = noiseFloor + ((1 - noiseFloor)*((gainLimits[i] - 0.1) /
     * 0.9));
     * }
     * gainLimits[2] = noiseFloor;
     * }
     */

    // Calculate add-on values
    addOnValues[0] = 0.0;
    for (int i = 0; i < numPoints - 1; i++) {
        addOnValues[i + 1]
            =addOnValues[i]
              + (gainLimits[i] * (gainFactors[i] - gainFactors[1 + i]));
    }

    // Positive half of table.
    // The original effect increased the 'strength' of the effect by
    // repeated passes over the audio data.
    // Here we model that more efficiently by repeated passes over a linear
    // table.
    for (int n = STEPS; n < TABLESIZE; n++) {
        mTable[n] = ((double)(n - STEPS) / (double)STEPS);
        for (int j = 0; j < numPasses; j++) {
            // Find the highest index for gain adjustment
            int index = numPoints - 1;
            for (int i = index; i >= 0 && mTable[n] < gainLimits[i]; i--) {
                index = i;
            }
            // the whole number of 'repeats'
            mTable[n] = (mTable[n] * gainFactors[index]) + addOnValues[index];
        }
        // Extrapolate for fine adjustment.
        // tiny fractions are not worth the processing time
        if (fractionalPass > 0.001) {
            int index = numPoints - 1;
            for (int i = index; i >= 0 && mTable[n] < gainLimits[i]; i--) {
                index = i;
            }
            mTable[n] += fractionalPass * ((mTable[n] * (gainFactors[index] - 1))
                                           + addOnValues[index]);
        }
    }
    CopyHalfTable();
}

void DistortionBase::Instance::Rectifier(const EffectDistortionSettings& ms)
{
    double amount = (ms.mParam1 / 50.0) - 1;
    double stepsize = 1.0 / STEPS;
    int index = STEPS;

    // positive half of waveform is passed unaltered.
    for (int n = 0; n <= STEPS; n++) {
        mTable[index] = n * stepsize;
        index += 1;
    }

    // negative half of table
    index = STEPS - 1;
    for (int n = 1; n <= STEPS; n++) {
        mTable[index] = n * stepsize * amount;
        index--;
    }
}

void DistortionBase::Instance::HardLimiter(
    EffectDistortionState& state, const EffectDistortionSettings& settings)
{
    // The LADSPA "hardLimiter 1413" is basically hard clipping,
    // but with a 'kind of' wet/dry mix:
    // out = ((wet-residual)*clipped) + (residual*in)
    HardClip(state, settings);
}

// Helper functions for lookup tables

void DistortionBase::Instance::CopyHalfTable()
{
    // Copy negative half of table from positive half
    int count = TABLESIZE - 1;
    for (int n = 0; n < STEPS; n++) {
        mTable[n] = -mTable[count];
        count--;
    }
}

float DistortionBase::Instance::WaveShaper(
    float sample, EffectDistortionSettings& ms)
{
    float out;
    int index;
    double xOffset;
    double amount = 1;

    switch (ms.mTableChoiceIndx) {
    // Do any pre-processing here
    case kHardClip:
        // Pre-gain
        amount = ms.mParam1 / 100.0;
        sample *= 1 + amount;
        break;
    default:
        break;
    }

    index = std::floor(sample * STEPS) + STEPS;
    index = std::max<int>(std::min<int>(index, 2 * STEPS - 1), 0);
    xOffset = ((1 + sample) * STEPS) - index;
    xOffset = std::min<double>(std::max<double>(xOffset, 0.0), 1.0); // Clip at 0dB

    // linear interpolation: y = y0 + (y1-y0)*(x-x0)
    out = mTable[index] + (mTable[index + 1] - mTable[index]) * xOffset;

    return out;
}

float DistortionBase::Instance::DCFilter(
    EffectDistortionState& data, float sample)
{
    // Rolling average gives less offset at the start than an IIR filter.
    const unsigned int queueLength = std::floor(data.samplerate / 20.0);

    data.queuetotal += sample;
    data.queuesamples.push(sample);

    if (data.queuesamples.size() > queueLength) {
        data.queuetotal -= data.queuesamples.front();
        data.queuesamples.pop();
    }

    return sample - (data.queuetotal / data.queuesamples.size());
}
