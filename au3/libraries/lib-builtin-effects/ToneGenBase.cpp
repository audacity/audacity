/**********************************************************************

  Audacity: A Digital Audio Editor

  Split from ToneGen.cpp

  Steve Jolly
  James Crook (Adapted for 'Chirps')

  This class implements a tone generator effect.

*******************************************************************//**

\class ToneGenBase
\brief An Effect that can generate a sine, square or sawtooth wave.
An extended mode of ToneGenBase supports 'chirps' where the
frequency changes smoothly during the tone.

*//*******************************************************************/
#include "ToneGenBase.h"
#include "Project.h"
#include "ProjectRate.h"
#include "ShuttleAutomation.h"
#include <math.h>

const EnumValueSymbol ToneGenBase::kInterStrings[nInterpolations] = {
    // These are acceptable dual purpose internal/visible names
    { XO("Linear") },
    { XO("Logarithmic") }
};

const EnumValueSymbol ToneGenBase::kWaveStrings[nWaveforms] = {
    { XO("Sine") },
    { XO("Square") },
    { XO("Sawtooth") },
    { XO("Square, no alias") },
    { XC("Triangle", "waveform") }
};

const EffectParameterMethods& ToneGenBase::Parameters() const
{
    static const auto postSet = [](ToneGenBase&, EffectSettings&, ToneGenBase& e,
                                   bool updating) {
        if (updating) {
            e.PostSet();
        }
        return true;
    };
    static CapturedParameters<
        ToneGenBase, StartFreq, EndFreq, StartAmp, EndAmp, Waveform, Interp>
    chirpParameters { postSet };
    static CapturedParameters<
        ToneGenBase, Frequency, Amplitude, Waveform, Interp>
    toneParameters { postSet };
    if (mType == Type::Chirp) {
        return chirpParameters;
    } else {
        return toneParameters;
    }
}

ToneGenBase::ToneGenBase(Type type)
    : mType{type}
{
    Parameters().Reset(*this);

    wxASSERT(nWaveforms == WXSIZEOF(kWaveStrings));
    wxASSERT(nInterpolations == WXSIZEOF(kInterStrings));

    // Chirp varies over time so must use selected duration.
    // TODO: When previewing, calculate only the first 'preview length'.
    if (type == Type::Chirp) {
        SetLinearEffectFlag(false);
    } else {
        SetLinearEffectFlag(true);
    }
}

ToneGenBase::~ToneGenBase()
{
}

// EffectDefinitionInterface implementation

EffectType ToneGenBase::GetType() const
{
    return EffectTypeGenerate;
}

unsigned ToneGenBase::GetAudioOutCount() const
{
    return 1;
}

bool ToneGenBase::ProcessInitialize(
    EffectSettings&, double sampleRate, ChannelNames chanMap)
{
    mSampleRate = sampleRate;
    mPositionInCycles = 0.0;
    mSample = 0;
    return true;
}

size_t ToneGenBase::ProcessBlock(
    EffectSettings&, const float* const*, float* const* outBlock,
    size_t blockLen)
{
    float* buffer = outBlock[0];
    double throwaway = 0; // passed to modf but never used
    double f = 0.0;
    double a, b;
    int k;

    double frequencyQuantum;
    double BlendedFrequency;
    double BlendedAmplitude;
    double BlendedLogFrequency = 0.0;

    // calculate delta, and reposition from where we left
    auto doubleSampleCount = mSampleCnt.as_double();
    auto doubleSample = mSample.as_double();
    double amplitudeQuantum = (mAmplitude1 - mAmplitude0) / doubleSampleCount;
    BlendedAmplitude = mAmplitude0 + amplitudeQuantum * doubleSample;

    // precalculations:
    double pre2PI = 2.0 * M_PI;
    double pre4divPI = 4.0 / M_PI;

    // initial setup should calculate deltas
    if (mInterpolation == kLogarithmic) {
        // this for log interpolation
        mLogFrequency[0] = log10(mFrequency0);
        mLogFrequency[1] = log10(mFrequency1);
        // calculate delta, and reposition from where we left
        frequencyQuantum
            =(mLogFrequency[1] - mLogFrequency[0]) / doubleSampleCount;
        BlendedLogFrequency = mLogFrequency[0] + frequencyQuantum * doubleSample;
        BlendedFrequency = pow(10.0, BlendedLogFrequency);
    } else {
        // this for regular case, linear interpolation
        frequencyQuantum = (mFrequency1 - mFrequency0) / doubleSampleCount;
        BlendedFrequency = mFrequency0 + frequencyQuantum * doubleSample;
    }

    // synth loop
    for (decltype(blockLen) i = 0; i < blockLen; i++) {
        switch (mWaveform) {
        case kSine:
            f = sin(pre2PI * mPositionInCycles / mSampleRate);
            break;
        case kSquare:
            f = (modf(mPositionInCycles / mSampleRate, &throwaway) < 0.5) ? 1.0
                : -1.0;
            break;
        case kSawtooth:
            f = (2.0 * modf(mPositionInCycles / mSampleRate + 0.5, &throwaway))
                - 1.0;
            break;
        case kTriangle:
            f = modf(mPositionInCycles / mSampleRate, &throwaway);
            if (f < 0.25) {
                f *= 4.0;
            } else if (f > 0.75) {
                f = (f - 1.0) * 4.0;
            } else { /* f >= 0.25 || f <= 0.75 */
                f = (0.5 - f) * 4.0;
            }
            break;
        case kSquareNoAlias: // Good down to 110Hz @ 44100Hz sampling.
            // do fundamental (k=1) outside loop
            b = (1.0 + cos((pre2PI * BlendedFrequency) / mSampleRate))
                / pre4divPI; // scaling
            f = pre4divPI * sin(pre2PI * mPositionInCycles / mSampleRate);
            for (k = 3; (k < 200) && (k * BlendedFrequency < mSampleRate / 2.0);
                 k += 2) {
                // Hann Window in freq domain
                a = 1.0 + cos((pre2PI * k * BlendedFrequency) / mSampleRate);
                // calc harmonic, apply window, scale to amplitude of fundamental
                f
                    +=a * sin(pre2PI * mPositionInCycles / mSampleRate * k) / (b * k);
            }
        }
        // insert value in buffer
        buffer[i] = (float)(BlendedAmplitude * f);
        // update freq,amplitude
        mPositionInCycles += BlendedFrequency;
        BlendedAmplitude += amplitudeQuantum;
        if (mInterpolation == kLogarithmic) {
            BlendedLogFrequency += frequencyQuantum;
            BlendedFrequency = pow(10.0, BlendedLogFrequency);
        } else {
            BlendedFrequency += frequencyQuantum;
        }
    }

    // update external placeholder
    mSample += blockLen;

    return blockLen;
}

void ToneGenBase::PostSet()
{
    if (mType == Type::Tone) {
        mFrequency1 = mFrequency0;
        mAmplitude1 = mAmplitude0;
    }
    //   double freqMax =
    //      (FindProject()
    //         ? ProjectRate::Get( *FindProject() ).GetRate()
    //         : 44100.0)
    //      / 2.0;
    //   mFrequency1 = std::clamp<double>(mFrequency1, EndFreq.min, freqMax);
}
