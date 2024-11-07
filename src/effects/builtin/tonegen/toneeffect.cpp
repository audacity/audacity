/*
* Audacity: A Digital Audio Editor
*/
#include "toneeffect.h"
#include "log.h"

using namespace au::effects;

const ComponentInterfaceSymbol ToneEffect::Symbol{ XO("Tone") };

ToneEffect::ToneEffect()
    : ToneGenBase(false), GeneratorEffect(mProjectRate, mT0, mT1)
{}

ComponentInterfaceSymbol ToneEffect::GetSymbol() const
{
    return Symbol;
}

void ToneEffect::doInit()
{
    // The tone generator is a chirp with constant amplitude and frequency.
    mAmplitude1 = mAmplitude0;
    mFrequency1 = mFrequency0;
}

ToneEffect::Waveform ToneEffect::waveform() const
{
    switch (mWaveform) {
    case kSine:
        return Waveform::Sine;
    case kSquare:
        return Waveform::Square;
    case kSawtooth:
        return Waveform::Sawtooth;
    case kSquareNoAlias:
        return Waveform::SquareNoAlias;
    case kTriangle:
        return Waveform::Triangle;
    }
    DO_ASSERT(false);
    return Waveform::Sine;
}

void ToneEffect::setWaveform(Waveform waveform)
{
    switch (waveform) {
    case Waveform::Sine:
        mWaveform = kSine;
        break;
    case Waveform::Square:
        mWaveform = kSquare;
        break;
    case Waveform::Sawtooth:
        mWaveform = kSawtooth;
        break;
    case Waveform::SquareNoAlias:
        mWaveform = kSquareNoAlias;
        break;
    case Waveform::Triangle:
        mWaveform = kTriangle;
        break;
    default:
        DO_ASSERT(false);
    }
}

double ToneEffect::amplitude() const
{
    return mAmplitude0;
}

void ToneEffect::setAmplitude(double amplitude)
{
    mAmplitude0 = mAmplitude1 = amplitude;
}

double ToneEffect::frequency() const
{
    return mFrequency0;
}

void ToneEffect::setFrequency(double frequency)
{
    mFrequency0 = mFrequency1 = frequency;
}

bool ToneEffect::isApplyAllowed() const
{
    // Doesn't matter which of start or end freq boundaries we use:
    static_assert(StartFreq.min == EndFreq.min);
    static_assert(StartFreq.max == EndFreq.max);
    static_assert(StartAmp.min == EndAmp.min);
    static_assert(StartAmp.max == EndAmp.max);
    constexpr auto frequencyMin = StartFreq.min;
    constexpr auto frequencyMax = StartFreq.max;
    constexpr auto amplitudeMin = StartAmp.min;
    constexpr auto amplitudeMax = StartAmp.max;
    return frequencyMin <= frequency() && frequency() <= frequencyMax
           && amplitudeMin <= amplitude() && amplitude() <= amplitudeMax;
}
