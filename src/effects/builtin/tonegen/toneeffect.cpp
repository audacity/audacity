/*
* Audacity: A Digital Audio Editor
*/
#include "toneeffect.h"
#include "log.h"

using namespace au::effects;

const ComponentInterfaceSymbol ToneEffect::Symbol{ XO("Tone") };

ToneEffect::ToneEffect()
    : ToneGenBase(Type::Tone), GeneratorEffect(mT0, mT1)
{}

ToneEffect::ToneEffect(Type type)
    : ToneGenBase(type), GeneratorEffect(mT0, mT1)
{}

ComponentInterfaceSymbol ToneEffect::GetSymbol() const
{
    return Symbol;
}

void ToneEffect::doInit()
{
    // The tone generator is a chirp with constant amplitude and frequency.
    if (mType == Type::Tone) {
        mAmplitude1 = mAmplitude0;
        mFrequency1 = mFrequency0;
    }
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

ToneEffect::Interpolation ToneEffect::interpolation() const
{
    switch (mInterpolation) {
    case kLinear:
        return Interpolation::Linear;
    case kLogarithmic:
        return Interpolation::Logarithmic;
    default:
        DO_ASSERT(false);
    }
    return Interpolation::Linear;
}

void ToneEffect::setInterpolation(Interpolation interpolation)
{
    switch (interpolation) {
    case Interpolation::Linear:
        mInterpolation = kLinear;
        break;
    case Interpolation::Logarithmic:
        mInterpolation = kLogarithmic;
        break;
    default:
        DO_ASSERT(false);
    }
}

double ToneEffect::amplitudeStart() const
{
    return mAmplitude0;
}

void ToneEffect::setAmplitudeStart(double amplitude)
{
    mAmplitude0 = amplitude;
    if (mType == Type::Tone) {
        mAmplitude1 = amplitude;
    }
}

double ToneEffect::amplitudeEnd() const
{
    return mAmplitude1;
}

void ToneEffect::setAmplitudeEnd(double amplitude)
{
    DO_ASSERT(mType == Type::Chirp);
    mAmplitude1 = amplitude;
}

double ToneEffect::frequencyStart() const
{
    return mFrequency0;
}

void ToneEffect::setFrequencyStart(double frequency)
{
    mFrequency0 = frequency;
    if (mType == Type::Tone) {
        mFrequency1 = frequency;
    }
}

double ToneEffect::frequencyEnd() const
{
    return mFrequency1;
}

void ToneEffect::setFrequencyEnd(double frequency)
{
    DO_ASSERT(mType == Type::Chirp);
    mFrequency1 = frequency;
}

bool ToneEffect::isApplyAllowed() const
{
    // Doesn't matter which of start or end freq boundaries we use:
    static_assert(StartFreq.min == EndFreq.min);
    static_assert(StartFreq.max == EndFreq.max);
    static_assert(StartAmp.min == EndAmp.min);
    static_assert(StartAmp.max == EndAmp.max);
    constexpr auto frequencyMin = StartFreq.min;
    constexpr auto amplitudeMin = StartAmp.min;
    constexpr auto amplitudeMax = StartAmp.max;
    const auto frequencyMax = mProjectRate / 2;
    return frequencyMin <= frequencyStart() && frequencyStart() <= frequencyMax
           && frequencyMin <= frequencyEnd() && frequencyEnd() <= frequencyMax
           && amplitudeMin <= amplitudeStart() && amplitudeStart() <= amplitudeMax
           && amplitudeMin <= amplitudeEnd() && amplitudeEnd() <= amplitudeMax;
}
