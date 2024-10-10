/*
* Audacity: A Digital Audio Editor
*/
#include "toneeffect.h"
#include "log.h"
#include "libraries/lib-components/EffectInterface.h"

using namespace au::effects;

const ComponentInterfaceSymbol ToneEffect::Symbol{ XO("Tone") };

ToneEffect::ToneEffect()
    : ToneGenBase(false)
{}

ComponentInterfaceSymbol ToneEffect::GetSymbol() const
{
    return Symbol;
}

void ToneEffect::init(const EffectSettings& settings)
{
    // The tone generator is a chirp with constant
    mAmplitude1 = mAmplitude0;
    mFrequency1 = mFrequency0;
    mT1 = mT0 + settings.extra.GetDuration();
}

double ToneEffect::sampleRate() const
{
    return mProjectRate;
}

ToneEffect::Waveform ToneEffect::waveform() const
{
    switch (mWaveform) {
    case kSine:
        return Waveform::sine;
    case kSquare:
        return Waveform::square;
    case kSawtooth:
        return Waveform::sawtooth;
    case kSquareNoAlias:
        return Waveform::squareNoAlias;
    case kTriangle:
        return Waveform::triangle;
    }
    DO_ASSERT(false);
    return Waveform::sine;
}

void ToneEffect::setWaveform(Waveform waveform)
{
    switch (waveform) {
    case Waveform::sine:
        mWaveform = kSine;
        break;
    case Waveform::square:
        mWaveform = kSquare;
        break;
    case Waveform::sawtooth:
        mWaveform = kSawtooth;
        break;
    case Waveform::squareNoAlias:
        mWaveform = kSquareNoAlias;
        break;
    case Waveform::triangle:
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

double ToneEffect::duration() const
{
    return mT1 - mT0;
}

void ToneEffect::setDuration(double duration)
{
    mT1 = mT0 + duration;
}
