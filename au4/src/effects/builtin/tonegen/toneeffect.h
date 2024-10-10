/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/ToneGenBase.h"

struct EffectSettings;

namespace au::effects {
class ToneEffect : public ::ToneGenBase
{
public:
    ToneEffect();

    enum class Waveform
    {
        sine,
        square,
        sawtooth,
        squareNoAlias,
        triangle,
    };

    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override;

    void init(const EffectSettings& settings);

    double sampleRate() const;

    Waveform waveform() const;
    void setWaveform(Waveform waveform);
    double amplitude() const;
    void setAmplitude(double amplitude);
    double frequency() const;
    void setFrequency(double frequency);
    double duration() const;
    void setDuration(double duration);
};
}
