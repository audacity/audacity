/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/ToneGenBase.h"
#include "../common/generatoreffect.h"

struct EffectSettings;

namespace au::effects {
class ToneEffect : public ::ToneGenBase, public GeneratorEffect
{
public:
    ToneEffect();

    enum class Waveform
    {
        Sine,
        Square,
        Sawtooth,
        SquareNoAlias,
        Triangle,
    };

    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override;

    Waveform waveform() const;
    void setWaveform(Waveform waveform);
    double amplitude() const;
    void setAmplitude(double amplitude);
    double frequency() const;
    void setFrequency(double frequency);
    bool isApplyAllowed() const;

private:
    void doInit() override;
};
}
