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

    enum class Interpolation
    {
        Linear,
        Logarithmic,
    };

    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override;

    Waveform waveform() const;
    void setWaveform(Waveform waveform);
    Interpolation interpolation() const;
    void setInterpolation(Interpolation interpolation);
    double amplitudeStart() const;
    void setAmplitudeStart(double amplitude);
    double amplitudeEnd() const;
    void setAmplitudeEnd(double amplitude);
    double frequencyStart() const;
    void setFrequencyStart(double frequency);
    double frequencyEnd() const;
    void setFrequencyEnd(double frequency);
    bool isApplyAllowed() const;

protected:
    ToneEffect(Type type);

private:
    void doInit() override;
};
}
