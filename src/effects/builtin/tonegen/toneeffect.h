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

inline bool operator==(ToneEffect::Waveform lhs, int rhs)
{
    return lhs == static_cast<ToneEffect::Waveform>(rhs);
}

inline bool operator==(int lhs, ToneEffect::Waveform rhs)
{
    return static_cast<ToneEffect::Waveform>(lhs) == rhs;
}

inline bool operator==(ToneEffect::Interpolation lhs, int rhs)
{
    return lhs == static_cast<ToneEffect::Interpolation>(rhs);
}

inline bool operator==(int lhs, ToneEffect::Interpolation rhs)
{
    return static_cast<ToneEffect::Interpolation>(lhs) == rhs;
}

inline bool operator!=(ToneEffect::Waveform lhs, int rhs)
{
    return lhs != static_cast<ToneEffect::Waveform>(rhs);
}

inline bool operator!=(int lhs, ToneEffect::Waveform rhs)
{
    return static_cast<ToneEffect::Waveform>(lhs) != rhs;
}

inline bool operator!=(ToneEffect::Interpolation lhs, int rhs)
{
    return lhs != static_cast<ToneEffect::Interpolation>(rhs);
}

inline bool operator!=(int lhs, ToneEffect::Interpolation rhs)
{
    return static_cast<ToneEffect::Interpolation>(lhs) != rhs;
}
}
