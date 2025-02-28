/**********************************************************************

  Audacity: A Digital Audio Editor

  Split from ToneGen.h

  Steve Jolly

  This class implements a tone generator effect.

**********************************************************************/
#pragma once

#include "SettingsVisitor.h" // EffectParameter
#include "StatefulPerTrackEffect.h"
#include <float.h> // for DBL_MAX

class NumericTextCtrl;
class ShuttleGui;

class BUILTIN_EFFECTS_API ToneGenBase : public StatefulPerTrackEffect
{
public:

    enum class Type {
        Tone,
        Chirp
    };

    static inline ToneGenBase* FetchParameters(ToneGenBase& e, EffectSettings&)
    {
        return &e;
    }

    ToneGenBase(Type type);
    virtual ~ToneGenBase();

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    unsigned GetAudioOutCount() const override;
    bool ProcessInitialize(
        EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;
    size_t ProcessBlock(
        EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

protected:
    double mSampleRate {};
    const Type mType;

private:
    // mSample is an external placeholder to remember the last "buffer"
    // position so we use it to reinitialize from where we left
    sampleCount mSample;
    double mPositionInCycles;

protected:
    // If we made these static variables,
    // Tone and Chirp would share the same parameters.
    int mWaveform;
    int mInterpolation;
    double mFrequency0;
    double mFrequency1;
    double mAmplitude0;
    double mAmplitude1;

private:
    double mLogFrequency[2];

    void PostSet();

    const EffectParameterMethods& Parameters() const override;

protected:
    enum kWaveforms
    {
        kSine,
        kSquare,
        kSawtooth,
        kSquareNoAlias,
        kTriangle,
        nWaveforms
    };

    static const EnumValueSymbol kWaveStrings[nWaveforms];

    enum kInterpolations
    {
        kLinear,
        kLogarithmic,
        nInterpolations
    };

    static const EnumValueSymbol kInterStrings[nInterpolations];

    // Yes, mFrequency0 and mAmplitude0 are each associated with more than one
    static constexpr EffectParameter StartFreq {
        &ToneGenBase::mFrequency0, L"StartFreq", 440.0, 1.0, DBL_MAX, 1
    };
    static constexpr EffectParameter EndFreq {
        &ToneGenBase::mFrequency1, L"EndFreq", 1320.0, 1.0, DBL_MAX, 1
    };
    static constexpr EffectParameter StartAmp {
        &ToneGenBase::mAmplitude0, L"StartAmp", 0.8, 0.0, 1.0, 1
    };
    static constexpr EffectParameter EndAmp {
        &ToneGenBase::mAmplitude1, L"EndAmp", 0.1, 0.0, 1.0, 1
    };
    static constexpr EffectParameter Frequency {
        &ToneGenBase::mFrequency0, L"Frequency", 440.0, 1.0, DBL_MAX, 1
    };
    static constexpr EffectParameter Amplitude {
        &ToneGenBase::mAmplitude0, L"Amplitude", 0.8, 0.0, 1.0, 1
    };
    static constexpr EnumParameter Waveform { &ToneGenBase::mWaveform,
                                              L"Waveform",
                                              0,
                                              0,
                                              nWaveforms - 1,
                                              1,
                                              kWaveStrings,
                                              nWaveforms };
    static constexpr EnumParameter Interp { &ToneGenBase::mInterpolation,
                                            L"Interpolation",
                                            0,
                                            0,
                                            nInterpolations - 1,
                                            1,
                                            kInterStrings,
                                            nInterpolations };
};
