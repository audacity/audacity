/**********************************************************************

Audacity: A Digital Audio Editor

ScienFilterBase.h

Norm C
Mitch Golden
Vaughan Johnson (Preview)

***********************************************************************/
#pragma once

#include "Biquad.h"
#include "ShuttleAutomation.h"
#include "StatefulPerTrackEffect.h"
#include <cfloat> // for FLT_MAX

class BUILTIN_EFFECTS_API ScienFilterBase : public StatefulPerTrackEffect
{
public:
    static inline ScienFilterBase*
    FetchParameters(ScienFilterBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    ScienFilterBase();
    virtual ~ScienFilterBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    unsigned GetAudioInCount() const override;
    unsigned GetAudioOutCount() const override;
    bool ProcessInitialize(
        EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;
    size_t ProcessBlock(
        EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

    // Effect implementation

    bool Init() override;

protected:
    // ScienFilterBase implementation

    void CalcFilter();
    float FilterMagnAtFreq(float Freq);

    float mCutoff;
    float mRipple;
    float mStopbandRipple;
    int mFilterType;   // Butterworth etc.
    int mFilterSubtype; // lowpass, highpass
    int mOrder;
    int mOrderIndex;
    ArrayOf<Biquad> mpBiquad;

    double mdBMax;
    double mdBMin;
    bool mEditingBatchParams;

    double mLoFreq;
    double mNyquist;

    const EffectParameterMethods& Parameters() const override;

    enum kSubTypes
    {
        kLowPass = Biquad::kLowPass,
        kHighPass = Biquad::kHighPass,
        nSubTypes = Biquad::nSubTypes
    };
    static const EnumValueSymbol kSubTypeStrings[nSubTypes];

    enum kTypes
    {
        kButterworth,
        kChebyshevTypeI,
        kChebyshevTypeII,
        nTypes
    };
    static const EnumValueSymbol kTypeStrings[nTypes];

    static_assert(nSubTypes == WXSIZEOF(kSubTypeStrings), "size mismatch");

    static constexpr EnumParameter Type { &ScienFilterBase::mFilterType,
                                          L"FilterType",
                                          kButterworth,
                                          0,
                                          nTypes - 1,
                                          1,
                                          kTypeStrings,
                                          nTypes };
    static constexpr EnumParameter Subtype { &ScienFilterBase::mFilterSubtype,
                                             L"FilterSubtype",
                                             kLowPass,
                                             0,
                                             nSubTypes - 1,
                                             1,
                                             kSubTypeStrings,
                                             nSubTypes };
    static constexpr EffectParameter Order {
        &ScienFilterBase::mOrder, L"Order", 1, 1, 10, 1
    };
    static constexpr EffectParameter Cutoff {
        &ScienFilterBase::mCutoff, L"Cutoff", 1000.0f, 1.0, FLT_MAX, 1
    };
    static constexpr EffectParameter Passband {
        &ScienFilterBase::mRipple, L"PassbandRipple", 1.0f, 0.0, 100.0, 1
    };
    static constexpr EffectParameter Stopband {
        &ScienFilterBase::mStopbandRipple, L"StopbandRipple", 30.0f, 0.0, 100.0, 1
    };
};
