/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyCompressorBase.h

  Dominic Mazzoni

**********************************************************************/
#pragma once

#include "MemoryX.h"
#include "ShuttleAutomation.h"
#include "TwoPassSimpleMono.h"

using Floats = ArrayOf<float>;
using Doubles = ArrayOf<double>;

class BUILTIN_EFFECTS_API LegacyCompressorBase : public EffectTwoPassSimpleMono
{
public:
    static inline LegacyCompressorBase*
    FetchParameters(LegacyCompressorBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    LegacyCompressorBase();
    virtual ~LegacyCompressorBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

protected:
    // EffectTwoPassSimpleMono implementation

    bool InitPass1() override;
    bool InitPass2() override;
    bool NewTrackPass1() override;
    bool ProcessPass2(float* buffer, size_t len) override;
    bool TwoBufferProcessPass1(
        float* buffer1, size_t len1, float* buffer2, size_t len2) override;

private:
    // LegacyCompressorBase implementation

    void FreshenCircle();
    float AvgCircle(float x);
    void Follow(
        float* buffer, float* env, size_t len, float* previous, size_t previous_len);
    float DoCompression(float x, double env);

protected:
    double mRMSSum;
    size_t mCircleSize;
    size_t mCirclePos;
    Doubles mCircle;

    double mAttackTime;
    double mThresholdDB;
    double mNoiseFloorDB;
    double mRatio;
    bool mNormalize; // MJS
    bool mUsePeak;

    double mDecayTime; // The "Release" time.
    double mAttackFactor;
    double mAttackInverseFactor;
    double mDecayFactor;
    double mThreshold;
    double mCompression;
    double mNoiseFloor;
    int mNoiseCounter;
    double mGain;
    double mLastLevel;
    Floats mFollow1, mFollow2;
    size_t mFollowLen;

    double mMax; // MJS

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter Threshold {
        &LegacyCompressorBase::mThresholdDB, L"Threshold", -12.0, -60.0, -1.0, 1
    };
    static constexpr EffectParameter NoiseFloor {
        &LegacyCompressorBase::mNoiseFloorDB,
        L"NoiseFloor",
        -40.0,
        -80.0,
        -20.0,
        0.2
    };
    static constexpr EffectParameter Ratio {
        &LegacyCompressorBase::mRatio, L"Ratio", 2.0, 1.1, 10.0, 10
    };
    static constexpr EffectParameter AttackTime {
        &LegacyCompressorBase::mAttackTime, L"AttackTime", 0.2, 0.1, 5.0, 100
    };
    static constexpr EffectParameter ReleaseTime {
        &LegacyCompressorBase::mDecayTime, L"ReleaseTime", 1.0, 1.0, 30.0, 10
    };
    static constexpr EffectParameter Normalize {
        &LegacyCompressorBase::mNormalize, L"Normalize", true, false, true, 1
    };
    static constexpr EffectParameter UsePeak {
        &LegacyCompressorBase::mUsePeak, L"UsePeak", false, false, true, 1
    };
};
