/**********************************************************************

  Audacity: A Digital Audio Editor

  NormalizeEffect.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/
#pragma once

#include "libraries/lib-effects/StatefulEffect.h"
#include "libraries/lib-command-parameters/ShuttleAutomation.h"

class WaveChannel;

namespace au::effects {
class NormalizeEffect : public StatefulEffect
{
public:
    static inline NormalizeEffect*
    FetchParameters(NormalizeEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    NormalizeEffect();
    virtual ~NormalizeEffect();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    ::EffectType GetType() const override;

    // Effect implementation

    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;
    bool Process(::EffectInstance& instance, ::EffectSettings& settings) override;

private:
    // NormalizeEffect implementation

    bool ProcessOne(
        WaveChannel& track, const TranslatableString& msg, double& progress, float offset);
    using ProgressReport = std::function<bool (double fraction)>;
    static bool AnalyseTrack(
        const WaveChannel& track, const ProgressReport& report, bool gain, bool dc, double curT0, double curT1, float& offset,
        float& extent);
    static bool AnalyseTrackData(
        const WaveChannel& track, const ProgressReport& report, double curT0, double curT1, float& offset);
    static double AnalyseDataDC(float* buffer, size_t len, double sum);
    void ProcessData(float* buffer, size_t len, float offset);

public:
    double mPeakLevel;
    bool mGain;
    bool mDC;
    bool mStereoInd;

protected:
    double mCurT0;
    double mCurT1;
    float mMult;

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter PeakLevel {
        &NormalizeEffect::mPeakLevel, L"PeakLevel", -1.0, -145.0, 0.0, 1
    };
    static constexpr EffectParameter RemoveDC {
        &NormalizeEffect::mDC, L"RemoveDcOffset", true, false, true, 1
    };
    static constexpr EffectParameter ApplyVolume {
        &NormalizeEffect::mGain, L"ApplyVolume", true, false, true, 1
    };
    static constexpr EffectParameter StereoInd {
        &NormalizeEffect::mStereoInd, L"StereoIndependent", false, false, true, 1
    };
};
}
