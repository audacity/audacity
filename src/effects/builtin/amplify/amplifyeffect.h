#pragma once

#include "SettingsVisitor.h"
#include "StatefulPerTrackEffect.h"

#include "../common/params.h"

namespace au::effects {
class AmplifyEffect : public StatefulPerTrackEffect
{
public:
    static inline AmplifyEffect* FetchParameters(AmplifyEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    AmplifyEffect();
    virtual ~AmplifyEffect() override;

    // ====================
    // fot view
    // ====================
    // properties
    float peak() const;
    ratio_t defaultRatio() const;
    db_t defaultAmp() const;

    // params
    ratio_t ratio() const;
    Param<db_t> amp() const;      // dB
    void setAmp(db_t v);

    bool canClip() const;
    void setCanClip(bool v);

    // state
    bool isApplyAllowed() const;
    // ====================

    // EffectDefinitionInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    EffectType GetType() const override;
    OptionalMessage LoadFactoryDefaults(EffectSettings& settings) const override;
    OptionalMessage DoLoadFactoryDefaults(EffectSettings& settings);

    unsigned GetAudioInCount() const override;
    unsigned GetAudioOutCount() const override;
    size_t ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

    // Effect implementation

    bool Init() override;
    std::any BeginPreview(const EffectSettings& settings) override;

protected:
    // TODO review this
    struct BUILTIN_EFFECTS_API Instance : StatefulPerTrackEffect::Instance
    {
        using StatefulPerTrackEffect::Instance::Instance;
        ~Instance() override;
    };

protected:
    void ClampRatio();

    // AmplifyEffect implementation
protected:
    double mPeak = 1.0;

    double mRatio = 1.0;
    double mRatioClip =1.0; // maximum value of mRatio which does not cause clipping
    double mAmp = 0.0;
    double mNewPeak = 1.0;
    bool mCanClip = true;

private:
    const EffectParameterMethods& Parameters() const override;

protected:
    static constexpr EffectParameter Ratio {
        &AmplifyEffect::mRatio, L"Ratio", 0.9f, 0.003162f, 316.227766f, 1.0f
    };
    // Amp is not saved in settings!
    static constexpr EffectParameter Amp {
        &AmplifyEffect::mAmp, L"", -0.91515f, -50.0f, 50.0f, 10.0f
    };
    static constexpr EffectParameter Clipping {
        &AmplifyEffect::mCanClip, L"AllowClipping", false, false, true, 1
    };
};
}
