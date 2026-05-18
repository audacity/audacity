#pragma once

#include "shared/types/decibel.h"

#include "au3-components/SettingsVisitor.h"
#include "au3-effects/StatefulPerTrackEffect.h"

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
    shared::Decibel defaultAmp() const;

    // params
    ratio_t ratio() const;
    Param<shared::Decibel> amp() const;
    void setAmp(shared::Decibel v);

    bool canClip() const;
    void setCanClip(bool v);

    // state
    bool isApplyAllowed() const;
    // ====================

    // EffectDefinitionInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    ::EffectType GetType() const override;
    ::EffectGroup GetGroup() const override;
    OptionalMessage LoadFactoryDefaults(EffectSettings& settings) const override;
    OptionalMessage DoLoadFactoryDefaults(EffectSettings& settings);
    bool ParamsAreInputAgnostic() const override { return false; }

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
    void SetRatio(double);

    // AmplifyEffect implementation
protected:
    double mPeak = 0.0;
    double mRatio = 1.0;
    bool mCanClip = true;

private:
    const EffectParameterMethods& Parameters() const override;

protected:
    static constexpr EffectParameter Ratio {
        &AmplifyEffect::mRatio, L"Ratio", 0.9f, 0.003162f, 316.227766f, 1.0f
    };
    static constexpr EffectParameter Clipping {
        &AmplifyEffect::mCanClip, L"AllowClipping", false, false, true, 1
    };
};
}
