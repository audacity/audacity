#pragma once

#include "SettingsVisitor.h"
#include "SoundTouchBase.h"

class BUILTIN_EFFECTS_API ChangeTempoBase : public SoundTouchBase
{
public:
    static inline ChangeTempoBase*
    FetchParameters(ChangeTempoBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    ChangeTempoBase();
    virtual ~ChangeTempoBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    bool SupportsAutomation() const override;

    // Effect implementation

    bool Init() override;
    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;
    bool Process(EffectInstance& instance, EffectSettings& settings) override;
    double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const override;

protected:
    const EffectParameterMethods& Parameters() const override;

    bool mUseSBSMS;
    double m_PercentChange; // percent change to apply to tempo
                            // -100% is meaningless, but sky's the upper limit
    double m_FromBPM;      // user-set beats-per-minute. Zero means not yet set.
    double m_ToBPM;        // Zero value means not yet set.
    double m_FromLength;   // starting length of selection
    double m_ToLength;     // target length of selection

    bool m_bLoopDetect;

    static constexpr EffectParameter Percentage {
        &ChangeTempoBase::m_PercentChange, L"Percentage", 0.0, -95.0, 3000.0, 1
    };
    static constexpr EffectParameter UseSBSMS {
        &ChangeTempoBase::mUseSBSMS, L"SBSMS", false, false, true, 1
    };
};
