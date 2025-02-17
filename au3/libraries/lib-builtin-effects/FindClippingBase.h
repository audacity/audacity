/**********************************************************************

  Audacity: A Digital Audio Editor

  FindClippingBase.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/
#pragma once

class LabelTrack;
class WaveChannel;

#include "ShuttleAutomation.h"
#include "StatefulEffect.h"

class BUILTIN_EFFECTS_API FindClippingBase : public StatefulEffect
{
public:
    static inline FindClippingBase*
    FetchParameters(FindClippingBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    FindClippingBase();
    virtual ~FindClippingBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    // Effect implementation

    bool Process(EffectInstance& instance, EffectSettings& settings) override;

private:
    // EffectFindCliping implementation

    bool ProcessOne(
        LabelTrack& lt, int count, const WaveChannel& wt, sampleCount start, sampleCount len);

protected:
    int mStart; ///< Using int rather than sampleCount because values are only
                ///< ever small numbers
    int mStop; ///< Using int rather than sampleCount because values are only
               ///< ever small numbers
    // To do: eliminate this

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter Start {
        &FindClippingBase::mStart, L"Duty Cycle Start", 3, 1, INT_MAX, 1
    };
    static constexpr EffectParameter Stop {
        &FindClippingBase::mStop, L"Duty Cycle End", 3, 1, INT_MAX, 1
    };
};
