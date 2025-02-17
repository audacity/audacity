/**********************************************************************

  Audacity: A Digital Audio Editor

  Equalization.h

  Mitch Golden
  Vaughan Johnson (Preview)

***********************************************************************/

#ifndef __AUDACITY_EFFECT_EQUALIZATION__
#define __AUDACITY_EFFECT_EQUALIZATION__

#include <wx/setup.h> // for wxUSE_* macros

#include "EqualizationBase.h"
#include "EqualizationUI.h"
#include "StatefulEffectUIServices.h"

class EffectEqualization : public EqualizationBase, public StatefulEffectUIServices
{
    using EqualizationBase::EqualizationBase;

    ComponentInterfaceSymbol GetSymbol() const override;

    bool ValidateUI(const EffectPlugin& plugin, EffectSettings&) const override;

    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;

private:
    wxWeakRef<wxWindow> mUIParent {};
    EqualizationUI mUI { *this, mUIParent, GetName(), mCurvesList, mOptions };
};

class EffectEqualizationCurve final : public EffectEqualization
{
public:
    static const ComponentInterfaceSymbol Symbol;

    EffectEqualizationCurve()
        : EffectEqualization(kEqOptionCurve) {}
};

class EffectEqualizationGraphic final : public EffectEqualization
{
public:
    static const ComponentInterfaceSymbol Symbol;

    EffectEqualizationGraphic()
        : EffectEqualization(kEqOptionGraphic) {}
};

#endif
