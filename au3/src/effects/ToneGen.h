/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.h

  Steve Jolly

  This class implements a tone generator effect.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TONEGEN__
#define __AUDACITY_EFFECT_TONEGEN__

#include "ShuttleAutomation.h"
#include "StatefulEffectUIServices.h"
#include "StatefulPerTrackEffect.h"
#include "ToneGenBase.h"
#include <float.h> // for DBL_MAX
#include <wx/weakref.h>

class NumericTextCtrl;
class ShuttleGui;

class EffectToneGen : public ToneGenBase, public StatefulEffectUIServices
{
public:
    EffectToneGen(bool isChirp)
        : ToneGenBase{isChirp}
    {
    }

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // Effect implementation

    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

protected:
    DECLARE_EVENT_TABLE()

private:
    // ToneGenBase implementation

    void OnControlUpdate(wxCommandEvent& evt);

    wxWeakRef<wxWindow> mUIParent{};
    NumericTextCtrl* mToneDurationT;
};

class EffectChirp final : public EffectToneGen
{
public:
    static const ComponentInterfaceSymbol Symbol;

    EffectChirp()
        : EffectToneGen{true} {}
    ~EffectChirp() override = default;
};

class EffectTone final : public EffectToneGen
{
public:
    static const ComponentInterfaceSymbol Symbol;

    EffectTone()
        : EffectToneGen{false} {}
    ~EffectTone() override = default;
};

#endif
