/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.h

  Dominic Mazzoni

  An effect to add white noise.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE__
#define __AUDACITY_EFFECT_NOISE__

#include "NoiseBase.h"
#include "StatefulEffectUIServices.h"
#include <wx/weakref.h>

class NumericTextCtrl;
class ShuttleGui;

class EffectNoise : public NoiseBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

private:
    NumericTextCtrl* mNoiseDurationT;
    wxWeakRef<wxWindow> mUIParent {};
};

#endif
