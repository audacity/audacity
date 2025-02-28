/**********************************************************************

  Audacity: A Digital Audio Editor

  FindClipping.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FINDCLIPPING__
#define __AUDACITY_EFFECT_FINDCLIPPING__

class wxString;

#include "FindClippingBase.h"
#include "StatefulEffectUIServices.h"
#include <wx/weakref.h>

class EffectFindClipping : public FindClippingBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    void DoPopulateOrExchange(
        ShuttleGui& S, EffectSettingsAccess& access);
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

private:
    wxWeakRef<wxWindow> mUIParent;
    EffectSettingsAccessPtr mpAccess;
};

#endif // __AUDACITY_EFFECT_FINDCLIPPING__
