/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseReduction.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)
  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE_REDUCTION__
#define __AUDACITY_EFFECT_NOISE_REDUCTION__

#include "NoiseReductionBase.h"
#include "StatefulEffectUIServices.h"

class EffectNoiseReduction final : public NoiseReductionBase, public StatefulEffectUIServices
{
public:
    int ShowHostInterface(EffectBase& plugin, wxWindow& parent, const EffectDialogFactory& factory,
                          std::shared_ptr<EffectInstance>& pInstance, EffectSettingsAccess& access, bool forceModal = false) override;

    class Dialog;
private:
    friend class Dialog;
};

#endif
