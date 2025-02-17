/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah

  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_WAHWAH__
#define __AUDACITY_EFFECT_WAHWAH__

#include "StatelessPerTrackEffect.h"
#include "WahWahBase.h"

class ShuttleGui;

class EffectWahwah : public WahWahBase, public StatelessEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) const override;

private:
    struct Editor;
};

#endif
