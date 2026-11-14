/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_ECHO__
#define __AUDACITY_EFFECT_ECHO__

#include "EchoBase.h"
#include "ShuttleAutomation.h"
#include "StatelessPerTrackEffect.h"
#include <float.h> // for FLT_MAX

class ShuttleGui;

class EffectEcho final : public EchoBase, public StatelessEffectUIServices
{
public:

    struct Editor;

    // Effect implementation
    std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs)
    const override;

    std::shared_ptr<EffectInstance> MakeInstance() const override;
};

#endif // __AUDACITY_EFFECT_ECHO__
