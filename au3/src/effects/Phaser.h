/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_PHASER__
#define __AUDACITY_EFFECT_PHASER__

#include "PhaserBase.h"
#include "StatelessPerTrackEffect.h"

class ShuttleGui;

class EffectPhaser : public PhaserBase, public StatelessEffectUIServices
{
public:
    // Effect implementation

    std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs)
    const override;

    struct Editor;
};

#endif
