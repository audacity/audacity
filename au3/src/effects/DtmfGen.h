/**********************************************************************

  Audacity: A Digital Audio Editor

  DtmfGen.h

  Salvo Ventura
  Dec 2006

  An effect that generates DTMF tones

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DTMF__
#define __AUDACITY_EFFECT_DTMF__

#include "DtmfBase.h"
#include "StatelessPerTrackEffect.h"

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class NumericTextCtrl;
class ShuttleGui;

class EffectDtmf final : public DtmfBase, public StatelessEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) const override;

    struct Editor;
};

#endif
