/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   Reverb.h
   Rob Sykes, Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERB__
#define __AUDACITY_EFFECT_REVERB__

#include "ReverbBase.h"
#include "StatelessPerTrackEffect.h"

class EffectReverb : public ReverbBase, public StatelessEffectUIServices
{
public:
    std::shared_ptr<EffectInstance> MakeInstance() const override;

    std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) const override;

    struct Editor;
};

#endif
