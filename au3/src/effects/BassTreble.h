/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2016 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   BassTreble.h (two shelf filters)
   Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASS_TREBLE__
#define __AUDACITY_EFFECT_BASS_TREBLE__

#include "BassTrebleBase.h"
#include "StatelessPerTrackEffect.h"

class ShuttleGui;

class EffectBassTreble final : public BassTrebleBase, public StatelessEffectUIServices
{
public:
    EffectBassTreble() = default;
    ~EffectBassTreble() override = default;

    std::shared_ptr<EffectInstance> MakeInstance() const override;

    struct Editor;

    std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) const override;
};

#endif
