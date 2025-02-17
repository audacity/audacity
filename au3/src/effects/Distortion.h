/**********************************************************************

  Audacity: A Digital Audio Editor

  Distortion.h

  Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DISTORTION__
#define __AUDACITY_EFFECT_DISTORTION__

#include "DistortionBase.h"
#include "StatelessPerTrackEffect.h"
#include "ShuttleAutomation.h"

class ShuttleGui;

class EffectDistortion final : public DistortionBase, public StatelessEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) const override;

    struct Editor;

private:
    enum control
    {
        ID_DCBlock = 10001,
        ID_Threshold,
        ID_NoiseFloor,
        ID_Param1,
        ID_Param2,
        ID_Repeats,
    };
};

#endif
