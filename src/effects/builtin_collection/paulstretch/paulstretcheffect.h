/*
 * Audacity: A Digital Audio Editor
 */
/**********************************************************************

   Audacity: A Digital Audio Editor
   paulstretcheffect.h

   Nasca Octavian Paul (Paul Nasca)

 **********************************************************************/
#pragma once

#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-effects/StatefulEffect.h"

#include <cfloat>

class WaveChannel;

namespace au::effects {
class PaulstretchEffect : public StatefulEffect
{
public:
    static inline PaulstretchEffect*
    FetchParameters(PaulstretchEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    PaulstretchEffect();
    virtual ~PaulstretchEffect();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    ::EffectType GetType() const override;
    ::EffectGroup GetGroup() const override { return EffectGroup::PitchAndTempo; }

    // Effect implementation

    double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const override;
    bool Process(::EffectInstance& instance, EffectSettings& settings) override;

    double mAmount = Amount.def;
    double mTime_resolution = Time.def; // seconds

protected:
    // PaulstretchEffect implementation

    size_t GetBufferSize(double rate) const;

    bool ProcessOne(
        const WaveChannel& track, WaveChannel& outputTrack, double t0, double t1, int count);

    const EffectParameterMethods& Parameters() const override;

public:
    static constexpr EffectParameter Amount {
        &PaulstretchEffect::mAmount, L"Stretch Factor", 10.0, 1.0, 1e20, 1
    };
    static constexpr EffectParameter Time { &PaulstretchEffect::mTime_resolution,
                                            L"Time Resolution",
                                            0.25,
                                            0.00099,
                                            1e20, // Things go berserk beyond this point - should still be enough to go crazy :D
                                            1 };
};
}
