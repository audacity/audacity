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
    bool SupportsMultipleClipSelection() const override { return false; }

    // Effect implementation

    double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const override;
    bool Process(::EffectInstance& instance, EffectSettings& settings) override;

    float mAmount;
    float mTime_resolution; // seconds

protected:
    // PaulstretchEffect implementation

    size_t GetBufferSize(double rate) const;

    bool ProcessOne(
        const WaveChannel& track, WaveChannel& outputTrack, double t0, double t1, int count);

    const EffectParameterMethods& Parameters() const override;

public:
    static constexpr EffectParameter Amount {
        &PaulstretchEffect::mAmount, L"Stretch Factor", 10.0f, 1.0, FLT_MAX, 1
    };
    static constexpr EffectParameter Time { &PaulstretchEffect::mTime_resolution,
                                            L"Time Resolution",
                                            0.25f,
                                            0.00099f,
                                            FLT_MAX,
                                            1 };
};
}
