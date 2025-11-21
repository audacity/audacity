/**********************************************************************

   Audacity: A Digital Audio Editor
   PaulstretchBase.h

   Nasca Octavian Paul (Paul Nasca)

 **********************************************************************/
#pragma once

#include "ShuttleAutomation.h"
#include "StatefulEffect.h"
#include <cfloat>

class WaveChannel;

class BUILTIN_EFFECTS_API PaulstretchBase : public StatefulEffect
{
public:
    static inline PaulstretchBase*
    FetchParameters(PaulstretchBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    PaulstretchBase();
    virtual ~PaulstretchBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    // Effect implementation

    double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const override;
    bool Process(EffectInstance& instance, EffectSettings& settings) override;

protected:
    // PaulstretchBase implementation

    size_t GetBufferSize(double rate) const;

    bool ProcessOne(
        const WaveChannel& track, WaveChannel& outputTrack, double t0, double t1, int count);

    float mAmount;
    float mTime_resolution; // seconds

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter Amount {
        &PaulstretchBase::mAmount, L"Stretch Factor", 10.0f, 1.0, FLT_MAX, 1
    };
    static constexpr EffectParameter Time { &PaulstretchBase::mTime_resolution,
                                            L"Time Resolution",
                                            0.25f,
                                            0.00099f,
                                            FLT_MAX,
                                            1 };
};
