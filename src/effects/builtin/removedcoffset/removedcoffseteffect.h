/**********************************************************************

  Audacity: A Digital Audio Editor

  RemoveDCOffsetEffect.h

  Extracted from Normalize.cpp by drtootsie

**********************************************************************/
#pragma once

#include "au3-effects/StatefulEffect.h"
#include "au3-command-parameters/ShuttleAutomation.h"

class WaveChannel;

namespace au::effects {
class RemoveDCOffsetEffect : public StatefulEffect
{
public:
    static inline RemoveDCOffsetEffect*
    FetchParameters(RemoveDCOffsetEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    RemoveDCOffsetEffect();
    virtual ~RemoveDCOffsetEffect();

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;
    ::EffectType GetType() const override;

    bool Process(::EffectInstance& instance, ::EffectSettings& settings) override;

private:
    bool ProcessOne(
        WaveChannel& track, const TranslatableString& msg, double& progress, float offset);
    using ProgressReport = std::function<bool (double fraction)>;
    static bool AnalyseTrackData(
        const WaveChannel& track, const ProgressReport& report, double curT0, double curT1, float& offset);
    static double AnalyseDataDC(float* buffer, size_t len, double sum);
    void ProcessData(float* buffer, size_t len, float offset);

    double mCurT0;
    double mCurT1;

    const EffectParameterMethods& Parameters() const override;
};
}
