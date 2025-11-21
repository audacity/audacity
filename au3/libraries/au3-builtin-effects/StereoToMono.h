/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.h

  Lynn Allan

**********************************************************************/
#pragma once

#include "StatefulEffect.h"

class BUILTIN_EFFECTS_API StereoToMono : public StatefulEffect
{
public:
    static const ComponentInterfaceSymbol Symbol;

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    bool IsInteractive() const override;

    unsigned GetAudioInCount() const override;
    unsigned GetAudioOutCount() const override;

    // Effect implementation

    bool Process(EffectInstance& instance, EffectSettings& settings) override;
    bool IsHiddenFromMenus() const override;

private:
    // EffectStereoToMono implementation

    bool ProcessOne(
        TrackList& outputs, sampleCount& curTime, sampleCount totalTime, WaveTrack& track);
};
