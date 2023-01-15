/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_STEREO_TO_MONO__
#define __AUDACITY_EFFECT_STEREO_TO_MONO__

#include "StatefulEffect.h"

class EffectStereoToMono final : public StatefulEffect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectStereoToMono();
   virtual ~EffectStereoToMono();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool IsInteractive() const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   // Effect implementation

   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;
   bool IsHiddenFromMenus() const override;

private:
   // EffectStereoToMono implementation

   bool ProcessOne(EffectContext &context,
      sampleCount & curTime, sampleCount totalTime,
      WaveTrack *left, WaveTrack *right);

};

#endif

