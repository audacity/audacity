/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_STEREO_TO_MONO__
#define __AUDACITY_EFFECT_STEREO_TO_MONO__

#include "Effect.h"

class EffectStereoToMono final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectStereoToMono();
   virtual ~EffectStereoToMono();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool IsInteractive() override;

   // EffectProcessor implementation

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;

   // Effect implementation

   bool Process() override;
   bool IsHiddenFromMenus() override;

private:
   // EffectStereoToMono implementation

   bool ProcessOne(sampleCount & curTime, sampleCount totalTime, WaveTrack *left, WaveTrack *right);

};

#endif

