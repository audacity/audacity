/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.h

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERSE__
#define __AUDACITY_EFFECT_REVERSE__

#include "StatefulEffect.h"

class EffectReverse final : public StatefulEffect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectReverse();
   virtual ~EffectReverse();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool IsInteractive() const override;

   // Effect implementation

   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;

private:
   // EffectReverse implementation

   bool ProcessOneClip(EffectContext &context, int count, WaveTrack* track,
      sampleCount start, sampleCount len,
      sampleCount originalStart, sampleCount originalEnd);
   bool ProcessOneWave(EffectContext &context, int count,
      WaveTrack* track, sampleCount start, sampleCount len);
 };

#endif

