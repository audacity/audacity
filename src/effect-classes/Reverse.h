/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.h

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERSE__
#define __AUDACITY_EFFECT_REVERSE__

#include "Effect.h"

class EffectReverse final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectReverse();
   virtual ~EffectReverse();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool IsInteractive() override;

   // Effect implementation

   bool Process() override;

private:
   // EffectReverse implementation

   bool ProcessOneClip(int count, WaveTrack* track,
                   sampleCount start, sampleCount len, sampleCount originalStart, sampleCount originalEnd);
   bool ProcessOneWave(int count, WaveTrack* track, sampleCount start, sampleCount len);
 };

#endif

