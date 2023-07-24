/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.h

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERSE__
#define __AUDACITY_EFFECT_REVERSE__

#include "StatefulEffect.h"
#include <functional>

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

   bool Process(EffectInstance &instance, EffectSettings &settings) override;

private:
   // EffectReverse implementation

   //! Argument is in (0, 1)
   //! @return true if processing should continue
   using ProgressReport = std::function<bool(double)>;
   bool ReverseOneClip(WaveTrack &track,
      sampleCount start, sampleCount len, sampleCount originalStart,
      sampleCount originalEnd, const ProgressReport &report = {});
   bool Reverse(WaveTrack &track,
      sampleCount start, sampleCount len, const ProgressReport &report = {});
   bool ReverseOne(WaveTrack &track,
      sampleCount start, sampleCount len, const ProgressReport &report = {});
 };

#endif

