/**********************************************************************

  Audacity: A Digital Audio Editor

  Repair.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPAIR__
#define __AUDACITY_EFFECT_REPAIR__

#include "StatefulEffect.h"

class WaveTrack;

class EffectRepair final : public StatefulEffect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectRepair();
   virtual ~EffectRepair();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool IsInteractive() const override;

   // Effect implementation

   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;

   bool NeedsDither() const override;

private:
   // EffectRepair implementation

   bool ProcessOne(EffectContext &context, int count, WaveTrack * track,
      sampleCount start, size_t len,
      size_t repairStart, // offset relative to start
      size_t repairLen);
};

#endif // __AUDACITY_EFFECT_REPAIT__
