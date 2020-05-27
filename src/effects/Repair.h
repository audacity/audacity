/**********************************************************************

  Audacity: A Digital Audio Editor

  Repair.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPAIR__
#define __AUDACITY_EFFECT_REPAIR__

#include "Effect.h"

class WaveTrack;

class EffectRepair final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectRepair();
   virtual ~EffectRepair();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool IsInteractive() override;

   // Effect implementation

   bool Process() override;

private:
   // EffectRepair implementation

   bool ProcessOne(int count, WaveTrack * track,
                   sampleCount start,
                   size_t len,
                   size_t repairStart, // offset relative to start
                   size_t repairLen);
};

#endif // __AUDACITY_EFFECT_REPAIT__
