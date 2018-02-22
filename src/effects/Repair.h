/**********************************************************************

  Audacity: A Digital Audio Editor

  Repair.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPAIR__
#define __AUDACITY_EFFECT_REPAIR__

#include <wx/string.h>

#include "Effect.h"

#define REPAIR_PLUGIN_SYMBOL XO("Repair")

class WaveTrack;

class EffectRepair final : public Effect
{
public:
   EffectRepair();
   virtual ~EffectRepair();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;
   bool IsInteractive() override;

   // Effect implementation

   bool Process() override;

private:
   // EffectRepair implementaion

   bool ProcessOne(int count, WaveTrack * track,
                   sampleCount start,
                   size_t len,
                   size_t repairStart, // offset relative to start
                   size_t repairLen);
};

#endif // __AUDACITY_EFFECT_REPAIT__
