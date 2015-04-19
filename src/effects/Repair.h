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

class EffectRepair : public Effect
{
public:
   EffectRepair();
   virtual ~EffectRepair();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();
   virtual bool IsInteractive();

   // Effect implementation

   virtual bool Process();

private:
   // EffectRepair implementaion

   bool ProcessOne(int count, WaveTrack * track,
                   sampleCount start,
                   sampleCount len,
                   sampleCount repairStart, sampleCount repairLen);
};

#endif // __AUDACITY_EFFECT_REPAIT__
