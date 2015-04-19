/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.h

  Dominic Mazzoni

  An effect to add silence.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SILENCE__
#define __AUDACITY_EFFECT_SILENCE__

#include <wx/string.h>

#include "../WaveTrack.h"
#include "../widgets/NumericTextCtrl.h"

#include "Generator.h"

#define SILENCE_PLUGIN_SYMBOL XO("Silence")

class EffectSilence : public Generator
{
public:
   EffectSilence();
   virtual ~EffectSilence();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // Effect implementation

   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

protected:
   // Generator implementation

   bool GenerateTrack(WaveTrack *tmp, const WaveTrack &track, int ntrack);

private:
   NumericTextCtrl *mDurationT;
};

#endif
