/**********************************************************************

  Audacity: A Digital Audio Editor

  FindClipping.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FINDCLIPPING__
#define __AUDACITY_EFFECT_FINDCLIPPING__

class wxString;

#include <wx/string.h>

#include "../LabelTrack.h"
#include "../WaveTrack.h"

#include "Effect.h"

#define FINDCLIPPING_PLUGIN_SYMBOL XO("Find Clipping")

class EffectFindClipping : public Effect
{
public:
   EffectFindClipping();
   virtual ~EffectFindClipping();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect implementation

   virtual bool Process();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   // EffectFindCliping implementation

   bool ProcessOne(LabelTrack *l, int count, WaveTrack * t,
                   sampleCount start, sampleCount len);

private:
   int mStart;   ///< Using int rather than sampleCount because values are only ever small numbers
   int mStop;    ///< Using int rather than sampleCount because values are only ever small numbers
};

#endif // __AUDACITY_EFFECT_FINDCLIPPING__
