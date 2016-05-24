/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_ECHO__
#define __AUDACITY_EFFECT_ECHO__

#include <wx/event.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "Effect.h"

class ShuttleGui;

#define ECHO_PLUGIN_SYMBOL XO("Echo")

class EffectEcho final : public Effect
{
public:
   EffectEcho();
   virtual ~EffectEcho();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   int GetAudioInCount() override;
   int GetAudioOutCount() override;
   bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) override;
   bool ProcessFinalize() override;
   sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen) override;
   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   // Effect implementation
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectEcho implementation

private:
   double delay;
   double decay;
   float *history;
   sampleCount histPos;
   sampleCount histLen;
};

#endif // __AUDACITY_EFFECT_ECHO__
