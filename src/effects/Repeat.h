/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPEAT__
#define __AUDACITY_EFFECT_REPEAT__

#include <wx/event.h>
#include <wx/string.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "Effect.h"

class ShuttleGui;

#define REPEAT_PLUGIN_SYMBOL XO("Repeat")

class EffectRepeat final : public Effect
{
public:
   EffectRepeat();
   virtual ~EffectRepeat();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   // Effect implementation

   bool Process() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectRepeat implementation

   void OnRepeatTextChange(wxCommandEvent & evt);
   void DisplayNewTime();

private:
   int repeatCount;

   wxTextCtrl   *mRepeatCount;
   wxStaticText *mCurrentTime;
   wxStaticText *mTotalTime;

   DECLARE_EVENT_TABLE()
};

#endif

