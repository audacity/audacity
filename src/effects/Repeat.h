/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPEAT__
#define __AUDACITY_EFFECT_REPEAT__

#include "Effect.h"
#include "../ShuttleAutomation.h"

class wxTextCtrl;
class ShuttleGui;

class wxStaticText;

class EffectRepeat final : public Effect
{
public:
   static inline EffectRepeat *
   FetchParameters(EffectRepeat &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectRepeat();
   virtual ~EffectRepeat();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   bool Process(EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectRepeat implementation

   void OnRepeatTextChange(wxCommandEvent & evt);
   void DisplayNewTime();

public: // TODO remove
   int repeatCount;

   wxTextCtrl   *mRepeatCount;
   wxStaticText *mCurrentTime;
   wxStaticText *mTotalTime;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()
};

#endif

