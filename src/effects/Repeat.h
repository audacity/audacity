/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPEAT__
#define __AUDACITY_EFFECT_REPEAT__

#include "Effect.h"
#include "ShuttleAutomation.h"

class wxTextCtrl;
class ShuttleGui;

class wxStaticText;

class EffectRepeat final : public StatefulEffect
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

   bool Process(EffectInstance &instance, EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

   bool NeedsDither() const override;

private:
   // EffectRepeat implementation

   void OnRepeatTextChange(wxCommandEvent & evt);
   void DisplayNewTime();

   wxWeakRef<wxWindow> mUIParent{};

   int repeatCount;

   wxTextCtrl   *mRepeatCount;
   wxStaticText *mCurrentTime;
   wxStaticText *mTotalTime;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter Count{ &EffectRepeat::repeatCount,
   L"Count",1,  1,    INT_MAX,  1  };
};

#endif
