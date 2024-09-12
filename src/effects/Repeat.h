/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPEAT__
#define __AUDACITY_EFFECT_REPEAT__

#include "StatefulEffect.h"
#include "StatefulEffectUIServices.h"
#include "ShuttleAutomation.h"
#include <wx/weakref.h>

class wxTextCtrl;
class ShuttleGui;

class wxStaticText;

class RepeatBase : public StatefulEffect
{
public:
   static inline RepeatBase *
   FetchParameters(RepeatBase &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   RepeatBase();
   virtual ~RepeatBase();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   bool Process(EffectInstance &instance, EffectSettings &settings) override;

   bool NeedsDither() const override;

protected:
   // RepeatBase implementation
   int repeatCount;

   const EffectParameterMethods& Parameters() const override;

static constexpr EffectParameter Count{ &RepeatBase::repeatCount,
   L"Count",1,  1,    INT_MAX,  1  };
};

class EffectRepeat final : public RepeatBase, public StatefulEffectUIServices
{
public:
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   void OnRepeatTextChange(wxCommandEvent & evt);
   void DisplayNewTime();

   wxWeakRef<wxWindow> mUIParent{};

   wxTextCtrl   *mRepeatCount;
   wxStaticText *mCurrentTime;
   wxStaticText *mTotalTime;
   DECLARE_EVENT_TABLE()
};

#endif
