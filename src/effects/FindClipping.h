/**********************************************************************

  Audacity: A Digital Audio Editor

  FindClipping.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FINDCLIPPING__
#define __AUDACITY_EFFECT_FINDCLIPPING__

class wxString;

class LabelTrack;

#include "Effect.h"
#include "ShuttleAutomation.h"

class EffectFindClipping final : public StatefulEffect
{
public:
   static inline EffectFindClipping *
   FetchParameters(EffectFindClipping &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectFindClipping();
   virtual ~EffectFindClipping();

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
   void DoPopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access);
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectFindCliping implementation

   bool ProcessOne(LabelTrack *lt, int count, const WaveTrack * wt,
                   sampleCount start, sampleCount len);

   wxWeakRef<wxWindow> mUIParent{};

   int mStart;   ///< Using int rather than sampleCount because values are only ever small numbers
   int mStop;    ///< Using int rather than sampleCount because values are only ever small numbers
   // To do: eliminate this
   EffectSettingsAccessPtr mpAccess;

   const EffectParameterMethods& Parameters() const override;

static constexpr EffectParameter Start{ &EffectFindClipping::mStart,
   L"Duty Cycle Start",  3,    1,    INT_MAX, 1   };
static constexpr EffectParameter Stop{ &EffectFindClipping::mStop,
   L"Duty Cycle End",    3,    1,    INT_MAX, 1   };
};

#endif // __AUDACITY_EFFECT_FINDCLIPPING__
