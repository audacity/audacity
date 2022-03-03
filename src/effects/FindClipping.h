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

class EffectFindClipping final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectFindClipping();
   virtual ~EffectFindClipping();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // EffectProcessor implementation

   bool DefineParams( ShuttleParams & S ) override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // Effect implementation

   bool Process(EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;
   void DoPopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectFindCliping implementation

   bool ProcessOne(LabelTrack *lt, int count, const WaveTrack * wt,
                   sampleCount start, sampleCount len);

private:
   int mStart;   ///< Using int rather than sampleCount because values are only ever small numbers
   int mStop;    ///< Using int rather than sampleCount because values are only ever small numbers
   // To do: eliminate this
   EffectSettingsAccessPtr mpAccess;
};

#endif // __AUDACITY_EFFECT_FINDCLIPPING__
