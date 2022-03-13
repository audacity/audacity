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
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // EffectProcessor implementation

   bool VisitSettings( SettingsVisitor & S ) override;
   bool GetAutomationParameters(CommandParameters & parms) const override;
   bool SetAutomationParameters(const CommandParameters & parms) override;

   // Effect implementation

   bool Process(EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;
   void DoPopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access);
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectFindCliping implementation

   bool ProcessOne(LabelTrack *lt, int count, const WaveTrack * wt,
                   sampleCount start, sampleCount len);

public: // TODO remove
   int mStart;   ///< Using int rather than sampleCount because values are only ever small numbers
   int mStop;    ///< Using int rather than sampleCount because values are only ever small numbers
   // To do: eliminate this
   EffectSettingsAccessPtr mpAccess;
};

#endif // __AUDACITY_EFFECT_FINDCLIPPING__
