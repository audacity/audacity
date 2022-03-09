/**********************************************************************

   Audacity: A Digital Audio Editor
   Paulstretch.h

   Nasca Octavian Paul (Paul Nasca)

 **********************************************************************/

#ifndef __AUDACITY_EFFECT_PAULSTRETCH__
#define __AUDACITY_EFFECT_PAULSTRETCH__

#include "Effect.h"

class ShuttleGui;

class EffectPaulstretch final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectPaulstretch();
   virtual ~EffectPaulstretch();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // EffectProcessor implementation

   bool DefineParams( ShuttleParams & S ) override;

   // Effect implementation

   double CalcPreviewInputLength(
      const EffectSettings &settings, double previewLength) override;
   bool Process(EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectPaulstretch implementation
   
   void OnText(wxCommandEvent & evt);
   size_t GetBufferSize(double rate);

   bool ProcessOne(WaveTrack *track, double t0, double t1, int count);

private:
   float mAmount;
   float mTime_resolution;  //seconds
   double m_t1;

   DECLARE_EVENT_TABLE()
};

#endif

