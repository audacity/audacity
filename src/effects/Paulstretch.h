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

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // EffectProcessor implementation

   bool DefineParams( ShuttleParams & S ) override;

   // Effect implementation

   double CalcPreviewInputLength(double previewLength) override;
   bool Process() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

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

