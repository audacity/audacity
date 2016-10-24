/**********************************************************************

   Audacity: A Digital Audio Editor
   Paulstretch.h

   Nasca Octavian Paul (Paul Nasca)

 **********************************************************************/

#ifndef __AUDACITY_EFFECT_PAULSTRETCH__
#define __AUDACITY_EFFECT_PAULSTRETCH__

#include <wx/string.h>

#include "Effect.h"

class ShuttleGui;

#define PAULSTRETCH_PLUGIN_SYMBOL XO("Paulstretch")

class EffectPaulstretch final : public Effect
{
public:
   EffectPaulstretch();
   virtual ~EffectPaulstretch();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

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

