/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemoval.h

  Craig DeForest

  (Structure largely stolen from NoiseRemoval.h by Dominic Mazzoni)

  This file is intended to become part of Audacity.  You may modify and/or
  distribute it under the same terms as Audacity itself.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CLICK_REMOVAL__
#define __AUDACITY_EFFECT_CLICK_REMOVAL__

#include <wx/event.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "Effect.h"

class Envelope;
class ShuttleGui;

#define CLICKREMOVAL_PLUGIN_SYMBOL XO("Click Removal")

class EffectClickRemoval final : public Effect
{
public:
   EffectClickRemoval();
   virtual ~EffectClickRemoval();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   // Effect implementation

   bool CheckWhetherSkipEffect() override;
   bool Startup() override;
   bool Process() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   bool ProcessOne(int count, WaveTrack * track,
                   sampleCount start, sampleCount len);

   bool RemoveClicks(int len, float *buffer);

   void OnWidthText(wxCommandEvent & evt);
   void OnThreshText(wxCommandEvent & evt);
   void OnWidthSlider(wxCommandEvent & evt);
   void OnThreshSlider(wxCommandEvent & evt);

private:
   Envelope *mEnvelope;

   bool mbDidSomething; // This effect usually does nothing on real-world data.
   size_t windowSize;
   int mThresholdLevel;
   int mClickWidth;
   int sep;

   wxSlider *mWidthS;
   wxSlider *mThreshS;
   wxTextCtrl *mWidthT;
   wxTextCtrl *mThreshT;

   DECLARE_EVENT_TABLE()
};

#endif
