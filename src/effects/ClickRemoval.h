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

#include "../Envelope.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include "Effect.h"

#define CLICKREMOVAL_PLUGIN_SYMBOL XO("Click Removal")

class EffectClickRemoval : public Effect
{
public:
   EffectClickRemoval();
   virtual ~EffectClickRemoval();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect implementation

   virtual bool CheckWhetherSkipEffect();
   virtual bool Startup();
   virtual bool Process();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   bool ProcessOne(int count, WaveTrack * track,
                   sampleCount start, sampleCount len);

   bool RemoveClicks(sampleCount len, float *buffer);

   void OnWidthText(wxCommandEvent & evt);
   void OnThreshText(wxCommandEvent & evt);
   void OnWidthSlider(wxCommandEvent & evt);
   void OnThreshSlider(wxCommandEvent & evt);

private:
   Envelope *mEnvelope;

   bool mbDidSomething; // This effect usually does nothing on real-world data.
   int windowSize;
   int mThresholdLevel;
   int mClickWidth;
   int sep;

   wxSlider *mWidthS;
   wxSlider *mThreshS;
   wxTextCtrl *mWidthT;
   wxTextCtrl *mThreshT;

   DECLARE_EVENT_TABLE();
};

#endif
