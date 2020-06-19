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

#include "Effect.h"

class wxSlider;
class wxTextCtrl;
class Envelope;
class ShuttleGui;

class EffectClickRemoval final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectClickRemoval();
   virtual ~EffectClickRemoval();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   wxString ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool DefineParams( ShuttleParams & S ) override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

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

   bool RemoveClicks(size_t len, float *buffer);

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
