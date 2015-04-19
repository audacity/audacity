/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_WAHWAH__
#define __AUDACITY_EFFECT_WAHWAH__

#include <wx/event.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "../ShuttleGui.h"

#include "Effect.h"

#define WAHWAH_PLUGIN_SYMBOL XO("Wahwah")

class EffectWahwah : public Effect
{
public:
   EffectWahwah();
   virtual ~EffectWahwah();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual int GetAudioInCount();
   virtual int GetAudioOutCount();
   virtual bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL);
   virtual sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen);
   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect implementation

   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   // EffectWahwah implementation

   void OnFreqSlider(wxCommandEvent & evt);
   void OnPhaseSlider(wxCommandEvent & evt);
   void OnDepthSlider(wxCommandEvent & evt);
   void OnResonanceSlider(wxCommandEvent & evt);
   void OnFreqOffSlider(wxCommandEvent & evt);

   void OnFreqText(wxCommandEvent & evt);
   void OnPhaseText(wxCommandEvent & evt);
   void OnDepthText(wxCommandEvent & evt);
   void OnResonanceText(wxCommandEvent & evt);
   void OnFreqOffText(wxCommandEvent & evt);

private:
   double depth;
   double freqofs;
   double phase;
   double lfoskip;
   unsigned long skipcount;
   double xn1, xn2, yn1, yn2;
   double b0, b1, b2, a0, a1, a2;

/* Parameters:
   mFreq - LFO frequency
   mPhase - LFO startphase in RADIANS - useful for stereo WahWah
   mDepth - Wah depth
   mRes - Resonance
   mFreqOfs - Wah frequency offset

   !!!!!!!!!!!!! IMPORTANT!!!!!!!!! :
   mDepth and mFreqOfs should be from 0(min) to 1(max) !
   mRes should be greater than 0 !  */

   double mFreq;
   double mPhase;
   int mDepth;
   double mRes;
   int mFreqOfs;

   wxTextCtrl *mFreqT;
   wxTextCtrl *mPhaseT;
   wxTextCtrl *mDepthT;
   wxTextCtrl *mResT;
   wxTextCtrl *mFreqOfsT;

   wxSlider *mFreqS;
   wxSlider *mPhaseS;
   wxSlider *mDepthS;
   wxSlider *mResS;
   wxSlider *mFreqOfsS;

   DECLARE_EVENT_TABLE();
};

#endif

