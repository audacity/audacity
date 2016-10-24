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

#include "Effect.h"

class ShuttleGui;

#define WAHWAH_PLUGIN_SYMBOL XO("Wahwah")

class EffectWahwahState
{
public:
   float samplerate;
   double depth;
   double freqofs;
   double phase;
   double outgain;
   double lfoskip;
   unsigned long skipcount;
   double xn1, xn2, yn1, yn2;
   double b0, b1, b2, a0, a1, a2;
};

WX_DECLARE_OBJARRAY(EffectWahwahState, EffectWahwahStateArray);

class EffectWahwah final : public Effect
{
public:
   EffectWahwah();
   virtual ~EffectWahwah();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;
   bool SupportsRealtime() override;

   // EffectClientInterface implementation

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;
   bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;
   bool RealtimeInitialize() override;
   bool RealtimeAddProcessor(unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize() override;
   size_t RealtimeProcess(int group,
                                       float **inbuf,
                                       float **outbuf,
                                       size_t numSamples) override;
   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   // Effect implementation

   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectWahwah implementation

   void InstanceInit(EffectWahwahState & data, float sampleRate);
   size_t InstanceProcess(EffectWahwahState & data, float **inBlock, float **outBlock, size_t blockLen);

   void OnFreqSlider(wxCommandEvent & evt);
   void OnPhaseSlider(wxCommandEvent & evt);
   void OnDepthSlider(wxCommandEvent & evt);
   void OnResonanceSlider(wxCommandEvent & evt);
   void OnFreqOffSlider(wxCommandEvent & evt);
   void OnGainSlider(wxCommandEvent & evt);

   void OnFreqText(wxCommandEvent & evt);
   void OnPhaseText(wxCommandEvent & evt);
   void OnDepthText(wxCommandEvent & evt);
   void OnResonanceText(wxCommandEvent & evt);
   void OnFreqOffText(wxCommandEvent & evt);
   void OnGainText(wxCommandEvent & evt);

private:
   EffectWahwahState mMaster;
   EffectWahwahStateArray mSlaves;

   /* Parameters:
   mFreq - LFO frequency
   mPhase - LFO startphase in RADIANS - useful for stereo WahWah
   mDepth - Wah depth
   mRes - Resonance
   mFreqOfs - Wah frequency offset
   mOutGain - output gain

   !!!!!!!!!!!!! IMPORTANT!!!!!!!!! :
   mDepth and mFreqOfs should be from 0(min) to 1(max) !
   mRes should be greater than 0 !  */

   double mFreq;
   double mPhase;
   int mDepth;
   double mRes;
   int mFreqOfs;
   double mOutGain;

   wxTextCtrl *mFreqT;
   wxTextCtrl *mPhaseT;
   wxTextCtrl *mDepthT;
   wxTextCtrl *mResT;
   wxTextCtrl *mFreqOfsT;
   wxTextCtrl *mOutGainT;

   wxSlider *mFreqS;
   wxSlider *mPhaseS;
   wxSlider *mDepthS;
   wxSlider *mResS;
   wxSlider *mFreqOfsS;
   wxSlider *mOutGainS;

   DECLARE_EVENT_TABLE()
};

#endif

