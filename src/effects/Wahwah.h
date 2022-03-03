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

#include "Effect.h"

class wxSlider;
class wxTextCtrl;
class ShuttleGui;

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

class EffectWahwah final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectWahwah();
   virtual ~EffectWahwah();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool SupportsRealtime() const override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // EffectProcessor implementation

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
   bool RealtimeInitialize(EffectSettings &settings) override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   size_t RealtimeProcess(int group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;
   bool DefineParams( ShuttleParams & S ) override;

   // Effect implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectWahwah implementation

   void InstanceInit(EffectWahwahState & data, float sampleRate);
   size_t InstanceProcess(EffectSettings &settings, EffectWahwahState & data,
      const float *const *inBlock, float *const *outBlock, size_t blockLen);

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
   std::vector<EffectWahwahState> mSlaves;

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

