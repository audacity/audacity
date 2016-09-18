/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_PHASER__
#define __AUDACITY_EFFECT_PHASER__

#include <wx/event.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "Effect.h"

class ShuttleGui;

#define NUM_STAGES 24

#define PHASER_PLUGIN_SYMBOL XO("Phaser")

class EffectPhaserState
{
public:
   // state variables
   float samplerate;
   sampleCount skipcount;
   double old[NUM_STAGES]; // must be as large as MAX_STAGES
   double gain;
   double fbout;
   double outgain;
   double lfoskip;
   double phase;
   int laststages;
};

WX_DECLARE_OBJARRAY(EffectPhaserState, EffectPhaserStateArray);

class EffectPhaser final : public Effect
{
public:
   EffectPhaser();
   virtual ~EffectPhaser();

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

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

private:
   // EffectPhaser implementation

   void InstanceInit(EffectPhaserState & data, float sampleRate);
   size_t InstanceProcess(EffectPhaserState & data, float **inBlock, float **outBlock, size_t blockLen);

   void OnStagesSlider(wxCommandEvent & evt);
   void OnDryWetSlider(wxCommandEvent & evt);
   void OnFeedbackSlider(wxCommandEvent & evt);
   void OnDepthSlider(wxCommandEvent & evt);
   void OnPhaseSlider(wxCommandEvent & evt);
   void OnFreqSlider(wxCommandEvent & evt);
   void OnGainSlider(wxCommandEvent & evt);

   void OnStagesText(wxCommandEvent & evt);
   void OnDryWetText(wxCommandEvent & evt);
   void OnFeedbackText(wxCommandEvent & evt);
   void OnDepthText(wxCommandEvent & evt);
   void OnPhaseText(wxCommandEvent & evt);
   void OnFreqText(wxCommandEvent & evt);
   void OnGainText(wxCommandEvent & evt);
/*
    Phaser Parameters

 mFreq       - Phaser's LFO frequency
 mPhase      - Phaser's LFO startphase (radians), needed for stereo Phasers
 mDepth      - Phaser depth (0 - no depth, 255 - max depth)
 mStages     - Phaser stages (recomanded from 2 to 16-24, and EVEN NUMBER)
 mDryWet     - Dry/wet mix, (0 - dry, 128 - dry=wet, 255 - wet)
 mFeedback   - Phaser FeedBack (0 - no feedback, 100 = 100% Feedback,
                               -100 = -100% FeedBack)
*/

private:
   EffectPhaserState mMaster;
   EffectPhaserStateArray mSlaves;

   // parameters
   int mStages;
   int mDryWet;
   double mFreq;
   double mPhase;
   int mDepth;
   int mFeedback;
   double mOutGain;

   wxTextCtrl *mStagesT;
   wxTextCtrl *mDryWetT;
   wxTextCtrl *mFreqT;
   wxTextCtrl *mPhaseT;
   wxTextCtrl *mDepthT;
   wxTextCtrl *mFeedbackT;
   wxTextCtrl *mOutGainT;

   wxSlider *mStagesS;
   wxSlider *mDryWetS;
   wxSlider *mFreqS;
   wxSlider *mPhaseS;
   wxSlider *mDepthS;
   wxSlider *mFeedbackS;
   wxSlider *mOutGainS;

   DECLARE_EVENT_TABLE()
};

#endif
