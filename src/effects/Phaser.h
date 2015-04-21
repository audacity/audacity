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

#include "../ShuttleGui.h"

#include "Effect.h"

#define NUM_STAGES 24

#define PHASER_PLUGIN_SYMBOL XO("Phaser")

class EffectPhaser : public Effect
{
public:
   EffectPhaser();
   virtual ~EffectPhaser();

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

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

protected:
   // EffectPhaser implementation

   void OnStagesSlider(wxCommandEvent & evt);
   void OnDryWetSlider(wxCommandEvent & evt);
   void OnFeedbackSlider(wxCommandEvent & evt);
   void OnDepthSlider(wxCommandEvent & evt);
   void OnPhaseSlider(wxCommandEvent & evt);
   void OnFreqSlider(wxCommandEvent & evt);
   void OnStagesText(wxCommandEvent & evt);
   void OnDryWetText(wxCommandEvent & evt);
   void OnFeedbackText(wxCommandEvent & evt);
   void OnDepthText(wxCommandEvent & evt);
   void OnPhaseText(wxCommandEvent & evt);
   void OnFreqText(wxCommandEvent & evt);
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
   // state variables
   sampleCount skipcount;
   double old[NUM_STAGES]; // must be as large as MAX_STAGES
   double gain;
   double fbout;
   double lfoskip;
   double phase;

   // parameters
   int mStages;
   int mDryWet;
   double mFreq;
   double mPhase;
   int mDepth;
   int mFeedback;

   wxTextCtrl *mStagesT;
   wxTextCtrl *mDryWetT;
   wxTextCtrl *mFreqT;
   wxTextCtrl *mPhaseT;
   wxTextCtrl *mDepthT;
   wxTextCtrl *mFeedbackT;

   wxSlider *mStagesS;
   wxSlider *mDryWetS;
   wxSlider *mFreqS;
   wxSlider *mPhaseS;
   wxSlider *mDepthS;
   wxSlider *mFeedbackS;

   DECLARE_EVENT_TABLE();
};

#endif
