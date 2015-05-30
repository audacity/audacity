/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser.cpp

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectPhaser
\brief An Effect that changes frequencies in a time varying manner.

*//*******************************************************************/


#include "../Audacity.h"

#include <math.h>

#include <wx/intl.h>

#include "../widgets/valnum.h"

#include "Phaser.h"

enum
{
   ID_Stages = 10000,
   ID_DryWet,
   ID_Freq,
   ID_Phase,
   ID_Depth,
   ID_Feedback
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key               Def   Min   Max         Scale
Param( Stages,    int,     XO("Stages"),     2,    2,    NUM_STAGES, 1  );
Param( DryWet,    int,     XO("DryWet"),     128,  0,    255,        1  );
Param( Freq,      double,  XO("Freq"),       0.4,  0.1,  4.0,        10 );
Param( Phase,     double,  XO("Phase"),      0.0,  0.0,  359.0,      1  );
Param( Depth,     int,     XO("Depth"),      100,  0,    255,        1  );
Param( Feedback,  int,     XO("Feedback"),   0,    -100, 100,        1  );

//
#define phaserlfoshape 4.0

// How many samples are processed before recomputing the lfo value again
#define lfoskipsamples 20

//
// EffectPhaser
//

BEGIN_EVENT_TABLE(EffectPhaser, wxEvtHandler)
    EVT_SLIDER(ID_Stages, EffectPhaser::OnStagesSlider)
    EVT_SLIDER(ID_DryWet, EffectPhaser::OnDryWetSlider)
    EVT_SLIDER(ID_Freq, EffectPhaser::OnFreqSlider)
    EVT_SLIDER(ID_Phase, EffectPhaser::OnPhaseSlider)
    EVT_SLIDER(ID_Depth, EffectPhaser::OnDepthSlider)
    EVT_SLIDER(ID_Feedback, EffectPhaser::OnFeedbackSlider)
    EVT_TEXT(ID_Stages, EffectPhaser::OnStagesText)
    EVT_TEXT(ID_DryWet, EffectPhaser::OnDryWetText)
    EVT_TEXT(ID_Freq, EffectPhaser::OnFreqText)
    EVT_TEXT(ID_Phase, EffectPhaser::OnPhaseText)
    EVT_TEXT(ID_Depth, EffectPhaser::OnDepthText)
    EVT_TEXT(ID_Feedback, EffectPhaser::OnFeedbackText)
END_EVENT_TABLE()

EffectPhaser::EffectPhaser()
{
   mStages = DEF_Stages;
   mDryWet = DEF_DryWet;
   mFreq = DEF_Freq;
   mPhase = DEF_Phase;
   mDepth = DEF_Depth;
   mFeedback = DEF_Feedback;

   SetLinearEffectFlag(true);
}

EffectPhaser::~EffectPhaser()
{
}

// IdentInterface implementation

wxString EffectPhaser::GetSymbol()
{
   return PHASER_PLUGIN_SYMBOL;
}

wxString EffectPhaser::GetDescription()
{
   return XO("Combines phase-shifted signals with the original signal");
}

// EffectIdentInterface implementation

EffectType EffectPhaser::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

int EffectPhaser::GetAudioInCount()
{
   return 1;
}

int EffectPhaser::GetAudioOutCount()
{
   return 1;
}

bool EffectPhaser::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames chanMap)
{
   for (int j = 0; j < mStages; j++)
   {
      old[j] = 0;
   }

   skipcount = 0;
   gain = 0;
   fbout = 0;
   lfoskip = mFreq * 2 * M_PI / mSampleRate;

   phase = mPhase * M_PI / 180;
   if (chanMap[0] == ChannelNameFrontRight)
   {
      phase += M_PI;
   }

   return true;
}

sampleCount EffectPhaser::ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen)
{
   float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   for (sampleCount i = 0; i < blockLen; i++)
   {
      double in = ibuf[i];

      double m = in + fbout * mFeedback / 100;

      if (((skipcount++) % lfoskipsamples) == 0)
      {
         //compute sine between 0 and 1
         gain = (1.0 + cos(skipcount * lfoskip + phase)) / 2.0;

         // change lfo shape
         gain = expm1(gain * phaserlfoshape) / expm1(phaserlfoshape);

         // attenuate the lfo
         gain = 1.0 - gain / 255.0 * mDepth;
      }

      // phasing routine
      for (int j = 0; j < mStages; j++)
      {
         double tmp = old[j];
         old[j] = gain * tmp + m;
         m = tmp - gain * old[j];
      }
      fbout = m;

      obuf[i] = (float) ((m * mDryWet + in * (255 - mDryWet)) / 255);
   }

   return blockLen;
}

bool EffectPhaser::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Stages, mStages);
   parms.Write(KEY_DryWet, mDryWet);
   parms.Write(KEY_Freq, mFreq);
   parms.Write(KEY_Phase, mPhase);
   parms.Write(KEY_Depth, mDepth);
   parms.Write(KEY_Feedback, mFeedback);

   return true;
}

bool EffectPhaser::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyInt(Stages);
   ReadAndVerifyInt(DryWet);
   ReadAndVerifyDouble(Freq);
   ReadAndVerifyDouble(Phase);
   ReadAndVerifyInt(Depth);
   ReadAndVerifyInt(Feedback);

   if (Stages & 1)    // must be even, but don't complain about it
   {
      Stages &= ~1;
   }

   mFreq = Freq;
   mFeedback = Feedback;
   mStages = Stages;
   mDryWet = DryWet;
   mDepth = Depth;
   mPhase = Phase;

   return true;
}

// Effect implementation

void EffectPhaser::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(2);

      IntegerValidator<int> vldStages(&mStages);
      vldStages.SetRange(MIN_Stages, MAX_Stages);
      mStagesT = S.Id(ID_Stages).AddTextBox(_("Stages:"), wxT(""), 12);
      mStagesT->SetValidator(vldStages);

      S.SetStyle(wxSL_HORIZONTAL);
      mStagesS = S.Id(ID_Stages).AddSlider(wxT(""), DEF_Stages * SCL_Stages, MAX_Stages * SCL_Stages, MIN_Stages * SCL_Stages);
      mStagesS->SetName(_("Stages"));
      mStagesS->SetLineSize(2);
      mStagesS->SetMinSize(wxSize(100, -1));

      IntegerValidator<int> vldDryWet(&mDryWet);
      vldDryWet.SetRange(MIN_DryWet, MAX_DryWet);
      mDryWetT = S.Id(ID_DryWet).AddTextBox(_("Dry/Wet:"), wxT(""), 12);
      mDryWetT->SetValidator(vldDryWet);

      S.SetStyle(wxSL_HORIZONTAL);
      mDryWetS = S.Id(ID_DryWet).AddSlider(wxT(""), DEF_DryWet * SCL_DryWet, MAX_DryWet * SCL_DryWet, MIN_DryWet * SCL_DryWet);
      mDryWetS->SetName(_("Dry Wet"));
      mDryWetS->SetMinSize(wxSize(100, -1));

      FloatingPointValidator<double> vldFreq(1, &mFreq);
      vldFreq.SetRange(MIN_Freq, MAX_Freq);
      mFreqT = S.Id(ID_Freq).AddTextBox(_("LFO Frequency (Hz):"), wxT(""), 12);
      mFreqT->SetValidator(vldFreq);

      S.SetStyle(wxSL_HORIZONTAL);
      mFreqS = S.Id(ID_Freq).AddSlider(wxT(""), DEF_Freq * SCL_Freq, MAX_Freq * SCL_Freq, MIN_Freq * SCL_Freq);
      mFreqS ->SetName(_("LFO frequency in hertz"));
      mFreqS ->SetMinSize(wxSize(100, -1));

      FloatingPointValidator<double> vldPhase(1, &mPhase);
      vldPhase.SetRange(MIN_Phase, MAX_Phase);
      mPhaseT = S.Id(ID_Phase).AddTextBox(_("LFO Start Phase (deg.):"), wxT(""), 12);
      mPhaseT->SetValidator(vldPhase);

      S.SetStyle(wxSL_HORIZONTAL);
      mPhaseS = S.Id(ID_Phase).AddSlider(wxT(""), DEF_Phase * SCL_Phase, MAX_Phase * SCL_Phase, MIN_Phase * SCL_Phase);
      mPhaseS->SetName(_("LFO start phase in degrees"));
      mPhaseS->SetLineSize(10);
      mPhaseS->SetMinSize(wxSize(100, -1));

      IntegerValidator<int> vldDepth(&mDepth);
      vldDepth.SetRange(MIN_Depth, MAX_Depth);
      mDepthT = S.Id(ID_Depth).AddTextBox(_("Depth:"), wxT(""), 12);
      mDepthT->SetValidator(vldDepth);

      S.SetStyle(wxSL_HORIZONTAL);
      mDepthS = S.Id(ID_Depth).AddSlider(wxT(""), DEF_Depth * SCL_Depth, MAX_Depth * SCL_Depth, MIN_Depth * SCL_Depth);
      mDepthS->SetName(_("Depth in percent"));
      mDepthS->SetMinSize(wxSize(100, -1));

      IntegerValidator<int> vldFeedback(&mFeedback);
      vldFeedback.SetRange(MIN_Feedback, MAX_Feedback);
      mFeedbackT = S.Id(ID_Feedback).AddTextBox(_("Feedback (%):"), wxT(""), 12);
      mFeedbackT->SetValidator(vldFeedback);

      S.SetStyle(wxSL_HORIZONTAL);
      mFeedbackS = S.Id(ID_Feedback).AddSlider(wxT(""), DEF_Feedback * SCL_Feedback, MAX_Feedback * SCL_Feedback, MIN_Feedback * SCL_Feedback);
      mFeedbackS->SetName(_("Feedback in percent"));
      mFeedbackS->SetLineSize(10);
      mFeedbackS->SetMinSize(wxSize(100, -1));
   }
   S.EndMultiColumn();
}

bool EffectPhaser::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mStagesS->SetValue((int) (mStages * SCL_Stages));
   mDryWetS->SetValue((int) (mDryWet * SCL_DryWet));
   mFreqS->SetValue((int) (mFreq * SCL_Freq));
   mPhaseS->SetValue((int) (mPhase * SCL_Phase));
   mDepthS->SetValue((int) (mDepth * SCL_Depth));
   mFeedbackS->SetValue((int) (mFeedback * SCL_Feedback));

   return true;
}

bool EffectPhaser::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   if (mStages & 1)    // must be even
   {
      mStages &= ~1;
      mStagesT->GetValidator()->TransferToWindow();
   }

   return true;
}

// EffectPhaser implementation

void EffectPhaser::OnStagesSlider(wxCommandEvent & evt)
{
   mStages = (evt.GetInt() / SCL_Stages) & ~1;  // must be even;
   mPhaseS->SetValue(mStages * SCL_Stages);
   mStagesT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectPhaser::OnDryWetSlider(wxCommandEvent & evt)
{
   mDryWet = evt.GetInt() / SCL_DryWet;
   mDryWetT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectPhaser::OnFreqSlider(wxCommandEvent & evt)
{
   mFreq = (double) evt.GetInt() / SCL_Freq;
   mFreqT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectPhaser::OnPhaseSlider(wxCommandEvent & evt)
{
   int val = ((evt.GetInt() + 5) / 10) * 10; // round to nearest multiple of 10
   val = val > MAX_Phase * SCL_Phase ? MAX_Phase * SCL_Phase : val;
   mPhaseS->SetValue(val);
   mPhase =  (double) val / SCL_Phase;
   mPhaseT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectPhaser::OnDepthSlider(wxCommandEvent & evt)
{
   mDepth = evt.GetInt() / SCL_Depth;
   mDepthT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectPhaser::OnFeedbackSlider(wxCommandEvent & evt)
{
   int val = evt.GetInt();
   val = ((val + (val > 0 ? 5 : -5)) / 10) * 10; // round to nearest multiple of 10
   val = val > MAX_Feedback * SCL_Feedback ? MAX_Feedback * SCL_Feedback : val;
   mFeedbackS->SetValue(val);
   mFeedback = val / SCL_Feedback;
   mFeedbackT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectPhaser::OnStagesText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mStagesS->SetValue((int) (mStages * SCL_Stages));
}

void EffectPhaser::OnDryWetText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mDryWetS->SetValue((int) (mDryWet * SCL_DryWet));
}

void EffectPhaser::OnFreqText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mFreqS->SetValue((int) (mFreq * SCL_Freq));
}

void EffectPhaser::OnPhaseText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mPhaseS->SetValue((int) (mPhase * SCL_Phase));
}

void EffectPhaser::OnDepthText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mDepthS->SetValue((int) (mDepth * SCL_Depth));
}

void EffectPhaser::OnFeedbackText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mFeedbackS->SetValue((int) (mFeedback * SCL_Feedback));
}
