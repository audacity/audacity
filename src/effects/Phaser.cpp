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
#include "Phaser.h"

#include <math.h>

#include <wx/intl.h>

#include "../ShuttleGui.h"
#include "../widgets/valnum.h"

#include "../Experimental.h"

enum
{
   ID_Stages = 10000,
   ID_DryWet,
   ID_Freq,
   ID_Phase,
   ID_Depth,
   ID_Feedback,
   ID_OutGain
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key               Def   Min   Max         Scale
Param( Stages,    int,     XO("Stages"),     2,    2,    NUM_STAGES, 1  );
Param( DryWet,    int,     XO("DryWet"),     128,  0,    255,        1  );
Param( Freq,      double,  XO("Freq"),       0.4,  0.001,4.0,        10.0 );
Param( Phase,     double,  XO("Phase"),      0.0,  0.0,  360.0,      1  );
Param( Depth,     int,     XO("Depth"),      100,  0,    255,        1  );
Param( Feedback,  int,     XO("Feedback"),   0,    -100, 100,        1  );
Param( OutGain,   double,  XO("Gain"),      -6.0,    -30.0,    30.0,    1   );

//
#define phaserlfoshape 4.0

// How many samples are processed before recomputing the lfo value again
#define lfoskipsamples 20

#include <wx/arrimpl.cpp>
WX_DEFINE_OBJARRAY(EffectPhaserStateArray);

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
    EVT_SLIDER(ID_OutGain, EffectPhaser::OnGainSlider)
    EVT_TEXT(ID_Stages, EffectPhaser::OnStagesText)
    EVT_TEXT(ID_DryWet, EffectPhaser::OnDryWetText)
    EVT_TEXT(ID_Freq, EffectPhaser::OnFreqText)
    EVT_TEXT(ID_Phase, EffectPhaser::OnPhaseText)
    EVT_TEXT(ID_Depth, EffectPhaser::OnDepthText)
    EVT_TEXT(ID_Feedback, EffectPhaser::OnFeedbackText)
    EVT_TEXT(ID_OutGain, EffectPhaser::OnGainText)
END_EVENT_TABLE()

EffectPhaser::EffectPhaser()
{
   mStages = DEF_Stages;
   mDryWet = DEF_DryWet;
   mFreq = DEF_Freq;
   mPhase = DEF_Phase;
   mDepth = DEF_Depth;
   mFeedback = DEF_Feedback;
   mOutGain = DEF_OutGain;

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

bool EffectPhaser::SupportsRealtime()
{
#if defined(EXPERIMENTAL_REALTIME_AUDACITY_EFFECTS)
   return true;
#else
   return false;
#endif
}

// EffectClientInterface implementation

unsigned EffectPhaser::GetAudioInCount()
{
   return 1;
}

unsigned EffectPhaser::GetAudioOutCount()
{
   return 1;
}

bool EffectPhaser::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames chanMap)
{
   InstanceInit(mMaster, mSampleRate);
   if (chanMap[0] == ChannelNameFrontRight)
   {
      mMaster.phase += M_PI;
   }

   return true;
}

size_t EffectPhaser::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   return InstanceProcess(mMaster, inBlock, outBlock, blockLen);
}

bool EffectPhaser::RealtimeInitialize()
{
   SetBlockSize(512);

   mSlaves.Clear();

   return true;
}

bool EffectPhaser::RealtimeAddProcessor(unsigned WXUNUSED(numChannels), float sampleRate)
{
   EffectPhaserState slave;

   InstanceInit(slave, sampleRate);

   mSlaves.Add(slave);

   return true;
}

bool EffectPhaser::RealtimeFinalize()
{
   mSlaves.Clear();

   return true;
}

size_t EffectPhaser::RealtimeProcess(int group,
                                          float **inbuf,
                                          float **outbuf,
                                          size_t numSamples)
{

   return InstanceProcess(mSlaves[group], inbuf, outbuf, numSamples);
}

bool EffectPhaser::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Stages, mStages);
   parms.Write(KEY_DryWet, mDryWet);
   parms.Write(KEY_Freq, mFreq);
   parms.Write(KEY_Phase, mPhase);
   parms.Write(KEY_Depth, mDepth);
   parms.Write(KEY_Feedback, mFeedback);
   parms.Write(KEY_OutGain, mOutGain);

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
   ReadAndVerifyDouble(OutGain);

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
   mOutGain = OutGain;

   return true;
}

// Effect implementation

void EffectPhaser::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(2);

      IntegerValidator<int> vldStages(&mStages);
      vldStages.SetRange(MIN_Stages, MAX_Stages);
      mStagesT = S.Id(ID_Stages).AddTextBox(_("Stages:"), wxT(""), 15);
      mStagesT->SetValidator(vldStages);

      S.SetStyle(wxSL_HORIZONTAL);
      mStagesS = S.Id(ID_Stages).AddSlider(wxT(""), DEF_Stages * SCL_Stages, MAX_Stages * SCL_Stages, MIN_Stages * SCL_Stages);
      mStagesS->SetName(_("Stages"));
      mStagesS->SetLineSize(2);
      mStagesS->SetMinSize(wxSize(100, -1));

      IntegerValidator<int> vldDryWet(&mDryWet);
      vldDryWet.SetRange(MIN_DryWet, MAX_DryWet);
      mDryWetT = S.Id(ID_DryWet).AddTextBox(_("Dry/Wet:"), wxT(""), 15);
      mDryWetT->SetValidator(vldDryWet);

      S.SetStyle(wxSL_HORIZONTAL);
      mDryWetS = S.Id(ID_DryWet).AddSlider(wxT(""), DEF_DryWet * SCL_DryWet, MAX_DryWet * SCL_DryWet, MIN_DryWet * SCL_DryWet);
      mDryWetS->SetName(_("Dry Wet"));
      mDryWetS->SetMinSize(wxSize(100, -1));

      FloatingPointValidator<double> vldFreq(5, &mFreq, NUM_VAL_ONE_TRAILING_ZERO);
      vldFreq.SetRange(MIN_Freq, MAX_Freq);
      mFreqT = S.Id(ID_Freq).AddTextBox(_("LFO Frequency (Hz):"), wxT(""), 15);
      mFreqT->SetValidator(vldFreq);

      S.SetStyle(wxSL_HORIZONTAL);
      mFreqS = S.Id(ID_Freq).AddSlider(wxT(""), DEF_Freq * SCL_Freq, MAX_Freq * SCL_Freq, 0.0);
      mFreqS ->SetName(_("LFO frequency in hertz"));
      mFreqS ->SetMinSize(wxSize(100, -1));

      FloatingPointValidator<double> vldPhase(1, &mPhase);
      vldPhase.SetRange(MIN_Phase, MAX_Phase);
      mPhaseT = S.Id(ID_Phase).AddTextBox(_("LFO Start Phase (deg.):"), wxT(""), 15);
      mPhaseT->SetValidator(vldPhase);

      S.SetStyle(wxSL_HORIZONTAL);
      mPhaseS = S.Id(ID_Phase).AddSlider(wxT(""), DEF_Phase * SCL_Phase, MAX_Phase * SCL_Phase, MIN_Phase * SCL_Phase);
      mPhaseS->SetName(_("LFO start phase in degrees"));
      mPhaseS->SetLineSize(10);
      mPhaseS->SetMinSize(wxSize(100, -1));

      IntegerValidator<int> vldDepth(&mDepth);
      vldDepth.SetRange(MIN_Depth, MAX_Depth);
      mDepthT = S.Id(ID_Depth).AddTextBox(_("Depth:"), wxT(""), 15);
      mDepthT->SetValidator(vldDepth);

      S.SetStyle(wxSL_HORIZONTAL);
      mDepthS = S.Id(ID_Depth).AddSlider(wxT(""), DEF_Depth * SCL_Depth, MAX_Depth * SCL_Depth, MIN_Depth * SCL_Depth);
      mDepthS->SetName(_("Depth in percent"));
      mDepthS->SetMinSize(wxSize(100, -1));

      IntegerValidator<int> vldFeedback(&mFeedback);
      vldFeedback.SetRange(MIN_Feedback, MAX_Feedback);
      mFeedbackT = S.Id(ID_Feedback).AddTextBox(_("Feedback (%):"), wxT(""), 15);
      mFeedbackT->SetValidator(vldFeedback);

      S.SetStyle(wxSL_HORIZONTAL);
      mFeedbackS = S.Id(ID_Feedback).AddSlider(wxT(""), DEF_Feedback * SCL_Feedback, MAX_Feedback * SCL_Feedback, MIN_Feedback * SCL_Feedback);
      mFeedbackS->SetName(_("Feedback in percent"));
      mFeedbackS->SetLineSize(10);
      mFeedbackS->SetMinSize(wxSize(100, -1));

      FloatingPointValidator<double> vldoutgain(1, &mOutGain);
      vldoutgain.SetRange(MIN_OutGain, MAX_OutGain);
      mOutGainT = S.Id(ID_OutGain).AddTextBox(_("Output gain (dB):"), wxT(""), 12);
      mOutGainT->SetValidator(vldoutgain);

      S.SetStyle(wxSL_HORIZONTAL);
      mOutGainS = S.Id(ID_OutGain).AddSlider(wxT(""), DEF_OutGain * SCL_OutGain, MAX_OutGain * SCL_OutGain, MIN_OutGain * SCL_OutGain);
      mOutGainS->SetName(_("Output gain (dB)"));
      mOutGainS->SetMinSize(wxSize(100, -1));
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
   mOutGainS->SetValue((int) (mOutGain * SCL_OutGain));

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

void EffectPhaser::InstanceInit(EffectPhaserState & data, float sampleRate)
{
   data.samplerate = sampleRate;

   for (int j = 0; j < mStages; j++)
   {
      data.old[j] = 0;
   }

   data.skipcount = 0;
   data.gain = 0;
   data.fbout = 0;
   data.laststages = 0;
   data.outgain = 0;

   return;
}

size_t EffectPhaser::InstanceProcess(EffectPhaserState & data, float **inBlock, float **outBlock, size_t blockLen)
{
   float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   for (int j = data.laststages; j < mStages; j++)
   {
      data.old[j] = 0;
   }
   data.laststages = mStages;

   data.lfoskip = mFreq * 2 * M_PI / data.samplerate;
   data.phase = mPhase * M_PI / 180;
   data.outgain = DB_TO_LINEAR(mOutGain);

   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      double in = ibuf[i];

      double m = in + data.fbout * mFeedback / 101;  // Feedback must be less than 100% to avoid infinite gain.

      if (((data.skipcount++) % lfoskipsamples) == 0)
      {
         //compute sine between 0 and 1
         data.gain =
            (1.0 +
             cos(data.skipcount.as_double() * data.lfoskip
                 + data.phase)) / 2.0;

         // change lfo shape
         data.gain = expm1(data.gain * phaserlfoshape) / expm1(phaserlfoshape);

         // attenuate the lfo
         data.gain = 1.0 - data.gain / 255.0 * mDepth;
      }

      // phasing routine
      for (int j = 0; j < mStages; j++)
      {
         double tmp = data.old[j];
         data.old[j] = data.gain * tmp + m;
         m = tmp - data.gain * data.old[j];
      }
      data.fbout = m;

      obuf[i] = (float) (data.outgain * (m * mDryWet + in * (255 - mDryWet)) / 255);
   }

   return blockLen;
}

void EffectPhaser::OnStagesSlider(wxCommandEvent & evt)
{
   mStages = (evt.GetInt() / SCL_Stages) & ~1;  // must be even;
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
   if (mFreq < MIN_Freq) mFreq = MIN_Freq;
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

void EffectPhaser::OnGainSlider(wxCommandEvent & evt)
{
   mOutGain = evt.GetInt() / SCL_OutGain;
   mOutGainT->GetValidator()->TransferToWindow();
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

void EffectPhaser::OnGainText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mOutGainS->SetValue((int) (mOutGain * SCL_OutGain));
}