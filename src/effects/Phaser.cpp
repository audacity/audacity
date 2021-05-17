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



#include "Phaser.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/intl.h>
#include <wx/slider.h>

#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../widgets/valnum.h"

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
Param( Stages,    int,     wxT("Stages"),     2,    2,    NUM_STAGES, 1  );
Param( DryWet,    int,     wxT("DryWet"),     128,  0,    255,        1  );
Param( Freq,      double,  wxT("Freq"),       0.4,  0.001,4.0,        10.0 );
Param( Phase,     double,  wxT("Phase"),      0.0,  0.0,  360.0,      1  );
Param( Depth,     int,     wxT("Depth"),      100,  0,    255,        1  );
Param( Feedback,  int,     wxT("Feedback"),   0,    -100, 100,        1  );
Param( OutGain,   double,  wxT("Gain"),      -6.0,    -30.0,    30.0,    1   );

//
#define phaserlfoshape 4.0

// How many samples are processed before recomputing the lfo value again
#define lfoskipsamples 20

//
// EffectPhaser
//

const ComponentInterfaceSymbol EffectPhaser::Symbol
{ XO("Phaser") };

namespace{ BuiltinEffectsModule::Registration< EffectPhaser > reg; }

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

// ComponentInterface implementation

ComponentInterfaceSymbol EffectPhaser::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectPhaser::GetDescription()
{
   return XO("Combines phase-shifted signals with the original signal");
}

wxString EffectPhaser::ManualPage()
{
   return wxT("Phaser");
}

// EffectDefinitionInterface implementation

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

   mSlaves.clear();

   return true;
}

bool EffectPhaser::RealtimeAddProcessor(unsigned WXUNUSED(numChannels), float sampleRate)
{
   EffectPhaserState slave;

   InstanceInit(slave, sampleRate);

   mSlaves.push_back(slave);

   return true;
}

bool EffectPhaser::RealtimeFinalize()
{
   mSlaves.clear();

   return true;
}

size_t EffectPhaser::RealtimeProcess(int group,
                                          float **inbuf,
                                          float **outbuf,
                                          size_t numSamples)
{

   return InstanceProcess(mSlaves[group], inbuf, outbuf, numSamples);
}
bool EffectPhaser::DefineParams( ShuttleParams & S ){
   S.SHUTTLE_PARAM( mStages,    Stages );
   S.SHUTTLE_PARAM( mDryWet,    DryWet );
   S.SHUTTLE_PARAM( mFreq,      Freq );
   S.SHUTTLE_PARAM( mPhase,     Phase );
   S.SHUTTLE_PARAM( mDepth,     Depth );
   S.SHUTTLE_PARAM( mFeedback,  Feedback );
   S.SHUTTLE_PARAM( mOutGain,   OutGain );
   return true;
}

bool EffectPhaser::GetAutomationParameters(CommandParameters & parms)
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

bool EffectPhaser::SetAutomationParameters(CommandParameters & parms)
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

      mStagesT = S.Id(ID_Stages)
         .Validator<IntegerValidator<int>>(
            &mStages, NumValidatorStyle::DEFAULT, MIN_Stages, MAX_Stages)
         .AddTextBox(XXO("&Stages:"), wxT(""), 15);

      mStagesS = S.Id(ID_Stages)
         .Name(XO("Stages"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_Stages * SCL_Stages, MAX_Stages * SCL_Stages, MIN_Stages * SCL_Stages);
      mStagesS->SetLineSize(2);

      mDryWetT = S.Id(ID_DryWet)
         .Validator<IntegerValidator<int>>(
            &mDryWet, NumValidatorStyle::DEFAULT, MIN_DryWet, MAX_DryWet)
         .AddTextBox(XXO("&Dry/Wet:"), wxT(""), 15);

      mDryWetS = S.Id(ID_DryWet)
         .Name(XO("Dry Wet"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_DryWet * SCL_DryWet, MAX_DryWet * SCL_DryWet, MIN_DryWet * SCL_DryWet);

      mFreqT = S.Id(ID_Freq)
         .Validator<FloatingPointValidator<double>>(
            5, &mFreq, NumValidatorStyle::ONE_TRAILING_ZERO, MIN_Freq, MAX_Freq)
         .AddTextBox(XXO("LFO Freq&uency (Hz):"), wxT(""), 15);

      mFreqS = S.Id(ID_Freq)
         .Name(XO("LFO frequency in hertz"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_Freq * SCL_Freq, MAX_Freq * SCL_Freq, 0.0);

      mPhaseT = S.Id(ID_Phase)
         .Validator<FloatingPointValidator<double>>(
            1, &mPhase, NumValidatorStyle::DEFAULT, MIN_Phase, MAX_Phase)
         .AddTextBox(XXO("LFO Sta&rt Phase (deg.):"), wxT(""), 15);

      mPhaseS = S.Id(ID_Phase)
         .Name(XO("LFO start phase in degrees"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_Phase * SCL_Phase, MAX_Phase * SCL_Phase, MIN_Phase * SCL_Phase);
      mPhaseS->SetLineSize(10);

      mDepthT = S.Id(ID_Depth)
         .Validator<IntegerValidator<int>>(
            &mDepth, NumValidatorStyle::DEFAULT, MIN_Depth, MAX_Depth)
         .AddTextBox(XXO("Dept&h:"), wxT(""), 15);

      mDepthS = S.Id(ID_Depth)
         .Name(XO("Depth in percent"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_Depth * SCL_Depth, MAX_Depth * SCL_Depth, MIN_Depth * SCL_Depth);

      mFeedbackT = S.Id(ID_Feedback)
         .Validator<IntegerValidator<int>>(
            &mFeedback, NumValidatorStyle::DEFAULT, MIN_Feedback, MAX_Feedback)
         .AddTextBox(XXO("Feedbac&k (%):"), wxT(""), 15);

      mFeedbackS = S.Id(ID_Feedback)
         .Name(XO("Feedback in percent"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_Feedback * SCL_Feedback, MAX_Feedback * SCL_Feedback, MIN_Feedback * SCL_Feedback);
      mFeedbackS->SetLineSize(10);

      mOutGainT = S.Id(ID_OutGain)
         .Validator<FloatingPointValidator<double>>(
            1, &mOutGain, NumValidatorStyle::DEFAULT, MIN_OutGain, MAX_OutGain)
         .AddTextBox(XXO("&Output gain (dB):"), wxT(""), 12);

      mOutGainS = S.Id(ID_OutGain)
         .Name(XO("Output gain (dB)"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_OutGain * SCL_OutGain, MAX_OutGain * SCL_OutGain, MIN_OutGain * SCL_OutGain);
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
