/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah.cpp

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectWahwah
\brief An Effect that adds a 'spectral glide'.

*//*******************************************************************/


#include "Wahwah.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/intl.h>
#include <wx/slider.h>

#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../widgets/valnum.h"

enum
{
   ID_Freq = 10000,
   ID_Phase,
   ID_Depth,
   ID_Res,
   ID_FreqOfs,
   ID_OutGain
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key               Def      Min      Max      Scale
Param( Freq,      double,  wxT("Freq"),       1.5,     0.1,     4.0,     10  );
Param( Phase,     double,  wxT("Phase"),      0.0,     0.0,     360.0,   1   );
Param( Depth,     int,     wxT("Depth"),      70,      0,       100,     1   ); // scaled to 0-1 before processing
Param( Res,       double,  wxT("Resonance"),  2.5,     0.1,     10.0,    10  );
Param( FreqOfs,   int,     wxT("Offset"),     30,      0,       100,     1   ); // scaled to 0-1 before processing
Param( OutGain,   double,  wxT("Gain"),      -6.0,    -30.0,    30.0,    1   );

// How many samples are processed before recomputing the lfo value again
#define lfoskipsamples 30

//
// EffectWahwah
//

const ComponentInterfaceSymbol EffectWahwah::Symbol
{ XO("Wahwah") };

namespace{ BuiltinEffectsModule::Registration< EffectWahwah > reg; }

BEGIN_EVENT_TABLE(EffectWahwah, wxEvtHandler)
    EVT_SLIDER(ID_Freq, EffectWahwah::OnFreqSlider)
    EVT_SLIDER(ID_Phase, EffectWahwah::OnPhaseSlider)
    EVT_SLIDER(ID_Depth, EffectWahwah::OnDepthSlider)
    EVT_SLIDER(ID_Res, EffectWahwah::OnResonanceSlider)
    EVT_SLIDER(ID_FreqOfs, EffectWahwah::OnFreqOffSlider)
    EVT_SLIDER(ID_OutGain, EffectWahwah::OnGainSlider)
    EVT_TEXT(ID_Freq, EffectWahwah::OnFreqText)
    EVT_TEXT(ID_Phase, EffectWahwah::OnPhaseText)
    EVT_TEXT(ID_Depth, EffectWahwah::OnDepthText)
    EVT_TEXT(ID_Res, EffectWahwah::OnResonanceText)
    EVT_TEXT(ID_FreqOfs, EffectWahwah::OnFreqOffText)
    EVT_TEXT(ID_OutGain, EffectWahwah::OnGainText)
END_EVENT_TABLE();

EffectWahwah::EffectWahwah()
{
   mFreq = DEF_Freq;
   mPhase = DEF_Phase;
   mDepth = DEF_Depth;
   mRes = DEF_Res;
   mFreqOfs = DEF_FreqOfs;
   mOutGain = DEF_OutGain;

   SetLinearEffectFlag(true);
}

EffectWahwah::~EffectWahwah()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectWahwah::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectWahwah::GetDescription()
{
   return XO("Rapid tone quality variations, like that guitar sound so popular in the 1970's");
}

wxString EffectWahwah::ManualPage()
{
   return wxT("Wahwah");
}

// EffectDefinitionInterface implementation

EffectType EffectWahwah::GetType()
{
   return EffectTypeProcess;
}

bool EffectWahwah::SupportsRealtime()
{
#if defined(EXPERIMENTAL_REALTIME_AUDACITY_EFFECTS)
   return true;
#else
   return false;
#endif
}

// EffectClientInterface implementation

unsigned EffectWahwah::GetAudioInCount()
{
   return 1;
}

unsigned EffectWahwah::GetAudioOutCount()
{
   return 1;
}

bool EffectWahwah::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames chanMap)
{
   InstanceInit(mMaster, mSampleRate);

   if (chanMap[0] == ChannelNameFrontRight)
   {
      mMaster.phase += M_PI;
   }

   return true;
}

size_t EffectWahwah::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   return InstanceProcess(mMaster, inBlock, outBlock, blockLen);
}

bool EffectWahwah::RealtimeInitialize()
{
   SetBlockSize(512);

   mSlaves.clear();

   return true;
}

bool EffectWahwah::RealtimeAddProcessor(unsigned WXUNUSED(numChannels), float sampleRate)
{
   EffectWahwahState slave;

   InstanceInit(slave, sampleRate);

   mSlaves.push_back(slave);

   return true;
}

bool EffectWahwah::RealtimeFinalize()
{
   mSlaves.clear();

   return true;
}

size_t EffectWahwah::RealtimeProcess(int group,
                                          float **inbuf,
                                          float **outbuf,
                                          size_t numSamples)
{

   return InstanceProcess(mSlaves[group], inbuf, outbuf, numSamples);
}

bool EffectWahwah::DefineParams( ShuttleParams & S ){
   S.SHUTTLE_PARAM( mFreq, Freq );
   S.SHUTTLE_PARAM( mPhase, Phase );
   S.SHUTTLE_PARAM( mDepth, Depth );
   S.SHUTTLE_PARAM( mRes, Res );
   S.SHUTTLE_PARAM( mFreqOfs, FreqOfs );
   S.SHUTTLE_PARAM( mOutGain, OutGain );
   return true;
}

bool EffectWahwah::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_Freq, mFreq);
   parms.Write(KEY_Phase, mPhase);
   parms.Write(KEY_Depth, mDepth);
   parms.Write(KEY_Res, mRes);
   parms.Write(KEY_FreqOfs, mFreqOfs);
   parms.Write(KEY_OutGain, mOutGain);
   
   return true;
}

bool EffectWahwah::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyDouble(Freq);
   ReadAndVerifyDouble(Phase);
   ReadAndVerifyInt(Depth);
   ReadAndVerifyDouble(Res);
   ReadAndVerifyInt(FreqOfs);
   ReadAndVerifyDouble(OutGain);

   mFreq = Freq;
   mPhase = Phase;
   mDepth = Depth;
   mRes = Res;
   mFreqOfs = FreqOfs;
   mOutGain = OutGain;

   return true;
}

// Effect implementation

void EffectWahwah::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(2);
   
      mFreqT = S.Id(ID_Freq)
         .Validator<FloatingPointValidator<double>>(
            5, &mFreq, NumValidatorStyle::ONE_TRAILING_ZERO, MIN_Freq, MAX_Freq)
         .AddTextBox(XXO("LFO Freq&uency (Hz):"), wxT(""), 12);

      mFreqS = S.Id(ID_Freq)
         .Name(XO("LFO frequency in hertz"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_Freq * SCL_Freq, MAX_Freq * SCL_Freq, MIN_Freq * SCL_Freq);

      mPhaseT = S.Id(ID_Phase)
         .Validator<FloatingPointValidator<double>>(
            1, &mPhase, NumValidatorStyle::DEFAULT, MIN_Phase, MAX_Phase)
         .AddTextBox(XXO("LFO Sta&rt Phase (deg.):"), wxT(""), 12);

      mPhaseS = S.Id(ID_Phase)
         .Name(XO("LFO start phase in degrees"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_Phase * SCL_Phase, MAX_Phase * SCL_Phase, MIN_Phase * SCL_Phase);
      mPhaseS->SetLineSize(10);

      mDepthT = S.Id(ID_Depth)
         .Validator<IntegerValidator<int>>(
            &mDepth, NumValidatorStyle::DEFAULT, MIN_Depth, MAX_Depth)
         .AddTextBox(XXO("Dept&h (%):"), wxT(""), 12);

      mDepthS = S.Id(ID_Depth)
         .Name(XO("Depth in percent"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_Depth * SCL_Depth, MAX_Depth * SCL_Depth, MIN_Depth * SCL_Depth);

      mResT = S.Id(ID_Res)
         .Validator<FloatingPointValidator<double>>(
            1, &mRes, NumValidatorStyle::DEFAULT, MIN_Res, MAX_Res)
         .AddTextBox(XXO("Reso&nance:"), wxT(""), 12);

      mResS = S.Id(ID_Res)
         .Name(XO("Resonance"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_Res * SCL_Res, MAX_Res * SCL_Res, MIN_Res * SCL_Res);

      mFreqOfsT = S.Id(ID_FreqOfs)
         .Validator<IntegerValidator<int>>(
            &mFreqOfs, NumValidatorStyle::DEFAULT, MIN_FreqOfs, MAX_FreqOfs)
         .AddTextBox(XXO("Wah Frequency Offse&t (%):"), wxT(""), 12);

      mFreqOfsS = S.Id(ID_FreqOfs)
         .Name(XO("Wah frequency offset in percent"))
         .Style(wxSL_HORIZONTAL)
         .MinSize( { 100, -1 } )
         .AddSlider( {}, DEF_FreqOfs * SCL_FreqOfs, MAX_FreqOfs * SCL_FreqOfs, MIN_FreqOfs * SCL_FreqOfs);

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

bool EffectWahwah::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mFreqS->SetValue((int) (mFreq * SCL_Freq));
   mPhaseS->SetValue((int) (mPhase * SCL_Phase));
   mDepthS->SetValue((int) (mDepth * SCL_Depth));
   mResS->SetValue((int) (mRes * SCL_Res));
   mFreqOfsS->SetValue((int) (mFreqOfs * SCL_FreqOfs));
   mOutGainS->SetValue((int) (mOutGain * SCL_OutGain));

   return true;
}

bool EffectWahwah::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

// EffectWahwah implementation

void EffectWahwah::InstanceInit(EffectWahwahState & data, float sampleRate)
{
   data.samplerate = sampleRate;
   data.lfoskip = mFreq * 2 * M_PI / sampleRate;
   data.skipcount = 0;
   data.xn1 = 0;
   data.xn2 = 0;
   data.yn1 = 0;
   data.yn2 = 0;
   data.b0 = 0;
   data.b1 = 0;
   data.b2 = 0;
   data.a0 = 0;
   data.a1 = 0;
   data.a2 = 0;

   data.depth = mDepth / 100.0;
   data.freqofs = mFreqOfs / 100.0;
   data.phase = mPhase * M_PI / 180.0;
   data.outgain = DB_TO_LINEAR(mOutGain);
}

size_t EffectWahwah::InstanceProcess(EffectWahwahState & data, float **inBlock, float **outBlock, size_t blockLen)
{
   float *ibuf = inBlock[0];
   float *obuf = outBlock[0];
   double frequency, omega, sn, cs, alpha;
   double in, out;

   data.lfoskip = mFreq * 2 * M_PI / data.samplerate;
   data.depth = mDepth / 100.0;
   data.freqofs = mFreqOfs / 100.0;

   data.phase = mPhase * M_PI / 180.0;
   data.outgain = DB_TO_LINEAR(mOutGain);

   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      in = (double) ibuf[i];

      if ((data.skipcount++) % lfoskipsamples == 0)
      {
         frequency = (1 + cos(data.skipcount * data.lfoskip + data.phase)) / 2;
         frequency = frequency * data.depth * (1 - data.freqofs) + data.freqofs;
         frequency = exp((frequency - 1) * 6);
         omega = M_PI * frequency;
         sn = sin(omega);
         cs = cos(omega);
         alpha = sn / (2 * mRes);
         data.b0 = (1 - cs) / 2;
         data.b1 = 1 - cs;
         data.b2 = (1 - cs) / 2;
         data.a0 = 1 + alpha;
         data.a1 = -2 * cs;
         data.a2 = 1 - alpha;
      };
      out = (data.b0 * in + data.b1 * data.xn1 + data.b2 * data.xn2 - data.a1 * data.yn1 - data.a2 * data.yn2) / data.a0;
      data.xn2 = data.xn1;
      data.xn1 = in;
      data.yn2 = data.yn1;
      data.yn1 = out;
      out *= data.outgain;

      obuf[i] = (float) out;
   }

   return blockLen;
}

void EffectWahwah::OnFreqSlider(wxCommandEvent & evt)
{
   mFreq = (double) evt.GetInt() / SCL_Freq;
   mFreqT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectWahwah::OnPhaseSlider(wxCommandEvent & evt)
{
   int val = ((evt.GetInt() + 5) / 10) * 10; // round to nearest multiple of 10
   val = val > MAX_Phase * SCL_Phase ? MAX_Phase * SCL_Phase : val;
   mPhaseS->SetValue(val);
   mPhase = (double) val / SCL_Phase;
   mPhaseT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectWahwah::OnDepthSlider(wxCommandEvent & evt)
{
   mDepth = evt.GetInt() / SCL_Depth;
   mDepthT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectWahwah::OnResonanceSlider(wxCommandEvent & evt)
{
   mRes = (double) evt.GetInt() / SCL_Res;
   mResT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectWahwah::OnFreqOffSlider(wxCommandEvent & evt)
{
   mFreqOfs = evt.GetInt() / SCL_FreqOfs;
   mFreqOfsT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectWahwah::OnGainSlider(wxCommandEvent & evt)
{
   mOutGain = evt.GetInt() / SCL_OutGain;
   mOutGainT->GetValidator()->TransferToWindow();
   EnableApply(mUIParent->Validate());
}

void EffectWahwah::OnFreqText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mFreqS->SetValue((int) (mFreq * SCL_Freq));
}

void EffectWahwah::OnPhaseText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mPhaseS->SetValue((int) (mPhase * SCL_Phase));
}

void EffectWahwah::OnDepthText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mDepthS->SetValue((int) (mDepth * SCL_Depth));
}

void EffectWahwah::OnResonanceText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mResS->SetValue((int) (mRes * SCL_Res));
}

void EffectWahwah::OnFreqOffText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mFreqOfsS->SetValue((int) (mFreqOfs * SCL_FreqOfs));
}

void EffectWahwah::OnGainText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mOutGainS->SetValue((int) (mOutGain * SCL_OutGain));
}
