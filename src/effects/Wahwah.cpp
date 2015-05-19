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

#include "../Audacity.h"

#include <math.h>

#include <wx/intl.h>

#include "../widgets/valnum.h"

#include "Wahwah.h"

enum
{
   ID_Freq = 10000,
   ID_Phase,
   ID_Depth,
   ID_Res,
   ID_FreqOfs
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key               Def      Min      Max      Scale
Param( Freq,      double,  XO("Freq"),       1.5,     0.1,     4.0,     10  );
Param( Phase,     double,  XO("Phase"),      0.0,     0.0,     359.0,   1   );
Param( Depth,     int,     XO("Depth"),      70,      0,       100,     1   ); // scaled to 0-1 before processing
Param( Res,       double,  XO("Resonance"),  2.5,     0.1,     10.0,    10  );
Param( FreqOfs,   int,     XO("Offset"),     30,      0,       100,     1   ); // scaled to 0-1 before processing

// How many samples are processed before recomputing the lfo value again
#define lfoskipsamples 30

//
// EffectWahwah
//

BEGIN_EVENT_TABLE(EffectWahwah, wxEvtHandler)
    EVT_SLIDER(ID_Freq, EffectWahwah::OnFreqSlider)
    EVT_SLIDER(ID_Phase, EffectWahwah::OnPhaseSlider)
    EVT_SLIDER(ID_Depth, EffectWahwah::OnDepthSlider)
    EVT_SLIDER(ID_Res, EffectWahwah::OnResonanceSlider)
    EVT_SLIDER(ID_FreqOfs, EffectWahwah::OnFreqOffSlider)
    EVT_TEXT(ID_Freq, EffectWahwah::OnFreqText)
    EVT_TEXT(ID_Phase, EffectWahwah::OnPhaseText)
    EVT_TEXT(ID_Depth, EffectWahwah::OnDepthText)
    EVT_TEXT(ID_Res, EffectWahwah::OnResonanceText)
    EVT_TEXT(ID_FreqOfs, EffectWahwah::OnFreqOffText)
END_EVENT_TABLE();

EffectWahwah::EffectWahwah()
{
   mFreq = DEF_Freq;
   mPhase = DEF_Phase;
   mDepth = DEF_Depth;
   mRes = DEF_Res;
   mFreqOfs = DEF_FreqOfs;

   SetLinearEffectFlag(true);
}

EffectWahwah::~EffectWahwah()
{
}

// IdentInterface implementation

wxString EffectWahwah::GetSymbol()
{
   return WAHWAH_PLUGIN_SYMBOL;
}

wxString EffectWahwah::GetDescription()
{
   return XO("Rapid tone quality variations, like that guitar sound so popular in the 1970's");
}

// EffectIdentInterface implementation

EffectType EffectWahwah::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

int EffectWahwah::GetAudioInCount()
{
   return 1;
}

int EffectWahwah::GetAudioOutCount()
{
   return 1;
}

bool EffectWahwah::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames chanMap)
{
   lfoskip = mFreq * 2 * M_PI / mSampleRate;
   skipcount = 0;
   xn1 = 0;
   xn2 = 0;
   yn1 = 0;
   yn2 = 0;
   b0 = 0;
   b1 = 0;
   b2 = 0;
   a0 = 0;
   a1 = 0;
   a2 = 0;

   depth = mDepth / 100.0;
   freqofs = mFreqOfs / 100.0;

   phase = mPhase * M_PI / 180.0;
   if (chanMap[0] == ChannelNameFrontRight)
   {
      phase += M_PI;
   }

   return true;
}

sampleCount EffectWahwah::ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen)
{
   float *ibuf = inBlock[0];
   float *obuf = outBlock[0];
   double frequency, omega, sn, cs, alpha;
   double in, out;

   for (int i = 0; i < blockLen; i++)
   {
      in = (double) ibuf[i];

      if ((skipcount++) % lfoskipsamples == 0)
      {
         frequency = (1 + cos(skipcount * lfoskip + phase)) / 2;
         frequency = frequency * depth * (1 - freqofs) + freqofs;
         frequency = exp((frequency - 1) * 6);
         omega = M_PI * frequency;
         sn = sin(omega);
         cs = cos(omega);
         alpha = sn / (2 * mRes);
         b0 = (1 - cs) / 2;
         b1 = 1 - cs;
         b2 = (1 - cs) / 2;
         a0 = 1 + alpha;
         a1 = -2 * cs;
         a2 = 1 - alpha;
      };
      out = (b0 * in + b1 * xn1 + b2 * xn2 - a1 * yn1 - a2 * yn2) / a0;
      xn2 = xn1;
      xn1 = in;
      yn2 = yn1;
      yn1 = out;

      obuf[i] = (float) out;
   }

   return blockLen;
}

bool EffectWahwah::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Freq, mFreq);
   parms.Write(KEY_Phase, mPhase);
   parms.Write(KEY_Depth, mDepth);
   parms.Write(KEY_Res, mRes);
   parms.Write(KEY_FreqOfs, mFreqOfs);
   
   return true;
}

bool EffectWahwah::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyDouble(Freq);
   ReadAndVerifyDouble(Phase);
   ReadAndVerifyInt(Depth);
   ReadAndVerifyDouble(Res);
   ReadAndVerifyInt(FreqOfs);

   mFreq = Freq;
   mPhase = Phase;
   mDepth = Depth;
   mRes = Res;
   mFreqOfs = FreqOfs;

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

      FloatingPointValidator<double> vldfreq(1, &mFreq);
      vldfreq.SetRange(MIN_Freq, MAX_Freq);
      mFreqT = S.Id(ID_Freq).AddTextBox(_("LFO Frequency (Hz):"), wxT(""), 12);
      mFreqT->SetValidator(vldfreq);

      S.SetStyle(wxSL_HORIZONTAL);
      mFreqS = S.Id(ID_Freq).AddSlider(wxT(""), DEF_Freq * SCL_Freq, MAX_Freq * SCL_Freq, MIN_Freq * SCL_Freq);
      mFreqS->SetName(_("LFO frequency in hertz"));
      mFreqS->SetMinSize(wxSize(100, -1));

      FloatingPointValidator<double> vldphase(1, &mPhase);
      vldphase.SetRange(MIN_Phase, MAX_Phase);
      mPhaseT = S.Id(ID_Phase).AddTextBox(_("LFO Start Phase (deg.):"), wxT(""), 12);
      mPhaseT->SetValidator(vldphase);

      S.SetStyle(wxSL_HORIZONTAL);
      mPhaseS = S.Id(ID_Phase).AddSlider(wxT(""), DEF_Phase * SCL_Phase, MAX_Phase * SCL_Phase, MIN_Phase * SCL_Phase);
      mPhaseS->SetName(_("LFO start phase in degrees"));
      mPhaseS->SetLineSize(10);
      mPhaseS->SetMinSize(wxSize(100, -1));

      IntegerValidator<int> vlddepth(&mDepth);
      vlddepth.SetRange(MIN_Depth, MAX_Depth);
      mDepthT = S.Id(ID_Depth).AddTextBox(_("Depth (%):"), wxT(""), 12);
      mDepthT->SetValidator(vlddepth);

      S.SetStyle(wxSL_HORIZONTAL);
      mDepthS = S.Id(ID_Depth).AddSlider(wxT(""), DEF_Depth * SCL_Depth, MAX_Depth * SCL_Depth, MIN_Depth * SCL_Depth);
      mDepthS->SetName(_("Depth in percent"));
      mDepthS->SetMinSize(wxSize(100, -1));

      FloatingPointValidator<double> vldres(1, &mRes);
      vldres.SetRange(MIN_Res, MAX_Res);
      mResT = S.Id(ID_Res).AddTextBox(_("Resonance:"), wxT(""), 12);
      mResT->SetValidator(vldres);

      S.SetStyle(wxSL_HORIZONTAL);
      mResS = S.Id(ID_Res).AddSlider(wxT(""), DEF_Res * SCL_Res, MAX_Res * SCL_Res, MIN_Res * SCL_Res);
      mResS->SetName(_("Resonance"));
      mResS->SetMinSize(wxSize(100, -1));

      IntegerValidator<int> vldfreqoffset(&mFreqOfs);
      vldfreqoffset.SetRange(MIN_FreqOfs, MAX_FreqOfs);
      mFreqOfsT = S.Id(ID_FreqOfs).AddTextBox(_("Wah Frequency Offset (%):"), wxT(""), 12);
      mFreqOfsT->SetValidator(vldfreqoffset);

      S.SetStyle(wxSL_HORIZONTAL);
      mFreqOfsS = S.Id(ID_FreqOfs).AddSlider(wxT(""), DEF_FreqOfs * SCL_FreqOfs, MAX_FreqOfs * SCL_FreqOfs, MIN_FreqOfs * SCL_FreqOfs);
      mFreqOfsT->SetName(_("Wah frequency offset in percent"));
      mFreqOfsT->SetMinSize(wxSize(100, -1));
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
