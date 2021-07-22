/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2016 Audacity Team.
   License: GPL v2.  See License.txt.

   BassTreble.cpp
   Steve Daulton

******************************************************************//**

\class EffectBassTreble
\brief A high shelf and low shelf filter.

*//*******************************************************************/


#include "BassTreble.h"
#include "LoadEffects.h"

#include <math.h>
#include <algorithm>

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/intl.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/slider.h>

#include "Prefs.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"

enum 
{
   ID_Bass = 10000,
   ID_Treble,
   ID_Gain,
   ID_Link
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key                  Def      Min      Max      Scale
Param( Bass,      double,  wxT("Bass"),          0.0,     -30.0,   30.0,    1  );
Param( Treble,    double,  wxT("Treble"),        0.0,     -30.0,   30.0,    1  );
Param( Gain,      double,  wxT("Gain"),          0.0,     -30.0,   30.0,    1  );
Param( Link,      bool,    wxT("Link Sliders"),  false,    false,  true,    1  );

// Used to communicate the type of the filter.
enum kShelfType
{
   kBass,
   kTreble
};

const ComponentInterfaceSymbol EffectBassTreble::Symbol
{ XO("Bass and Treble") };

namespace{ BuiltinEffectsModule::Registration< EffectBassTreble > reg; }

BEGIN_EVENT_TABLE(EffectBassTreble, wxEvtHandler)
   EVT_SLIDER(ID_Bass,     EffectBassTreble::OnBassSlider)
   EVT_SLIDER(ID_Treble,   EffectBassTreble::OnTrebleSlider)
   EVT_SLIDER(ID_Gain,     EffectBassTreble::OnGainSlider)
   EVT_TEXT(ID_Bass,       EffectBassTreble::OnBassText)
   EVT_TEXT(ID_Treble,     EffectBassTreble::OnTrebleText)
   EVT_TEXT(ID_Gain,       EffectBassTreble::OnGainText)
   EVT_CHECKBOX(ID_Link,   EffectBassTreble::OnLinkCheckbox)
END_EVENT_TABLE()

EffectBassTreble::EffectBassTreble()
{
   mBass = DEF_Bass;
   mTreble = DEF_Treble;
   mGain = DEF_Gain;
   mLink = DEF_Link;

   SetLinearEffectFlag(true);
}

EffectBassTreble::~EffectBassTreble()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectBassTreble::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectBassTreble::GetDescription()
{
   return XO("Simple tone control effect");
}

ManualPageID EffectBassTreble::ManualPage()
{
   return L"Bass_and_Treble";
}

// EffectDefinitionInterface implementation

EffectType EffectBassTreble::GetType()
{
   return EffectTypeProcess;
}

bool EffectBassTreble::SupportsRealtime()
{
#if defined(EXPERIMENTAL_REALTIME_AUDACITY_EFFECTS)
   return true;
#else
   return false;
#endif
}


// EffectClientInterface implementation

unsigned EffectBassTreble::GetAudioInCount()
{
   return 1;
}

unsigned EffectBassTreble::GetAudioOutCount()
{
   return 1;
}

bool EffectBassTreble::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames WXUNUSED(chanMap))
{
   InstanceInit(mMaster, mSampleRate);

   return true;
}

size_t EffectBassTreble::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   return InstanceProcess(mMaster, inBlock, outBlock, blockLen);
}

bool EffectBassTreble::RealtimeInitialize()
{
   SetBlockSize(512);

   mSlaves.clear();

   return true;
}

bool EffectBassTreble::RealtimeAddProcessor(unsigned WXUNUSED(numChannels), float sampleRate)
{
   EffectBassTrebleState slave;

   InstanceInit(slave, sampleRate);

   mSlaves.push_back(slave);

   return true;
}

bool EffectBassTreble::RealtimeFinalize()
{
   mSlaves.clear();

   return true;
}

size_t EffectBassTreble::RealtimeProcess(int group,
                                              float **inbuf,
                                              float **outbuf,
                                              size_t numSamples)
{
   return InstanceProcess(mSlaves[group], inbuf, outbuf, numSamples);
}
bool EffectBassTreble::DefineParams( ShuttleParams & S ){
   S.SHUTTLE_PARAM( mBass, Bass );
   S.SHUTTLE_PARAM( mTreble, Treble );
   S.SHUTTLE_PARAM( mGain, Gain );
   S.SHUTTLE_PARAM( mLink, Link );
   return true;
}

bool EffectBassTreble::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_Bass, mBass);
   parms.Write(KEY_Treble, mTreble);
   parms.Write(KEY_Gain, mGain);
   parms.Write(KEY_Link, mLink);

   return true;
}

bool EffectBassTreble::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyDouble(Bass);
   ReadAndVerifyDouble(Treble);
   ReadAndVerifyDouble(Gain);
   ReadAndVerifyBool(Link);

   mBass = Bass;
   mTreble = Treble;
   mGain = Gain;
   mLink = Link;

   return true;
}

bool EffectBassTreble::CheckWhetherSkipEffect()
{
   return (mBass == 0.0 && mTreble == 0.0 && mGain == 0.0);
}


// Effect implementation

void EffectBassTreble::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.AddSpace(0, 5);

   S.StartStatic(XO("Tone controls"));
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(2);

         // Bass control
         mBassT = S.Id(ID_Bass)
            .Name(XO("Bass (dB):"))
            .Validator<FloatingPointValidator<double>>(
               1, &mBass, NumValidatorStyle::DEFAULT, MIN_Bass, MAX_Bass)
            .AddTextBox(XXO("Ba&ss (dB):"), wxT(""), 10);

         mBassS = S.Id(ID_Bass)
            .Name(XO("Bass"))
            .Style(wxSL_HORIZONTAL)
            .AddSlider( {}, 0, MAX_Bass * SCL_Bass, MIN_Bass * SCL_Bass);

         // Treble control
         mTrebleT = S.Id(ID_Treble)
            .Validator<FloatingPointValidator<double>>(
               1, &mTreble, NumValidatorStyle::DEFAULT, MIN_Treble, MAX_Treble)
            .AddTextBox(XXO("&Treble (dB):"), wxT(""), 10);

         mTrebleS = S.Id(ID_Treble)
            .Name(XO("Treble"))
            .Style(wxSL_HORIZONTAL)
            .AddSlider( {}, 0, MAX_Treble * SCL_Treble, MIN_Treble * SCL_Treble);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(XO("Output"));
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(2);

         // Gain control
         mGainT = S.Id(ID_Gain)
            .Validator<FloatingPointValidator<double>>(
               1, &mGain, NumValidatorStyle::DEFAULT, MIN_Gain, MAX_Gain)
            .AddTextBox(XXO("&Volume (dB):"), wxT(""), 10);

         mGainS = S.Id(ID_Gain)
            .Name(XO("Level"))
            .Style(wxSL_HORIZONTAL)
            .AddSlider( {}, 0, MAX_Gain * SCL_Gain, MIN_Gain * SCL_Gain);
      }
      S.EndMultiColumn();

      S.StartMultiColumn(2, wxCENTER);
      {
         // Link checkbox
         mLinkCheckBox = S.Id(ID_Link).AddCheckBox(XXO("&Link Volume control to Tone controls"),
                                          DEF_Link);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
}

bool EffectBassTreble::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mBassS->SetValue((int) (mBass * SCL_Bass));
   mTrebleS->SetValue((int) mTreble *SCL_Treble);
   mGainS->SetValue((int) mGain * SCL_Gain);
   mLinkCheckBox->SetValue(mLink);

   return true;
}

bool EffectBassTreble::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}


// EffectBassTreble implementation

void EffectBassTreble::InstanceInit(EffectBassTrebleState & data, float sampleRate)
{
   data.samplerate = sampleRate;
   data.slope = 0.4f;   // same slope for both filters
   data.hzBass = 250.0f;   // could be tunable in a more advanced version
   data.hzTreble = 4000.0f;   // could be tunable in a more advanced version

   data.a0Bass = 1;
   data.a1Bass = 0;
   data.a2Bass = 0;
   data.b0Bass = 0;
   data.b1Bass = 0;
   data.b2Bass = 0;

   data.a0Treble = 1;
   data.a1Treble = 0;
   data.a2Treble = 0;
   data.b0Treble = 0;
   data.b1Treble = 0;
   data.b2Treble = 0;

   data.xn1Bass = 0;
   data.xn2Bass = 0;
   data.yn1Bass = 0;
   data.yn2Bass = 0;

   data.xn1Treble = 0;
   data.xn2Treble = 0;
   data.yn1Treble = 0;
   data.yn2Treble = 0;

   data.bass = -1;
   data.treble = -1;
   data.gain = DB_TO_LINEAR(mGain);

}


// EffectClientInterface implementation


size_t EffectBassTreble::InstanceProcess(EffectBassTrebleState & data,
                                              float **inBlock,
                                              float **outBlock,
                                              size_t blockLen)
{
   float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   // Set value to ensure correct rounding
   double oldBass = DB_TO_LINEAR(mBass);
   double oldTreble = DB_TO_LINEAR(mTreble);

   data.gain = DB_TO_LINEAR(mGain);

   // Compute coefficients of the low shelf biquand IIR filter
   if (data.bass != oldBass)
      Coefficients(data.hzBass, data.slope, mBass, data.samplerate, kBass,
                  data.a0Bass, data.a1Bass, data.a2Bass,
                  data.b0Bass, data.b1Bass, data.b2Bass);

   // Compute coefficients of the high shelf biquand IIR filter
   if (data.treble != oldTreble)
      Coefficients(data.hzTreble, data.slope, mTreble, data.samplerate, kTreble,
                  data.a0Treble, data.a1Treble, data.a2Treble,
                  data.b0Treble, data.b1Treble, data.b2Treble);

   for (decltype(blockLen) i = 0; i < blockLen; i++) {
      obuf[i] = DoFilter(data, ibuf[i]) * data.gain;
   }

   return blockLen;
}



// Effect implementation


void EffectBassTreble::Coefficients(double hz, double slope, double gain, double samplerate, int type,
                                   double& a0, double& a1, double& a2,
                                   double& b0, double& b1, double& b2)
{
   double w = 2 * M_PI * hz / samplerate;
   double a = exp(log(10.0) * gain / 40);
   double b = sqrt((a * a + 1) / slope - (pow((a - 1), 2)));

   if (type == kBass)
   {
      b0 = a * ((a + 1) - (a - 1) * cos(w) + b * sin(w));
      b1 = 2 * a * ((a - 1) - (a + 1) * cos(w));
      b2 = a * ((a + 1) - (a - 1) * cos(w) - b * sin(w));
      a0 = ((a + 1) + (a - 1) * cos(w) + b * sin(w));
      a1 = -2 * ((a - 1) + (a + 1) * cos(w));
      a2 = (a + 1) + (a - 1) * cos(w) - b * sin(w);
   }
   else //assumed kTreble
   {
      b0 = a * ((a + 1) + (a - 1) * cos(w) + b * sin(w));
      b1 = -2 * a * ((a - 1) + (a + 1) * cos(w));
      b2 = a * ((a + 1) + (a - 1) * cos(w) - b * sin(w));
      a0 = ((a + 1) - (a - 1) * cos(w) + b * sin(w));
      a1 = 2 * ((a - 1) - (a + 1) * cos(w));
      a2 = (a + 1) - (a - 1) * cos(w) - b * sin(w);
   }
}

float EffectBassTreble::DoFilter(EffectBassTrebleState & data, float in)
{
   // Bass filter
   float out = (data.b0Bass * in + data.b1Bass * data.xn1Bass + data.b2Bass * data.xn2Bass -
         data.a1Bass * data.yn1Bass - data.a2Bass * data.yn2Bass) / data.a0Bass;
   data.xn2Bass = data.xn1Bass;
   data.xn1Bass = in;
   data.yn2Bass = data.yn1Bass;
   data.yn1Bass = out;

   // Treble filter
   in = out;
   out = (data.b0Treble * in + data.b1Treble * data.xn1Treble + data.b2Treble * data.xn2Treble -
         data.a1Treble * data.yn1Treble - data.a2Treble * data.yn2Treble) / data.a0Treble;
   data.xn2Treble = data.xn1Treble;
   data.xn1Treble = in;
   data.yn2Treble = data.yn1Treble;
   data.yn1Treble = out;

   return out;
}


void EffectBassTreble::OnBassText(wxCommandEvent & WXUNUSED(evt))
{
   double oldBass = mBass;

   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   if (mLink) UpdateGain(oldBass, kBass);
   mBassS->SetValue((int) (mBass * SCL_Bass));
}

void EffectBassTreble::OnTrebleText(wxCommandEvent & WXUNUSED(evt))
{
   double oldTreble = mTreble;

   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   if (mLink) UpdateGain(oldTreble, kTreble);
   mTrebleS->SetValue((int) (mTreble * SCL_Treble));
}

void EffectBassTreble::OnGainText(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mGainS->SetValue((int) (mGain * SCL_Gain));
}

void EffectBassTreble::OnBassSlider(wxCommandEvent & evt)
{
   double oldBass = mBass;
   mBass = (double) evt.GetInt() / SCL_Bass;
   mBassT->GetValidator()->TransferToWindow();

   if (mLink) UpdateGain(oldBass, kBass);
   EnableApply(mUIParent->Validate());
}

void EffectBassTreble::OnTrebleSlider(wxCommandEvent & evt)
{
   double oldTreble = mTreble;
   mTreble = (double) evt.GetInt() / SCL_Treble;
   mTrebleT->GetValidator()->TransferToWindow();

   if (mLink) UpdateGain(oldTreble, kTreble);
   EnableApply(mUIParent->Validate());
}

void EffectBassTreble::OnGainSlider(wxCommandEvent & evt)
{
   mGain = (double) evt.GetInt() / SCL_Gain;
   mGainT->GetValidator()->TransferToWindow();

   EnableApply(mUIParent->Validate());
}

void EffectBassTreble::OnLinkCheckbox(wxCommandEvent& /*evt*/)
{
   mLink = mLinkCheckBox->GetValue();
}

void EffectBassTreble::UpdateGain(double oldVal, int control)
{
   double newVal;
   oldVal = (oldVal > 0)? oldVal / 2.0 : oldVal / 4.0;

   if (control == kBass)
      newVal = (mBass > 0)? mBass / 2.0 : mBass / 4.0;
   else
      newVal = (mTreble > 0)? mTreble / 2.0 : mTreble / 4.0;

   mGain -= newVal - oldVal;
   mGain = std::min(MAX_Gain, std::max(MIN_Gain, mGain));

   mGainS->SetValue(mGain);
   mGainT->GetValidator()->TransferToWindow();

}
