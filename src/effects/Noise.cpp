/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectNoise
\brief An Effect for the "Generator" menu to add white noise.

*//*******************************************************************/

#include "Noise.h"
#include "../Audacity.h"
#include "../Project.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846  /* pi */
#endif
#define AMP_MIN 0
#define AMP_MAX 1

//
// EffectNoise
//

bool EffectNoise::Init()
{
   gPrefs->Read(wxT("/Effects/Noise/Duration"), &mDuration, 1L);
   return true;
}

bool EffectNoise::PromptUser()
{
   wxArrayString noiseTypeList;

   noiseTypeList.Add(_("White"));
   noiseTypeList.Add(_("Pink"));
   noiseTypeList.Add(_("Brownian"));

   NoiseDialog dlog(this, mParent, _("Noise Generator"));

   // dialog will be passed values from effect
   // Effect retrieves values from saved config
   // Dialog will take care of using them to initialize controls
   // If there is a selection, use that duration, otherwise use
   // value from saved config: this is useful is user wants to
   // replace selection with noise
   //
   if (mT1 > mT0) {
      mDuration = mT1 - mT0;
      dlog.nIsSelection = true;
   } else {
      gPrefs->Read(wxT("/Effects/Noise/Duration"), &mDuration, 30L);
      dlog.nIsSelection = false;
   }

   gPrefs->Read(wxT("/Effects/Noise/Type"), &noiseType, 0L);
   gPrefs->Read(wxT("/Effects/Noise/Amplitude"), &noiseAmplitude, 0.8f);

   // Initialize dialog locals
   dlog.nDuration = mDuration;
   dlog.nAmplitude = noiseAmplitude;
   dlog.nType = noiseType;
   dlog.nTypeList = &noiseTypeList;

   // start dialog
   dlog.Init();
   dlog.TransferDataToWindow();
   dlog.Fit();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   // if there was an OK, retrieve values
   noiseType = dlog.nType;
   mDuration = dlog.nDuration;
   noiseAmplitude = dlog.nAmplitude;

   return true;
}

bool EffectNoise::TransferParameters( Shuttle & WXUNUSED(shuttle) )
{
   return true;
}

bool EffectNoise::MakeNoise(float *buffer, sampleCount len, float fs, float amplitude)
{
   float white;
   sampleCount i;
   float div = ((float)RAND_MAX) / 2.0f;

   switch (noiseType) {
   default:
   case 0: // white
       for(i=0; i<len; i++)
          buffer[i] = amplitude * ((rand() / div) - 1.0f);
       break;

   case 1: // pink
      // based on Paul Kellet's "instrumentation grade" algorithm.
      white=0;

      // 0.129f is an experimental normalization factor.
      amplitude *= 0.129f;
      for(i=0; i<len; i++) {
      white=(rand() / div) - 1.0f;
      buf0=0.99886f * buf0 + 0.0555179f * white;
      buf1=0.99332f * buf1 + 0.0750759f * white;
      buf2=0.96900f * buf2 + 0.1538520f * white;
      buf3=0.86650f * buf3 + 0.3104856f * white;
      buf4=0.55000f * buf4 + 0.5329522f * white;
      buf5=-0.7616f * buf5 - 0.0168980f * white;
      buffer[i] = amplitude *
         (buf0 + buf1 + buf2 + buf3 + buf4 + buf5 + buf6 + white * 0.5362);
      buf6 = white * 0.115926;
      }
      break;

   case 2: // Brownian
       white=0;
       //float leakage=0.997; // experimental value at 44.1kHz
       //double scaling = 0.05; // experimental value at 44.1kHz
       // min and max protect against instability at extreme sample rates.
       float leakage = ((fs-144.0)/fs < 0.9999)? (fs-144.0)/fs : 0.9999;
       float scaling = (9.0/sqrt(fs) > 0.01)? 9.0/sqrt(fs) : 0.01;

       for(i=0; i<len; i++){
         white=(rand() / div) - 1.0f;
         z = leakage * y + white * scaling;
         y = (fabs(z) > 1.0) ? (leakage * y - white * scaling) : z;
         buffer[i] = amplitude * y;
       }
       break;
   }
   return true;
}

void EffectNoise::GenerateBlock(float *data,
                                const WaveTrack &track,
                                sampleCount block)
{
   MakeNoise(data, block, track.GetRate(), noiseAmplitude);
}

void EffectNoise::Success()
{
   /* save last used values
      save duration unless value was got from selection, so we save only
      when user explicitely setup a value
      */
   if (mT1 == mT0)
      gPrefs->Write(wxT("/Effects/Noise/Duration"), mDuration);

   gPrefs->Write(wxT("/Effects/Noise/Type"), noiseType);
   gPrefs->Write(wxT("/Effects/Noise/Amplitude"), noiseAmplitude);
   gPrefs->Flush();
}

//----------------------------------------------------------------------------
// NoiseDialog
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(NoiseDialog, EffectDialog)
    EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, NoiseDialog::OnTimeCtrlUpdate)
END_EVENT_TABLE()

NoiseDialog::NoiseDialog(EffectNoise * effect, wxWindow * parent, const wxString & title)
:  EffectDialog(parent, title, INSERT_EFFECT),
   mEffect(effect)
{
   mNoiseDurationT = NULL;
   /* // already initialized in EffectNoise::PromptUser
   nDuration = mDuration;
   nAmplitude = noiseAmplitude;
   nType = noiseType;
   */
}

void NoiseDialog::PopulateOrExchange( ShuttleGui & S )
{
   S.StartMultiColumn(2, wxCENTER);
   {
      S.TieChoice(_("Noise type:"), nType, nTypeList);
      S.TieNumericTextBox(_("Amplitude (0-1)") + wxString(wxT(":")), nAmplitude, 10);
      S.AddPrompt(_("Duration") + wxString(wxT(":")));
      if (mNoiseDurationT == NULL)
      {
         mNoiseDurationT = new
         TimeTextCtrl(this,
                      wxID_ANY,
                      wxT(""),
                      nDuration,
                      mEffect->mProjectRate,
                      wxDefaultPosition,
                      wxDefaultSize,
                      true);
         /* use this instead of "seconds" because if a selection is passed to
          * the effect, I want it (nDuration) to be used as the duration, and
          * with "seconds" this does not always work properly. For example,
          * it rounds down to zero... */
         mNoiseDurationT->SetName(_("Duration"));
         mNoiseDurationT->SetFormatString(mNoiseDurationT->GetBuiltinFormat(nIsSelection==true?(_("hh:mm:ss + samples")):(_("hh:mm:ss + milliseconds"))));
         mNoiseDurationT->EnableMenu();
      }
      S.AddWindow(mNoiseDurationT, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL);
   }
   S.EndMultiColumn();
}

bool NoiseDialog::TransferDataToWindow()
{
   EffectDialog::TransferDataToWindow();

   // Must handle this ourselves since ShuttleGui doesn't know about it
   mNoiseDurationT->SetTimeValue(nDuration);

   return true;
}

bool NoiseDialog::TransferDataFromWindow()
{
   EffectDialog::TransferDataFromWindow();

   nAmplitude = TrapDouble(nAmplitude, AMP_MIN, AMP_MAX);

   // Must handle this ourselves since ShuttleGui doesn't know about it
   nDuration = mNoiseDurationT->GetTimeValue();

   return true;
}

void NoiseDialog::OnTimeCtrlUpdate(wxCommandEvent & WXUNUSED(event)) {
   Fit();
}
