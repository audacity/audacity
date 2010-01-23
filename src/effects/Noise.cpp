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
   gPrefs->Read(wxT("/CsPresets/NoiseGen_Duration"), &mDuration, 1L);
   return true;
}

bool EffectNoise::PromptUser()
{
   wxArrayString noiseTypeList;

   noiseTypeList.Add(_("White"));
   noiseTypeList.Add(_("Pink"));
   noiseTypeList.Add(_("Brown"));

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
      gPrefs->Read(wxT("/CsPresets/NoiseGen_Duration"), &mDuration, 1L);
      dlog.nIsSelection = false;
   }

   gPrefs->Read(wxT("/CsPresets/NoiseGen_Type"), &noiseType, 0L);
   gPrefs->Read(wxT("/CsPresets/NoiseGen_Amp"), &noiseAmplitude, 0.8f);

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

bool EffectNoise::TransferParameters( Shuttle & shuttle )
{
   return true;
}

bool EffectNoise::MakeNoise(float *buffer, sampleCount len, float fs, float amplitude)
{
   float white, buf0, buf1, buf2, buf3, buf4, buf5;
   float a0, b1, fc, y;
   sampleCount i;
   float div = ((float)RAND_MAX) / 2.0f;

   switch (noiseType) {
   default:
   case 0: // white
       for(i=0; i<len; i++)
          buffer[i] = amplitude * ((rand() / div) - 1.0f);
       break;

   case 1: // pink
       white=buf0=buf1=buf2=buf3=buf4=buf5=0;

       // 0.55f is an experimental normalization factor: thanks to Martyn
       amplitude *= 0.55f;
       for(i=0; i<len; i++) {
        white=(rand() / div) - 1.0f;
        buf0=0.997f * buf0 + 0.029591f * white;
        buf1=0.985f * buf1 + 0.032534f * white;
        buf2=0.950f * buf2 + 0.048056f * white;
        buf3=0.850f * buf3 + 0.090579f * white;
        buf4=0.620f * buf4 + 0.108990f * white;
        buf5=0.250f * buf5 + 0.255784f * white;
        buffer[i] = amplitude * (buf0 + buf1 + buf2 + buf3 + buf4 + buf5);
       };
       break;

   case 2: // brown
       // fc=100 Hz,
       // y[n]=a0*x[n] + b1*y[n-1];
       white=a0=b1=fc=y=0;
       fc=100; //fs=44100;
       b1=exp(-2*M_PI*fc/fs);
       a0=1.0f-b1;

       // 6.2f is an experimental normalization factor: thanks to Martyn
       amplitude *= 6.2f;
       for(i=0; i<len; i++){
         white=(rand() / div) - 1.0f;
         y = (a0 * white + b1 * y);
         buffer[i] = amplitude * y;
       };
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
      gPrefs->Write(wxT("/CsPresets/NoiseGen_Duration"), mDuration);

   gPrefs->Write(wxT("/CsPresets/NoiseGen_Type"), noiseType);
   gPrefs->Write(wxT("/CsPresets/NoiseGen_Amp"), noiseAmplitude);
}

//----------------------------------------------------------------------------
// NoiseDialog
//----------------------------------------------------------------------------

#define FREQ_MIN 1
#define FREQ_MAX 20000
#define AMP_MIN 0
#define AMP_MAX 1

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
      S.TieChoice(_("Noise type"), nType, nTypeList);
      S.TieTextBox(_("Amplitude (0-1)"),  nAmplitude, 10);
      S.AddPrompt(_("Duration"));
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
         mNoiseDurationT->SetFormatString(mNoiseDurationT->GetBuiltinFormat(nIsSelection==true?(wxT("hh:mm:ss + samples")):(wxT("seconds"))));
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

void NoiseDialog::OnTimeCtrlUpdate(wxCommandEvent & event) {
   Fit();
}


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 0ca03dc2-c229-44b4-a6eb-1d5d04a3983c

