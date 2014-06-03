/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.cpp

  Dominic Mazzoni
  Vaughan Johnson (dialog)

*******************************************************************//**

\class EffectEcho
\brief An Effect that causes an echo, variable delay and volume.

*//****************************************************************//**

\class EchoDialog
\brief EchoDialog used with EffectEcho

*//*******************************************************************/


#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/validate.h>
#include <wx/valtext.h>

#include <wx/generic/textdlgg.h>
#include <wx/intl.h>
#include <math.h>

#include "Echo.h"
#include "../WaveTrack.h"

EffectEcho::EffectEcho()
{
   delay = float(1.0);
   decay = float(0.5);
}

wxString EffectEcho::GetEffectDescription() {
   // Note: This is useful only after values have been set.
   return wxString::Format(_("Applied effect: %s delay = %f seconds, decay factor = %f"),
                           this->GetEffectName().c_str(), delay, decay);
}

bool EffectEcho::PromptUser()
{
   EchoDialog dlog(this, mParent);
   dlog.delay = delay;
   dlog.decay = decay;
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   delay = dlog.delay;
   decay = dlog.decay;

   return true;
}

bool EffectEcho::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferFloat(wxT("Delay"),delay,1.0);
   shuttle.TransferFloat(wxT("Decay"),decay,0.5);
   return true;
}

bool EffectEcho::Process()
{
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         sampleCount start = track->TimeToLongSamples(t0);
         sampleCount end = track->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);

         if (!ProcessOne(count, track, start, len))
         {
            bGoodResult = false;
            break;
         }
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

bool EffectEcho::ProcessOne(int count, WaveTrack * track,
                            sampleCount start, sampleCount len)
{
   sampleCount s = 0;
   sampleCount blockSize = (sampleCount) (track->GetRate() * delay);

   //do nothing if the delay is less than 1 sample or greater than
   //the length of the selection
   if (blockSize < 1 || blockSize > len)
      return true;

   float *buffer0 = new float[blockSize];
   float *buffer1 = new float[blockSize];

   float *ptr0 = buffer0;
   float *ptr1 = buffer1;

   bool first = true;

   while (s < len) {
      sampleCount block = blockSize;
      if (s + block > len)
         block = len - s;

      track->Get((samplePtr)ptr0, floatSample, start + s, block);
      if (!first) {
         for (sampleCount i = 0; i < block; i++)
            ptr0[i] += ptr1[i] * decay;
         track->Set((samplePtr)ptr0, floatSample, start + s, block);
      }

      float *ptrtemp = ptr0;
      ptr0 = ptr1;
      ptr1 = ptrtemp;

      first = false;

      s += block;

      if (TrackProgress(count, s / (double) len)) {
         delete[]buffer0;
         delete[]buffer1;

         return false;
      }
   }

   delete[]buffer0;
   delete[]buffer1;

   return true;
}

//----------------------------------------------------------------------------
// EchoDialog
//----------------------------------------------------------------------------

// event table for EchoDialog

BEGIN_EVENT_TABLE(EchoDialog, EffectDialog)
    EVT_BUTTON(ID_EFFECT_PREVIEW, EchoDialog::OnPreview)
END_EVENT_TABLE()

EchoDialog::EchoDialog(EffectEcho * effect, wxWindow * parent)
: EffectDialog(parent, _("Echo"))
{
   m_bLoopDetect = false;
   m_pEffect = effect;

   // NULL out these control members because there are some cases where the
   // event table handlers get called during this method, and those handlers that
   // can cause trouble check for NULL.
   m_pTextCtrl_Delay = NULL;
   m_pTextCtrl_Decay = NULL;

   // effect parameters
   delay = float(1.0);
   decay = float(0.5);

   // Initialize dialog
   Init();
}

void EchoDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      m_pTextCtrl_Delay = S.AddTextBox(_("Delay time (seconds):"),
                                       wxT("1.0"),
                                       10);
      m_pTextCtrl_Delay->SetValidator(wxTextValidator(wxFILTER_NUMERIC));

      m_pTextCtrl_Decay = S.AddTextBox(_("Decay factor:"),
                                       wxT("0.5"),
                                       10);
      m_pTextCtrl_Decay->SetValidator(wxTextValidator(wxFILTER_NUMERIC));
   }
   S.EndMultiColumn();
}

bool EchoDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

   wxString str;
   if (m_pTextCtrl_Delay) {
      str.Printf(wxT("%g"), delay);
      m_pTextCtrl_Delay->SetValue(str);
   }
   if (m_pTextCtrl_Decay) {
      str.Printf(wxT("%g"), decay);
      m_pTextCtrl_Decay->SetValue(str);
   }

   m_bLoopDetect = false;
   return true;
}

bool EchoDialog::TransferDataFromWindow()
{
   double newValue;
   wxString str;
   if (m_pTextCtrl_Delay) {
      str = m_pTextCtrl_Delay->GetValue();
      str.ToDouble(&newValue);
      delay = (float)(newValue);
   }
   if (m_pTextCtrl_Decay) {
      str = m_pTextCtrl_Decay->GetValue();
      str.ToDouble(&newValue);
      decay = (float)(newValue);
   }
   return true;
}

// handler implementations for EchoDialog

void EchoDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   float oldDelay = m_pEffect->delay;
   float oldDecay = m_pEffect->decay;

   m_pEffect->delay = delay;
   m_pEffect->decay = decay;

   m_pEffect->Preview();

   m_pEffect->delay = oldDelay;
   m_pEffect->decay = oldDecay;
}
