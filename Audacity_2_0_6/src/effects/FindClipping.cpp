/**********************************************************************

  Audacity: A Digital Audio Editor

  FindClipping.cpp

  Leland Lucius

*******************************************************************//**

\class EffectFindClipping
\brief Locates clipping and inserts labels when found

*//****************************************************************//**

\class FindClippingDialog
\brief FindClippingDialog used with EffectFindClipping

*//*******************************************************************/


#include "../Audacity.h"
#include "../AudacityApp.h"

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

#include "FindClipping.h"
#include "../LabelTrack.h"
#include "../WaveTrack.h"

EffectFindClipping::EffectFindClipping()
{
   SetEffectFlags(BUILTIN_EFFECT | ANALYZE_EFFECT);
   mStart = 3;
   mStop = 3;
}

wxString EffectFindClipping::GetEffectDescription()
{
   return wxString::Format(_("Detect clipping"));
}

bool EffectFindClipping::PromptUser()
{
   FindClippingDialog dlg(this, mParent);
   dlg.CentreOnParent();

   if (dlg.ShowModal() == wxID_CANCEL) {
      return false;
   }

   return true;
}

bool EffectFindClipping::TransferParameters(Shuttle & shuttle)
{
   shuttle.TransferInt(wxT("Start"), mStart, 3);
   shuttle.TransferInt(wxT("Stop"), mStop, 3);

   return true;
}

bool EffectFindClipping::Process()
{
   LabelTrack *l = NULL;
   Track *original = NULL;

   TrackListOfKindIterator iter(Track::Label, mTracks);
   for (Track *t = iter.First(); t; t = iter.Next()) {
      if (t->GetName() == wxT("Clipping")) {
         l = (LabelTrack *) t;
         // copy LabelTrack here, so it can be undone on cancel
         l->Copy(l->GetStartTime(), l->GetEndTime(), &original);
         original->SetOffset(l->GetStartTime());
         original->SetName(wxT("Clipping"));
         break;
      }
   }

   if (!l) {
      l = mFactory->NewLabelTrack();
      l->SetName(_("Clipping"));
      mTracks->Add((Track *) l);
   }

   int count = 0;

   // JC: Only process selected tracks.
   SelectedTrackListOfKindIterator waves(Track::Wave, mTracks);
   WaveTrack *t = (WaveTrack *) waves.First();
   while (t) {
      double trackStart = t->GetStartTime();
      double trackEnd = t->GetEndTime();
      double t0 = mT0 < trackStart ? trackStart : mT0;
      double t1 = mT1 > trackEnd ? trackEnd : mT1;

      if (t1 > t0) {
         sampleCount start = t->TimeToLongSamples(t0);
         sampleCount end = t->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);

         if (!ProcessOne(l, count, t, start, len)) {
            //put it back how it was
            mTracks->Remove((Track *) l);
            if(original) {
               mTracks->Add((Track *) original);
            }
            return false;
         }
      }

      count++;
      t = (WaveTrack *) waves.Next();
   }

   return true;
}

bool EffectFindClipping::ProcessOne(LabelTrack * l,
                                    int count,
                                    WaveTrack * t,
                                    sampleCount start,
                                    sampleCount len)
{
   bool bGoodResult = true;
   sampleCount s = 0;
   sampleCount blockSize = (sampleCount) (mStart * 1000);

   if (len < mStart) {
      return true;
   }

   float *buffer = new float[blockSize];

   float *ptr = buffer;

   sampleCount startrun = 0;
   sampleCount stoprun = 0;
   sampleCount samps = 0;
   sampleCount block = 0;
   double startTime = -1.0;

   while (s < len) {
      if (block == 0) {
         if (TrackProgress(count, s / (double) len)) {
            bGoodResult = false;
            break;
         }

         block = s + blockSize > len ? len - s : blockSize;

         t->Get((samplePtr)buffer, floatSample, start + s, block);
         ptr = buffer;
      }

      float v = fabs(*ptr++);
      if (v >= MAX_AUDIO) {
         if (startrun == 0) {
            startTime = t->LongSamplesToTime(start + s);
            samps = 0;
         }
         else {
            stoprun = 0;
         }
         startrun++;
         samps++;
      }
      else {
         if (startrun >= mStart) {
            stoprun++;
            samps++;

            if (stoprun >= mStop) {
               l->AddLabel(startTime,
                           t->LongSamplesToTime(start + s - mStop),
                           wxString::Format(wxT("%lld of %lld"), startrun, samps - mStop));
               startrun = 0;
               stoprun = 0;
               samps = 0;
            }
         }
         else {
            startrun = 0;
         }
      }

      s++;
      block--;
   }

   delete [] buffer;

   return bGoodResult;
}

//----------------------------------------------------------------------------
// FindClippingDialog
//----------------------------------------------------------------------------

FindClippingDialog::FindClippingDialog(EffectFindClipping * effect, wxWindow * parent)
: EffectDialog(parent, _("Find Clipping"), INSERT_EFFECT)
{
   mEffect = effect;

   Init();
}

void FindClippingDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(_("Start threshold (samples):"),
                   mEffect->mStart,
                   10)->SetValidator(wxTextValidator(wxFILTER_NUMERIC));

      S.TieTextBox(_("Stop threshold (samples):"),
                   mEffect->mStop,
                   10)->SetValidator(wxTextValidator(wxFILTER_NUMERIC));
   }
   S.EndMultiColumn();
}

bool FindClippingDialog::TransferDataFromWindow()
{
   EffectDialog::TransferDataFromWindow();

   if (mEffect->mStart <= 0 || mEffect->mStop <= 0) {
      wxMessageBox(_("Start and stop must be greater than 0."));
      return false;
   }

   return true;
}
