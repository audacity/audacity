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

#include <math.h>

#include <wx/intl.h>

#include "../AudacityApp.h"
#include "../widgets/valnum.h"

#include "FindClipping.h"

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name    Type  Key               Def   Min   Max      Scale
Param( Start,  int,  XO("Duty Cycle"), 3,    1,    INT_MAX, 1   );
Param( Stop,   int,  XO("Duty Cycle"), 3,    1,    INT_MAX, 1   );

EffectFindClipping::EffectFindClipping()
{
   mStart = DEF_Start;
   mStop = DEF_Stop;
}

EffectFindClipping::~EffectFindClipping()
{
}

// IdentInterface implementation

wxString EffectFindClipping::GetSymbol()
{
   return FINDCLIPPING_PLUGIN_SYMBOL;
}

wxString EffectFindClipping::GetDescription()
{
   return XO("This displays runs of clipped samples in a Label Track");
}

// EffectIdentInterface implementation

EffectType EffectFindClipping::GetType()
{
   return EffectTypeAnalyze;
}

// EffectClientInterface implementation

bool EffectFindClipping::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Start, mStart);
   parms.Write(KEY_Stop, mStop);

   return true;
}

bool EffectFindClipping::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyInt(Start);
   ReadAndVerifyInt(Stop);

   mStart = Start;
   mStop = Stop;

   return true;
}

// Effect implementation

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
               l->AddLabel(SelectedRegion(startTime,
                                          t->LongSamplesToTime(start + s - mStop)),
                           wxString::Format(wxT("%lld of %lld"), (long long) startrun, (long long) (samps - mStop)));
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

void EffectFindClipping::PopulateOrExchange(ShuttleGui & S)
{
   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      IntegerValidator<int> vldStart(&mStart);
      vldStart.SetMin(MIN_Start);
      S.TieTextBox(_("Start threshold (samples):"),
                   mStart,
                   10)->SetValidator(vldStart);

      IntegerValidator<int> vldStop(&mStop);
      vldStop.SetMin(MIN_Stop);
      S.TieTextBox(_("Stop threshold (samples):"),
                   mStop,
                   10)->SetValidator(vldStop);
   }
   S.EndMultiColumn();
}

bool EffectFindClipping::TransferDataToWindow()
{
   ShuttleGui S(mUIParent, eIsSettingToDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectFindClipping::TransferDataFromWindow()
{
   if (!mUIParent->Validate())
   {
      return false;
   }

   ShuttleGui S(mUIParent, eIsGettingFromDialog);
   PopulateOrExchange(S);

   return true;
}
