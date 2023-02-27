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
#include "FindClipping.h"
#include "AnalysisTracks.h"
#include "EffectEditor.h"
#include "EffectOutputTracks.h"
#include "LoadEffects.h"

#include <math.h>


#include "ShuttleGui.h"
#include "../widgets/valnum.h"
#include "AudacityMessageBox.h"

#include "LabelTrack.h"
#include "WaveTrack.h"

const EffectParameterMethods& EffectFindClipping::Parameters() const
{
   static CapturedParameters<EffectFindClipping,
      Start, Stop
   > parameters;
   return parameters;
}

const ComponentInterfaceSymbol EffectFindClipping::Symbol
{ XO("Find Clipping") };

namespace{ BuiltinEffectsModule::Registration< EffectFindClipping > reg; }

EffectFindClipping::EffectFindClipping()
{
   Parameters().Reset(*this);
}

EffectFindClipping::~EffectFindClipping()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectFindClipping::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectFindClipping::GetDescription() const
{
   return XO("Creates labels where clipping is detected");
}

ManualPageID EffectFindClipping::ManualPage() const
{
   return L"Find_Clipping";
}

// EffectDefinitionInterface implementation

EffectType EffectFindClipping::GetType() const
{
   return EffectTypeAnalyze;
}

// Effect implementation

bool EffectFindClipping::Process(EffectInstance &, EffectSettings &)
{
   std::shared_ptr<AddedAnalysisTrack> addedTrack;
   std::optional<ModifiedAnalysisTrack> modifiedTrack;
   const wxString name{ _("Clipping") };

   auto clt = *inputTracks()->Any<const LabelTrack>().find_if(
      [&](const Track *track){ return track->GetName() == name; });

   LabelTrack *lt{};
   if (!clt)
      addedTrack = (AddAnalysisTrack(*mTracks, name)), lt = addedTrack->get();
   else
      modifiedTrack.emplace(ModifyAnalysisTrack(*mTracks, *clt, name)),
      lt = modifiedTrack->get();

   int count = 0;

   // JC: Only process selected tracks.
   // PRL:  Compute the strech into temporary tracks.  Don't commit the stretch.
   EffectOutputTracks temp { *mTracks, GetType(), { { mT0, mT1 } } };
   for (auto t : temp.Get().Selected<const WaveTrack>()) {
      double trackStart = t->GetStartTime();
      double trackEnd = t->GetEndTime();
      double t0 = std::max(trackStart, mT0);
      double t1 = std::min(trackEnd, mT1);
      if (t1 > t0) {
         auto start = t->TimeToLongSamples(t0);
         auto end = t->TimeToLongSamples(t1);
         auto len = end - start;

         for (const auto pChannel : t->Channels())
            if (!ProcessOne(*lt, count++, *pChannel, start, len))
               return false;
      }
   }

   // No cancellation, so commit the addition of the track.
   if (addedTrack)
      addedTrack->Commit();
   if (modifiedTrack)
      modifiedTrack->Commit();
   return true;
}

bool EffectFindClipping::ProcessOne(LabelTrack &lt,
   int count, const WaveChannel &wt, sampleCount start, sampleCount len)
{
   bool bGoodResult = true;
   size_t blockSize = (mStart * 1000);

   if (len < mStart)
      return true;

   Floats buffer;
   try {
      // mStart should be positive.
      // if we are throwing bad_alloc and mStart is negative, find out why.
      if (mStart < 0 || (int)blockSize < mStart)
         // overflow
         throw std::bad_alloc{};
      buffer.reinit(blockSize);
   }
   catch( const std::bad_alloc & ) {
      EffectUIServices::DoMessageBox(*this,
         XO("Requested value exceeds memory capacity."));
      return false;
   }

   float *ptr = buffer.get();

   decltype(len) s = 0, startrun = 0, stoprun = 0, samps = 0;
   decltype(blockSize) block = 0;
   double startTime = -1.0;

   while (s < len) {
      if (block == 0) {
         if (TrackProgress(count, s.as_double() / len.as_double() )) {
            bGoodResult = false;
            break;
         }
         block = limitSampleBufferSize( blockSize, len - s );
         wt.GetFloats(buffer.get(), start + s, block);
         ptr = buffer.get();
      }

      float v = fabs(*ptr++);
      if (v >= MAX_AUDIO) {
         if (startrun == 0) {
            startTime = wt.LongSamplesToTime(start + s);
            samps = 0;
         }
         else
            stoprun = 0;
         startrun++;
         samps++;
      }
      else {
         if (startrun >= mStart) {
            stoprun++;
            samps++;
            if (stoprun >= mStop) {
               lt.AddLabel(
                  SelectedRegion(startTime,
                     wt.LongSamplesToTime(start + s - mStop)),
                  /*!
                   i18n-hint: Two numbers are substituted; the second is the
                   size of a set, the first is the size of a subset, and not
                   understood as an ordinal (i.e., not meaning "first", or
                   "second", etc.)
                   */
                  XC("%lld of %lld", "find clipping")
                     .Format(startrun.as_long_long(),
                        (samps - mStop).as_long_long())
                     .Translation());
               startrun = 0;
               stoprun = 0;
               samps = 0;
            }
         }
         else
            startrun = 0;
      }
      s++;
      block--;
   }
   return bGoodResult;
}

std::unique_ptr<EffectEditor> EffectFindClipping::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();
   DoPopulateOrExchange(S, access);
   return nullptr;
}

void EffectFindClipping::DoPopulateOrExchange(
   ShuttleGui & S, EffectSettingsAccess &access)
{
   mpAccess = access.shared_from_this();
   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S
         .Validator<IntegerValidator<int>>(
            &mStart, NumValidatorStyle::DEFAULT, Start.min)
         .TieTextBox(XXO("&Start threshold (samples):"), mStart, 10);

      S
         .Validator<IntegerValidator<int>>(
            &mStop, NumValidatorStyle::DEFAULT, Stop.min)
         .TieTextBox(XXO("St&op threshold (samples):"), mStop, 10);
   }
   S.EndMultiColumn();
}

bool EffectFindClipping::TransferDataToWindow(const EffectSettings &)
{
   ShuttleGui S(mUIParent, eIsSettingToDialog);
   // To do: eliminate this and just use validators for controls
   DoPopulateOrExchange(S, *mpAccess);

   return true;
}

bool EffectFindClipping::TransferDataFromWindow(EffectSettings &)
{
   if (!mUIParent->Validate())
   {
      return false;
   }

   ShuttleGui S(mUIParent, eIsGettingFromDialog);
   // To do: eliminate this and just use validators for controls
   DoPopulateOrExchange(S, *mpAccess);

   return true;
}
