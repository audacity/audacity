/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.cpp

  Dominic Mazzoni
  Vaughan Johnson

*******************************************************************//**

\class EffectRepeat
\brief An Effect that repeats audio several times over.

*//****************************************************************//**

\class RepeatDialog
\brief Dialog used with EffectRepeat

*//*******************************************************************/
#include "Repeat.h"
#include "EffectEditor.h"
#include "EffectOutputTracks.h"

#include <math.h>

#include <wx/stattext.h>

#include "LabelTrack.h"
#include "ShuttleGui.h"
#include "SyncLock.h"
#include "WaveTrack.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/valnum.h"

#include "LoadEffects.h"

const EffectParameterMethods& EffectRepeat::Parameters() const
{
   static CapturedParameters<EffectRepeat,
      Count
   > parameters;
   return parameters;
}

const ComponentInterfaceSymbol EffectRepeat::Symbol
{ XO("Repeat") };

namespace{ BuiltinEffectsModule::Registration< EffectRepeat > reg; }

BEGIN_EVENT_TABLE(EffectRepeat, wxEvtHandler)
   EVT_TEXT(wxID_ANY, EffectRepeat::OnRepeatTextChange)
END_EVENT_TABLE()

EffectRepeat::EffectRepeat()
{
   Parameters().Reset(*this);
   SetLinearEffectFlag(true);
}

EffectRepeat::~EffectRepeat()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectRepeat::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectRepeat::GetDescription() const
{
   return XO("Repeats the selection the specified number of times");
}

ManualPageID EffectRepeat::ManualPage() const
{
   return L"Repeat";
}

// EffectDefinitionInterface implementation

EffectType EffectRepeat::GetType() const
{
   return EffectTypeProcess;
}

// Effect implementation

bool EffectRepeat::Process(EffectInstance &, EffectSettings &)
{
   // Set up mOutputTracks.
   // This effect needs all for sync-lock grouping.
   EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } }, true };

   int nTrack = 0;
   bool bGoodResult = true;
   double maxDestLen = 0.0; // used to change selection to generated bit

   outputs.Get().Any().VisitWhile(bGoodResult,
      [&](LabelTrack &track) {
         if (SyncLock::IsSelectedOrSyncLockSelected(&track))
         {
            if (!track.Repeat(mT0, mT1, repeatCount))
               bGoodResult = false;
         }
      },
      [&](auto &&fallthrough){ return [&](WaveTrack &track) {
         if (!track.GetSelected())
            return fallthrough(); // Fall through to next lambda
         auto start = track.TimeToLongSamples(mT0);
         auto end = track.TimeToLongSamples(mT1);
         auto len = end - start;
         const double tLen = track.LongSamplesToTime(len);
         const double tc = mT0 + tLen;

         if (len <= 0)
            return;

         auto tempList = track.Copy(mT0, mT1);
         const auto firstTemp = *tempList->Any<const WaveTrack>().begin();



         auto t0 = tc;
         for (size_t j = 0; j < repeatCount; ++j) {
            if (TrackProgress(nTrack, j / repeatCount)) {
               // TrackProgress returns true on Cancel.
               bGoodResult = false;
               return;
            }
            track.Paste(t0, *firstTemp);
            t0 += tLen;
         }
         if (t0 > maxDestLen)
            maxDestLen = t0;

         const auto compareIntervals = [](const auto& a, const auto& b) {
            return a->Start() < b->Start();
         };

         const auto eps = 0.5 / track.GetRate();
         auto sortedIntervals = std::vector(
            track.Intervals().begin(),
            track.Intervals().end()
         );
         auto sourceIntervals = std::vector(
            firstTemp->Intervals().begin(),
            firstTemp->Intervals().end()
         );
         std::sort(sortedIntervals.begin(), sortedIntervals.end(), compareIntervals);
         std::sort(sourceIntervals.begin(), sourceIntervals.end(), compareIntervals);
         for (auto it = sortedIntervals.begin(); it != sortedIntervals.end(); ++it)
         {
            const auto& interval = *it;
            //Find first pasted interval
            if (std::abs((*it)->GetPlayStartTime() - tc) > eps)
               continue;

            //Fix pasted clips names
            for(int j = 0; j < repeatCount; ++j)
            {
               for (const auto& src : sourceIntervals)
               {
                  if(it == sortedIntervals.end())
                     break;
                  (*it++)->SetName(src->GetName());
               }
            }
            break;
         }
         nTrack++;
      }; },
      [&](Track &t)
      {
         if (SyncLock::IsSyncLockSelected(&t))
            t.SyncLockAdjust(mT1, mT1 + (mT1 - mT0) * repeatCount);
      }
   );

   if (bGoodResult)
   {
      // Select the NEW bits + original bit
      mT1 = maxDestLen;
   }

   if (bGoodResult)
      outputs.Commit();
   return bGoodResult;
}

std::unique_ptr<EffectEditor> EffectRepeat::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();
   S.StartHorizontalLay(wxCENTER, false);
   {
      mRepeatCount = S.Validator<IntegerValidator<int>>(
            &repeatCount, NumValidatorStyle::DEFAULT,
            Count.min, 2147483647 / mProjectRate )
         .AddTextBox(XXO("&Number of repeats to add:"), L"", 12);
   }
   S.EndHorizontalLay();

   S.StartMultiColumn(1, wxCENTER);
   {
      mCurrentTime = S.AddVariableText(
         XO("Current selection length: dd:hh:mm:ss"));
      mTotalTime = S.AddVariableText(XO("New selection length: dd:hh:mm:ss"));
   }
   S.EndMultiColumn();
   return nullptr;
}

bool EffectRepeat::TransferDataToWindow(const EffectSettings &)
{
   mRepeatCount->ChangeValue(wxString::Format(wxT("%d"), repeatCount));

   DisplayNewTime();

   return true;
}

bool EffectRepeat::TransferDataFromWindow(EffectSettings &)
{
   if (!mUIParent->Validate())
   {
      return false;
   }

   long l;

   mRepeatCount->GetValue().ToLong(&l);

   repeatCount = (int) l;

   return true;
}

void EffectRepeat::DisplayNewTime()
{
   long l;
   wxString str;
   mRepeatCount->GetValue().ToLong(&l);

   NumericConverter nc(FormatterContext::SampleRateContext(mProjectRate),
                       NumericConverterType_TIME(),
                       GetSelectionFormat(),
                       mT1 - mT0);

   str = wxString::Format( _("Current selection length: %s"), nc.GetString() );

   mCurrentTime->SetLabel(str);
   mCurrentTime->SetName(str); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   if (l > 0) {
      EffectEditor::EnableApply(mUIParent, true);
      repeatCount = l;

      nc.SetValue((mT1 - mT0) * (repeatCount + 1));
      str = wxString::Format( _("New selection length: %s"), nc.GetString() );
   }
   else {
      str = _("Warning: No repeats.");
      EffectEditor::EnableApply(mUIParent, false);
   }
   mTotalTime->SetLabel(str);
   mTotalTime->SetName(str); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
}

void EffectRepeat::OnRepeatTextChange(wxCommandEvent & WXUNUSED(evt))
{
   DisplayNewTime();
}

bool EffectRepeat::NeedsDither() const
{
   return false;
}
