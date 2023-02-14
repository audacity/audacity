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


#include <math.h>

#include <wx/stattext.h>

#include "../LabelTrack.h"
#include "ShuttleGui.h"
#include "../SyncLock.h"
#include "WaveTrack.h"
#include "WaveClip.h"
#include "../widgets/NumericTextCtrl.h"
#include "valnum.h"

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
   CopyInputTracks(true);

   int nTrack = 0;
   bool bGoodResult = true;
   double maxDestLen = 0.0; // used to change selection to generated bit

   mOutputTracks->Any().VisitWhile( bGoodResult,
      [&](LabelTrack *track)
      {
         if (SyncLock::IsSelectedOrSyncLockSelected(track))
         {
            if (!track->Repeat(mT0, mT1, repeatCount))
               bGoodResult = false;
         }
      },
      [&](WaveTrack *track, const Track::Fallthrough &fallthrough)
      {
         if (!track->GetSelected())
            return fallthrough(); // Fall through to next lambda
         auto start = track->TimeToLongSamples(mT0);
         auto end = track->TimeToLongSamples(mT1);
         auto len = end - start;
         const double tLen = track->LongSamplesToTime(len);
         const double tc = mT0 + tLen;

         if (len <= 0)
            return;

         auto dest = std::dynamic_pointer_cast<WaveTrack>(track->Copy(mT0, mT1));
         std::vector<wxString> clipNames;
         for(auto clip : dest->SortedClipArray())
         {
            if(!clip->GetIsPlaceholder())
               clipNames.push_back(clip->GetName());
         }
         auto t0 = tc;
         for(int j=0; j<repeatCount; j++)
         {
            if (TrackProgress(nTrack, j / repeatCount)) // TrackProgress returns true on Cancel.
            {
               bGoodResult = false;
               return;
            }
            track->Paste(t0, dest.get());
            t0 += tLen;
         }
         if (t0 > maxDestLen)
            maxDestLen = t0; 

         auto clips = track->SortedClipArray();
         for(size_t i = 0; i < clips.size(); ++i)
         {
            const auto eps = 0.5 / track->GetRate();
            //Find first pasted clip
            if(std::abs(clips[i]->GetPlayStartTime() - tc) > eps)
               continue;

            //Fix pasted clips names
            for(int j = 0; j < repeatCount; ++j)
            {
               for(size_t k = 0; k < clipNames.size(); ++k)
                  clips[i + k]->SetName(clipNames[k]);
               i += clipNames.size();
            }
            break;
         }

         nTrack++;
      },
      [&](Track *t)
      {
         if( SyncLock::IsSyncLockSelected(t) )
            t->SyncLockAdjust(mT1, mT1 + (mT1 - mT0) * repeatCount);
      }
   );

   if (bGoodResult)
   {
      // Select the NEW bits + original bit
      mT1 = maxDestLen;
   }

   ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

std::unique_ptr<EffectUIValidator> EffectRepeat::PopulateOrExchange(
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

   NumericConverter nc(NumericConverter::TIME,
                       GetSelectionFormat(),
                       mT1 - mT0,
                       mProjectRate);

   str = wxString::Format( _("Current selection length: %s"), nc.GetString() );

   mCurrentTime->SetLabel(str);
   mCurrentTime->SetName(str); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   if (l > 0) {
      EffectUIValidator::EnableApply(mUIParent, true);
      repeatCount = l;

      nc.SetValue((mT1 - mT0) * (repeatCount + 1));
      str = wxString::Format( _("New selection length: %s"), nc.GetString() );
   }
   else {
      str = _("Warning: No repeats.");
      EffectUIValidator::EnableApply(mUIParent, false);
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
