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


#include "../Audacity.h"


#include <math.h>

#include <wx/intl.h>

#include "../LabelTrack.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/valnum.h"

#include "Repeat.h"

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name    Type  Key             Def  Min   Max      Scale
Param( Count,  int,  XO("Count"),    1,  1,    INT_MAX, 1  );

BEGIN_EVENT_TABLE(EffectRepeat, wxEvtHandler)
   EVT_TEXT(wxID_ANY, EffectRepeat::OnRepeatTextChange)
END_EVENT_TABLE()

EffectRepeat::EffectRepeat()
{
   repeatCount = DEF_Count;

   SetLinearEffectFlag(true);
}

EffectRepeat::~EffectRepeat()
{
}

// IdentInterface implementation

wxString EffectRepeat::GetSymbol()
{
   return REPEAT_PLUGIN_SYMBOL;
}

wxString EffectRepeat::GetDescription()
{
   return XO("Repeats the selection the specified number of times");
}

// EffectIdentInterface implementation

EffectType EffectRepeat::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

bool EffectRepeat::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Count, repeatCount);

   return true;
}

bool EffectRepeat::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyInt(Count);

   repeatCount = Count;

   return true;
}

// Effect implementation

bool EffectRepeat::Process()
{
   // Set up mOutputTracks.
   // This effect needs Track::All for sync-lock grouping.
   CopyInputTracks(Track::All);

   int nTrack = 0;
   bool bGoodResult = true;
   double maxDestLen = 0.0; // used to change selection to generated bit

   TrackListIterator iter(mOutputTracks.get());

   for (Track *t = iter.First(); t && bGoodResult; t = iter.Next())
   {
      if (t->GetKind() == Track::Label)
      {
         if (t->GetSelected() || t->IsSyncLockSelected())
         {
            LabelTrack* track = (LabelTrack*)t;

            if (!track->Repeat(mT0, mT1, repeatCount))
            {
               bGoodResult = false;
               break;
            }
         }
      }
      else if (t->GetKind() == Track::Wave && t->GetSelected())
      {
         WaveTrack* track = (WaveTrack*)t;

         auto start = track->TimeToLongSamples(mT0);
         auto end = track->TimeToLongSamples(mT1);
         auto len = end - start;
         double tLen = track->LongSamplesToTime(len);
         double tc = mT0 + tLen;

         if (len <= 0)
         {
            continue;
         }

         auto dest = track->Copy(mT0, mT1);
         for(int j=0; j<repeatCount; j++)
         {
            if (!track->Paste(tc, dest.get()) ||
                  TrackProgress(nTrack, j / repeatCount)) // TrackProgress returns true on Cancel.
            {
               bGoodResult = false;
               break;
            }
            tc += tLen;
         }
         if (tc > maxDestLen)
            maxDestLen = tc;
         nTrack++;
      }
      else if (t->IsSyncLockSelected())
      {
         t->SyncLockAdjust(mT1, mT1 + (mT1 - mT0) * repeatCount);
      }
   }

   if (bGoodResult)
   {
      // Select the NEW bits + original bit
      mT1 = maxDestLen;
   }

   ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

void EffectRepeat::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxCENTER, false);
   {
      IntegerValidator<int> vldRepeatCount(&repeatCount);
      vldRepeatCount.SetRange(MIN_Count, 2147483647 / mProjectRate);
      mRepeatCount = S.AddTextBox(_("Number of repeats to add:"), wxT(""), 12);
      mRepeatCount->SetValidator(vldRepeatCount);
   }
   S.EndHorizontalLay();

   S.StartMultiColumn(1, wxCENTER);
   {
      mCurrentTime = S.AddVariableText(_("Current selection length: dd:hh:mm:ss"));
      mTotalTime = S.AddVariableText(_("New selection length: dd:hh:mm:ss"));
   }
   S.EndMultiColumn();
}

bool EffectRepeat::TransferDataToWindow()
{
   mRepeatCount->ChangeValue(wxString::Format(wxT("%d"), repeatCount));

   DisplayNewTime();

   return true;
}

bool EffectRepeat::TransferDataFromWindow()
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

   str = _("Current selection length: ") + nc.GetString();

   mCurrentTime->SetLabel(str);
   mCurrentTime->SetName(str); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   if (l > 0) {
      EnableApply(true);
      repeatCount = l;

      nc.SetValue((mT1 - mT0) * (repeatCount + 1));
      str = _("New selection length: ") + nc.GetString();
   }
   else {
      str = _("Warning: No repeats.");
      EnableApply(false);
   }
   mTotalTime->SetLabel(str);
   mTotalTime->SetName(str); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
}

void EffectRepeat::OnRepeatTextChange(wxCommandEvent & WXUNUSED(evt))
{
   DisplayNewTime();
}
