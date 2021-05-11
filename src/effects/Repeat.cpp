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

#include <wx/intl.h>
#include <wx/stattext.h>

#include "../LabelTrack.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/valnum.h"

#include "LoadEffects.h"

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name    Type  Key             Def  Min   Max      Scale
Param( Count,  int,  wxT("Count"),    1,  1,    INT_MAX, 1  );

const ComponentInterfaceSymbol EffectRepeat::Symbol
{ XO("Repeat") };

namespace{ BuiltinEffectsModule::Registration< EffectRepeat > reg; }

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

// ComponentInterface implementation

ComponentInterfaceSymbol EffectRepeat::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectRepeat::GetDescription()
{
   return XO("Repeats the selection the specified number of times");
}

wxString EffectRepeat::ManualPage()
{
   return wxT("Repeat");
}

// EffectDefinitionInterface implementation

EffectType EffectRepeat::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation
bool EffectRepeat::DefineParams( ShuttleParams & S ){
   S.SHUTTLE_PARAM( repeatCount, Count );
   return true;
}

bool EffectRepeat::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_Count, repeatCount);

   return true;
}

bool EffectRepeat::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyInt(Count);

   repeatCount = Count;

   return true;
}

// Effect implementation

bool EffectRepeat::Process()
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
         if (track->GetSelected() || track->IsSyncLockSelected())
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
         double tLen = track->LongSamplesToTime(len);
         double tc = mT0 + tLen;

         if (len <= 0)
            return;

         auto dest = track->Copy(mT0, mT1);
         for(int j=0; j<repeatCount; j++)
         {
            if (TrackProgress(nTrack, j / repeatCount)) // TrackProgress returns true on Cancel.
            {
               bGoodResult = false;
               return;
            }
            track->Paste(tc, dest.get());
            tc += tLen;
         }
         if (tc > maxDestLen)
            maxDestLen = tc;
         nTrack++;
      },
      [&](Track *t)
      {
         if( t->IsSyncLockSelected() )
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

void EffectRepeat::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxCENTER, false);
   {
      mRepeatCount = S.Validator<IntegerValidator<int>>(
            &repeatCount, NumValidatorStyle::DEFAULT,
            MIN_Count, 2147483647 / mProjectRate
         )
         .AddTextBox(XXO("&Number of repeats to add:"), wxT(""), 12);
   }
   S.EndHorizontalLay();

   S.StartMultiColumn(1, wxCENTER);
   {
      mCurrentTime = S.AddVariableText(
         XO("Current selection length: dd:hh:mm:ss"));
      mTotalTime = S.AddVariableText(XO("New selection length: dd:hh:mm:ss"));
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

   str = wxString::Format( _("Current selection length: %s"), nc.GetString() );

   mCurrentTime->SetLabel(str);
   mCurrentTime->SetName(str); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   if (l > 0) {
      EnableApply(true);
      repeatCount = l;

      nc.SetValue((mT1 - mT0) * (repeatCount + 1));
      str = wxString::Format( _("New selection length: %s"), nc.GetString() );
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
