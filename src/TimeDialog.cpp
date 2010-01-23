/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeDialog.cpp

  Dominic Mazzoni

*******************************************************************//**

\class TimeDialog
\brief Dialog used to request a time value.

*//*******************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/dynarray.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/string.h>

#include "ShuttleGui.h"
#include "TimeDialog.h"

BEGIN_EVENT_TABLE(TimeDialog, wxDialog)
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, TimeDialog::OnUpdate)
END_EVENT_TABLE()

TimeDialog::TimeDialog(wxWindow *parent,
                       const wxString &title,
                       const wxString &prompt)
:  wxDialog(parent, wxID_ANY, title),
   mPrompt(prompt),
   mFormat(wxT("seconds")),
   mRate(44100),
   mTime(0.0),
   mTimeCtrl(NULL)
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

void TimeDialog::PopulateOrExchange(ShuttleGui &S)
{
   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      S.StartStatic(mPrompt, true);
      {
         mTimeCtrl = new
            TimeTextCtrl(this,
                         wxID_ANY,
                         wxT(""),
                         mTime,
                         mRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
         mTimeCtrl->SetName(mPrompt);
         mTimeCtrl->SetFormatString(mTimeCtrl->GetBuiltinFormat(mFormat));
         S.AddWindow(mTimeCtrl);
         mTimeCtrl->EnableMenu();
      }
      S.EndStatic();
   }
   S.EndVerticalLay();
   S.AddStandardButtons();

   TransferDataToWindow();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

bool TimeDialog::TransferDataToWindow()
{
   mTimeCtrl->SetFormatString(mTimeCtrl->GetBuiltinFormat(mFormat));
   mTimeCtrl->SetSampleRate(mRate);
   mTimeCtrl->SetTimeValue(mTime);
   mTimeCtrl->SetFocus();

   return true;
}

bool TimeDialog::TransferDataFromWindow()
{
   mTime = mTimeCtrl->GetTimeValue();

   return true;
}

const double TimeDialog::GetTimeValue()
{
   return mTime;
}

void TimeDialog::SetFormatString(wxString formatString)
{
   mFormat = formatString;
   TransferDataToWindow();
}

void TimeDialog::SetSampleRate(double sampleRate)
{
   mRate = sampleRate;
   TransferDataToWindow();
}

void TimeDialog::SetTimeValue(double newTime)
{
   mTime = newTime;
   TransferDataToWindow();
}

void TimeDialog::OnUpdate(wxCommandEvent &event)
{
   Layout();
   Refresh();

   event.Skip(false);
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
// arch-tag: 94f72c32-970b-4f4e-bbf3-3880fce7b965
