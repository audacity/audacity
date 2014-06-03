/**********************************************************************

  Audacity: A Digital Audio Editor

  TimerRecordDialog.cpp

  Copyright 2006-2009 by Vaughan Johnson

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

*******************************************************************//**

\class TimerRecordDialog
\brief Dialog for Timer Record, i.e., timed or long recording.

*//*******************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/datetime.h>
#include <wx/intl.h>
#include <wx/progdlg.h>
#include <wx/sizer.h>
#include <wx/string.h>
#include <wx/timer.h>
#include <wx/dynlib.h> //<! For windows.h

#include "TimerRecordDialog.h"
#include "Project.h"
#include "Internat.h"
#include "Prefs.h"

#define TIMER_ID 7000

enum { // control IDs
   ID_DATEPICKER_START = 10000,
   ID_TIMETEXT_START,
   ID_DATEPICKER_END,
   ID_TIMETEXT_END,
   ID_TIMETEXT_DURATION
};

const int kTimerInterval = 50; // ms

static double wxDateTime_to_AudacityTime(wxDateTime& dateTime)
{
   return (dateTime.GetHour() * 3600.0) + (dateTime.GetMinute() * 60.0) + dateTime.GetSecond();
};

BEGIN_EVENT_TABLE(TimerRecordDialog, wxDialog)
   EVT_DATE_CHANGED(ID_DATEPICKER_START, TimerRecordDialog::OnDatePicker_Start)
   EVT_TEXT(ID_TIMETEXT_START, TimerRecordDialog::OnTimeText_Start)

   EVT_DATE_CHANGED(ID_DATEPICKER_END, TimerRecordDialog::OnDatePicker_End)
   EVT_TEXT(ID_TIMETEXT_END, TimerRecordDialog::OnTimeText_End)

   EVT_TEXT(ID_TIMETEXT_DURATION, TimerRecordDialog::OnTimeText_Duration)

   EVT_BUTTON(wxID_OK, TimerRecordDialog::OnOK)

   EVT_TIMER(TIMER_ID, TimerRecordDialog::OnTimer)
END_EVENT_TABLE()

TimerRecordDialog::TimerRecordDialog(wxWindow* parent)
: wxDialog(parent, -1, _("Audacity Timer Record"), wxDefaultPosition,
           wxDefaultSize, wxCAPTION)
{
   m_DateTime_Start = wxDateTime::UNow();
   long seconds; // default duration is 1 hour = 3600 seconds
   gPrefs->Read(wxT("/TimerRecord/LastDuration"), &seconds, 3600);
   m_TimeSpan_Duration = wxTimeSpan::Seconds(seconds);
   m_DateTime_End = m_DateTime_Start + m_TimeSpan_Duration;

   m_pDatePickerCtrl_Start = NULL;
   m_pTimeTextCtrl_Start = NULL;

   m_pDatePickerCtrl_End = NULL;
   m_pTimeTextCtrl_End = NULL;

   m_pTimeTextCtrl_Duration = NULL;

   ShuttleGui S(this, eIsCreating);
   this->PopulateOrExchange(S);

   // Set initial focus to "1" of "01h" in Duration TimeTextCtrl, instead of OK button (default).
   m_pTimeTextCtrl_Duration->SetFocus();
   m_pTimeTextCtrl_Duration->SetFieldFocus(3);

   m_timer.SetOwner(this, TIMER_ID);
   m_timer.Start(kTimerInterval);
}

TimerRecordDialog::~TimerRecordDialog()
{
}

void TimerRecordDialog::OnTimer(wxTimerEvent& WXUNUSED(event))
{
   wxDateTime dateTime_UNow = wxDateTime::UNow();
   if (m_DateTime_Start < dateTime_UNow) {
      m_DateTime_Start = dateTime_UNow;
      m_pDatePickerCtrl_Start->SetValue(m_DateTime_Start);
      m_pTimeTextCtrl_Start->SetTimeValue(wxDateTime_to_AudacityTime(m_DateTime_Start));
      this->UpdateEnd(); // Keep Duration constant and update End for changed Start.
   }
}

void TimerRecordDialog::OnDatePicker_Start(wxDateEvent& WXUNUSED(event))
{
   m_DateTime_Start = m_pDatePickerCtrl_Start->GetValue();
   double dTime = m_pTimeTextCtrl_Start->GetTimeValue();
   long hr = (long)(dTime / 3600.0);
   long min = (long)((dTime - (hr * 3600.0)) / 60.0);
   long sec = (long)(dTime - (hr * 3600.0) - (min * 60.0));
   m_DateTime_Start.SetHour(hr);
   m_DateTime_Start.SetMinute(min);
   m_DateTime_Start.SetSecond(sec);

   // User might have had the dialog up for a while, or
   // had a future day, set hour of day less than now's, then changed day to today.
   wxTimerEvent dummyTimerEvent;
   this->OnTimer(dummyTimerEvent);

   // Always update End for changed Start, keeping Duration constant.
   // Note that OnTimer sometimes calls UpdateEnd, so sometimes this is redundant,
   // but OnTimer doesn't need to always call UpdateEnd, but we must here.
   this->UpdateEnd();
}

void TimerRecordDialog::OnTimeText_Start(wxCommandEvent& WXUNUSED(event))
{
   //v TimeTextCtrl doesn't implement upper ranges, i.e., if I tell it "024 h 060 m 060 s", then
   // user increments the hours past 23, it rolls over to 0 (although if you increment below 0, it stays at 0).
   // So instead, set the max to 99 and just catch hours > 24 and fix the ctrls.
   double dTime = m_pTimeTextCtrl_Start->GetTimeValue();
   long days = (long)(dTime / (24.0 * 3600.0));
   if (days > 0) {
      dTime -= (double)days * 24.0 * 3600.0;
      m_DateTime_Start += wxTimeSpan::Days(days);
      m_pDatePickerCtrl_Start->SetValue(m_DateTime_Start);
      m_pTimeTextCtrl_Start->SetTimeValue(dTime);
   }

   wxDateEvent dummyDateEvent;
   this->OnDatePicker_Start(dummyDateEvent);
}

void TimerRecordDialog::OnDatePicker_End(wxDateEvent& WXUNUSED(event))
{
   m_DateTime_End = m_pDatePickerCtrl_End->GetValue();
   double dTime = m_pTimeTextCtrl_End->GetTimeValue();
   long hr = (long)(dTime / 3600.0);
   long min = (long)((dTime - (hr * 3600.0)) / 60.0);
   long sec = (long)(dTime - (hr * 3600.0) - (min * 60.0));
   m_DateTime_End.SetHour(hr);
   m_DateTime_End.SetMinute(min);
   m_DateTime_End.SetSecond(sec);

   // DatePickerCtrls use SetRange to make sure End is never less than Start, but
   // need to implement it for the TimeTextCtrls.
   if (m_DateTime_End < m_DateTime_Start) {
      m_DateTime_End = m_DateTime_Start;
      m_pDatePickerCtrl_End->SetValue(m_DateTime_End);
      m_pTimeTextCtrl_End->SetTimeValue(wxDateTime_to_AudacityTime(m_DateTime_End));
   }

   this->UpdateDuration(); // Keep Start constant and update Duration for changed End.
}

void TimerRecordDialog::OnTimeText_End(wxCommandEvent& WXUNUSED(event))
{
   //v TimeTextCtrl doesn't implement upper ranges, i.e., if I tell it "024 h 060 m 060 s", then
   // user increments the hours past 23, it rolls over to 0 (although if you increment below 0, it stays at 0).
   // So instead, set the max to 99 and just catch hours > 24 and fix the ctrls.
   double dTime = m_pTimeTextCtrl_End->GetTimeValue();
   long days = (long)(dTime / (24.0 * 3600.0));
   if (days > 0) {
      dTime -= (double)days * 24.0 * 3600.0;
      m_DateTime_End += wxTimeSpan::Days(days);
      m_pDatePickerCtrl_End->SetValue(m_DateTime_End);
      m_pTimeTextCtrl_End->SetTimeValue(dTime);
   }

   wxDateEvent dummyDateEvent;
   this->OnDatePicker_End(dummyDateEvent);
}

void TimerRecordDialog::OnTimeText_Duration(wxCommandEvent& WXUNUSED(event))
{
   double dTime = m_pTimeTextCtrl_Duration->GetTimeValue();
   long hr = (long)(dTime / 3600.0);
   long min = (long)((dTime - (hr * 3600.0)) / 60.0);
   long sec = (long)(dTime - (hr * 3600.0) - (min * 60.0));
   m_TimeSpan_Duration = wxTimeSpan(hr, min, sec); //v milliseconds?

   this->UpdateEnd(); // Keep Start constant and update End for changed Duration.
}

void TimerRecordDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
   this->TransferDataFromWindow();
   if (!m_TimeSpan_Duration.IsPositive())
   {
      wxMessageBox(_("Duration is zero. Nothing will be recorded."),
                     _("Error in Duration"), wxICON_EXCLAMATION | wxOK);
      return;
   }

   m_timer.Stop(); // Don't need to keep updating m_DateTime_Start to prevent backdating.
   this->EndModal(wxID_OK);
   wxLongLong duration = m_TimeSpan_Duration.GetSeconds();
   // this will assert if the duration won't fit in a long
   gPrefs->Write(wxT("/TimerRecord/LastDuration"), duration.ToLong());
   gPrefs->Flush();
}

///Runs the wait for start dialog.  Returns false if the user clicks stop while we are recording
///so that the high
bool TimerRecordDialog::RunWaitDialog()
{
   int updateResult = eProgressSuccess;
   if (m_DateTime_Start > wxDateTime::UNow())
      updateResult = this->WaitForStart();

   if (updateResult != eProgressSuccess)
   {
      // Don't proceed, but don't treat it as canceled recording. User just canceled waiting.
      return true;
   }
   else
   {
      // Record for specified time.
      AudacityProject* pProject = GetActiveProject();
      pProject->OnRecord();
      bool bIsRecording = true;

      wxString strMsg =
         _("Recording start") + (wxString)wxT(":\t\t")
         + GetDisplayDate(m_DateTime_Start) + wxT("\n") + _("Recording end")
         + wxT(":\t\t") + GetDisplayDate(m_DateTime_End) + wxT("\n")
         + _("Duration") + wxT(":\t\t") + m_TimeSpan_Duration.Format();

      TimerProgressDialog
         progress(m_TimeSpan_Duration.GetMilliseconds().GetValue(),
                  _("Audacity Timer Record Progress"),
                  strMsg,
                  pdlgHideCancelButton);

      // Make sure that start and end time are updated, so we always get the full
      // duration, even if there's some delay getting here.
      wxTimerEvent dummyTimerEvent;
      this->OnTimer(dummyTimerEvent);

      // Loop for progress display during recording.
      while (bIsRecording && (updateResult == eProgressSuccess))
      {
         wxMilliSleep(kTimerInterval);
         updateResult = progress.Update();
         bIsRecording = (wxDateTime::UNow() <= m_DateTime_End); // Call UNow() again for extra accuracy...
      }
      pProject->OnStop();
   }
   // Let the caller handle cancellation or failure from recording progress.
   if (updateResult == eProgressCancelled || updateResult == eProgressFailed)
      return false;
   return true;
}

wxString TimerRecordDialog::GetDisplayDate( wxDateTime & dt )
{
#if defined(__WXMSW__)
   // On Windows, wxWidgets uses the system date control and it displays the
   // date based on the Windows locale selected by the user.  But, wxDateTime
   // using the strftime function to return the formatted date.  Since the
   // default locale for the Windows CRT environment is "C", the dates come
   // back in a different format.
   //
   // So, we make direct Windows calls to format the date like it the date
   // control.
   //
   // (Most of this taken from src/msw/datectrl.cpp)

   const wxDateTime::Tm tm(dt.GetTm());
   SYSTEMTIME st;
   wxString s;
   int len;

   st.wYear = (WXWORD)tm.year;
   st.wMonth = (WXWORD)(tm.mon - wxDateTime::Jan + 1);
   st.wDay = tm.mday;
   st.wDayOfWeek = st.wMinute = st.wSecond = st.wMilliseconds = 0;

   len = ::GetDateFormat(LOCALE_USER_DEFAULT,
                         DATE_SHORTDATE,
                         &st,
                         NULL,
                         NULL,
                         0);
   if (len > 0) {
      len = ::GetDateFormat(LOCALE_USER_DEFAULT,
                            DATE_SHORTDATE,
                            &st,
                            NULL,
                            wxStringBuffer(s, len),
                            len);
      if (len > 0) {
         s += wxT(" ") + dt.FormatTime();
         return s;
      }
   }
#endif

   // Use default formatting
wxPrintf(wxT("%s\n"), dt.Format().c_str());
   return dt.FormatDate() + wxT(" ") + dt.FormatTime();
}

void TimerRecordDialog::PopulateOrExchange(ShuttleGui& S)
{
   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      /* i18n-hint: This string is used to configure the controls for times when the recording is
       * started and stopped. As such it is important that only the alphabetic parts of the string
       * are translated, with the numbers left exactly as they are.
       * The 'h' indicates the first number displayed is hours, the 'm' indicates the second number
       * displayed is minutes, and the 's' indicates that the third number displayed is seconds.
       */
      wxString strFormat = _("099 h 060 m 060 s");
      S.StartStatic(_("Start Date and Time"), true);
      {
         m_pDatePickerCtrl_Start =
            new wxDatePickerCtrl(this, // wxWindow *parent,
                                 ID_DATEPICKER_START, // wxWindowID id,
                                 m_DateTime_Start); // const wxDateTime& dt = wxDefaultDateTime,
                                 // const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDP_DEFAULT | wxDP_SHOWCENTURY, const wxValidator& validator = wxDefaultValidator, const wxString& name = "datectrl")
         m_pDatePickerCtrl_Start->SetName(_("Start Date"));
         m_pDatePickerCtrl_Start->SetRange(wxDateTime::Today(), wxInvalidDateTime); // No backdating.
         S.AddWindow(m_pDatePickerCtrl_Start);

         m_pTimeTextCtrl_Start = new TimeTextCtrl(this, ID_TIMETEXT_START);
         m_pTimeTextCtrl_Start->SetName(_("Start Time"));
         m_pTimeTextCtrl_Start->SetFormatString(strFormat);
         m_pTimeTextCtrl_Start->SetTimeValue(wxDateTime_to_AudacityTime(m_DateTime_Start));
         S.AddWindow(m_pTimeTextCtrl_Start);
         m_pTimeTextCtrl_Start->EnableMenu(false);
      }
      S.EndStatic();

      S.StartStatic(_("End Date and Time"), true);
      {
         m_pDatePickerCtrl_End =
            new wxDatePickerCtrl(this, // wxWindow *parent,
                                 ID_DATEPICKER_END, // wxWindowID id,
                                 m_DateTime_End); // const wxDateTime& dt = wxDefaultDateTime,
                                 // const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDP_DEFAULT | wxDP_SHOWCENTURY, const wxValidator& validator = wxDefaultValidator, const wxString& name = "datectrl")
         m_pDatePickerCtrl_End->SetRange(m_DateTime_Start, wxInvalidDateTime); // No backdating.
         m_pDatePickerCtrl_End->SetName(_("End Date"));
         S.AddWindow(m_pDatePickerCtrl_End);

         m_pTimeTextCtrl_End = new TimeTextCtrl(this, ID_TIMETEXT_END);
         m_pTimeTextCtrl_End->SetName(_("End Time"));
         m_pTimeTextCtrl_End->SetFormatString(strFormat);
         m_pTimeTextCtrl_End->SetTimeValue(wxDateTime_to_AudacityTime(m_DateTime_End));
         S.AddWindow(m_pTimeTextCtrl_End);
         m_pTimeTextCtrl_End->EnableMenu(false);
      }
      S.EndStatic();

      S.StartStatic(_("Duration"), true);
      {
         /* i18n-hint: This string is used to configure the controls which shows the recording
          * duration. As such it is important that only the alphabetic parts of the string
          * are translated, with the numbers left exactly as they are.
          * The string 'days' indicates that the first number in the control will be the number of days,
          * then the 'h' indicates the second number displayed is hours, the 'm' indicates the third
          * number displayed is minutes, and the 's' indicates that the fourth number displayed is
          * seconds.
          */
         wxString strFormat1 = _("099 days 024 h 060 m 060 s");
         m_pTimeTextCtrl_Duration = new TimeTextCtrl(this, ID_TIMETEXT_DURATION);
         m_pTimeTextCtrl_Duration->SetName(_("Duration"));
         m_pTimeTextCtrl_Duration->SetFormatString(strFormat1);
         m_pTimeTextCtrl_Duration->SetTimeValue(m_TimeSpan_Duration.GetSeconds().ToDouble());
         S.AddWindow(m_pTimeTextCtrl_Duration);
         m_pTimeTextCtrl_Duration->EnableMenu(false);
      }
      S.EndStatic();
   }
   S.EndVerticalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

bool TimerRecordDialog::TransferDataFromWindow()
{
   double dTime;
   long hr;
   long min;
   long sec;

   m_DateTime_Start = m_pDatePickerCtrl_Start->GetValue();
   dTime = m_pTimeTextCtrl_Start->GetTimeValue();
   hr = (long)(dTime / 3600.0);
   min = (long)((dTime - (hr * 3600.0)) / 60.0);
   sec = (long)(dTime - (hr * 3600.0) - (min * 60.0));
   m_DateTime_Start.SetHour(hr);
   m_DateTime_Start.SetMinute(min);
   m_DateTime_Start.SetSecond(sec);

   m_DateTime_End = m_pDatePickerCtrl_End->GetValue();
   dTime = m_pTimeTextCtrl_End->GetTimeValue();
   hr = (long)(dTime / 3600.0);
   min = (long)((dTime - (hr * 3600.0)) / 60.0);
   sec = (long)(dTime - (hr * 3600.0) - (min * 60.0));
   m_DateTime_End.SetHour(hr);
   m_DateTime_End.SetMinute(min);
   m_DateTime_End.SetSecond(sec);

   m_TimeSpan_Duration = m_DateTime_End - m_DateTime_Start;

   return true;
}

// Update m_TimeSpan_Duration and ctrl based on m_DateTime_Start and m_DateTime_End.
void TimerRecordDialog::UpdateDuration()
{
   m_TimeSpan_Duration = m_DateTime_End - m_DateTime_Start;
   m_pTimeTextCtrl_Duration->SetTimeValue(m_TimeSpan_Duration.GetSeconds().ToDouble());
}

// Update m_DateTime_End and ctrls based on m_DateTime_Start and m_TimeSpan_Duration.
void TimerRecordDialog::UpdateEnd()
{
   //v Use remaining disk -> record time calcs from AudacityProject::OnTimer to set range?
   m_DateTime_End = m_DateTime_Start + m_TimeSpan_Duration;
   m_pDatePickerCtrl_End->SetValue(m_DateTime_End);
   m_pDatePickerCtrl_End->SetRange(m_DateTime_Start, wxInvalidDateTime); // No backdating.
   m_pDatePickerCtrl_End->Refresh();
   m_pTimeTextCtrl_End->SetTimeValue(wxDateTime_to_AudacityTime(m_DateTime_End));
}

int TimerRecordDialog::WaitForStart()
{
   wxString strMsg;
   /* i18n-hint: A time specification like "Sunday 28th October 2007 15:16:17 GMT"
    * but hopefully translated by wxwidgets will be inserted into this */
   strMsg.Printf(_("Waiting to start recording at %s.\n"),
                  GetDisplayDate(m_DateTime_Start).c_str());
   wxDateTime startWait_DateTime = wxDateTime::UNow();
   wxTimeSpan waitDuration = m_DateTime_Start - startWait_DateTime;
   TimerProgressDialog
      progress(waitDuration.GetMilliseconds().GetValue(),
               _("Audacity Timer Record - Waiting for Start"),
               strMsg,
               pdlgHideStopButton);

   int updateResult = eProgressSuccess;
   bool bIsRecording = false;
   while (updateResult == eProgressSuccess && !bIsRecording)
   {
      wxMilliSleep(10);
      updateResult = progress.Update();
      bIsRecording = (m_DateTime_Start <= wxDateTime::UNow());
   }
   return updateResult;
}
