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
#include "TimerRecordDialog.h"
#include "FileNames.h"

#include <wx/defs.h>
#include <wx/datetime.h>
#include <wx/intl.h>
#include <wx/progdlg.h>
#include <wx/sizer.h>
#include <wx/string.h>
#include <wx/timer.h>
#include <wx/dynlib.h> //<! For windows.h
#include <wx/msgdlg.h>

#include "ShuttleGui.h"
#include "Project.h"
#include "Internat.h"
#include "Prefs.h"
#include "widgets/NumericTextCtrl.h"
#include "widgets/HelpSystem.h"

#define TIMER_ID 7000

enum { // control IDs
   ID_DATEPICKER_START = 10000,
   ID_TIMETEXT_START,
   ID_DATEPICKER_END,
   ID_TIMETEXT_END,
   ID_TIMETEXT_DURATION,
   ID_AUTOSAVEPATH_BUTTON,
   ID_AUTOSAVEPATH_TEXT,
   ID_AUTOEXPORTPATH_BUTTON,
   ID_AUTOEXPORTPATH_TEXT,
   ID_AUTOSAVE_CHECKBOX,
   ID_AUTOEXPORT_CHECKBOX
};

enum {
   CONTROL_GROUP_SAVE,
   CONTROL_GROUP_EXPORT
};

// Post Timer Recording Actions
// Ensure this matches the enum in Menus.cpp
enum {
   POST_TIMER_RECORD_STOPPED = -3,
   POST_TIMER_RECORD_CANCEL_WAIT,
   POST_TIMER_RECORD_CANCEL,
   POST_TIMER_RECORD_NOTHING,
   POST_TIMER_RECORD_CLOSE,
   POST_TIMER_RECORD_RESTART,
   POST_TIMER_RECORD_SHUTDOWN
};

const int kTimerInterval = 50; // ms

static double wxDateTime_to_AudacityTime(wxDateTime& dateTime)
{
   return (dateTime.GetHour() * 3600.0) + (dateTime.GetMinute() * 60.0) + dateTime.GetSecond();
};


// The purpose of the DatePickerCtrlAx class is to make to wxDatePickerCtrl more accessible for
// the NVDA screen reader.
// By default the msaa state of wxDatePickerCtrl is always normal (0x0), and this causes nvda not
// to read the control when the user tabs to it. This class
// modifies the state to be focusable + focused (when it's the focus).
// Note that even with this class NVDA still doesn't read the NEW selected part of the control when left/right
// arrow keys are used.

#if wxUSE_ACCESSIBILITY

class DatePickerCtrlAx final : public wxWindowAccessible
{
public:
   DatePickerCtrlAx(wxDatePickerCtrl * ctrl) : wxWindowAccessible(ctrl), mCtrl(ctrl) {};

   virtual ~ DatePickerCtrlAx() {};

   // Returns a state constant.
   wxAccStatus GetState(int childId, long *state) override;

private:
   wxDatePickerCtrl *mCtrl;
};

// Returns a state constant.
wxAccStatus DatePickerCtrlAx::GetState(int WXUNUSED(childId), long *state)
{
   *state = wxACC_STATE_SYSTEM_FOCUSABLE;
   *state |= (mCtrl == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0);

   return wxACC_OK;
}

#endif // wxUSE_ACCESSIBILITY


BEGIN_EVENT_TABLE(TimerRecordDialog, wxDialogWrapper)
   EVT_DATE_CHANGED(ID_DATEPICKER_START, TimerRecordDialog::OnDatePicker_Start)
   EVT_TEXT(ID_TIMETEXT_START, TimerRecordDialog::OnTimeText_Start)

   EVT_DATE_CHANGED(ID_DATEPICKER_END, TimerRecordDialog::OnDatePicker_End)
   EVT_TEXT(ID_TIMETEXT_END, TimerRecordDialog::OnTimeText_End)

   EVT_TEXT(ID_TIMETEXT_DURATION, TimerRecordDialog::OnTimeText_Duration)

   EVT_BUTTON(wxID_OK, TimerRecordDialog::OnOK)
   EVT_BUTTON(wxID_HELP, TimerRecordDialog::OnHelpButtonClick)

   EVT_TIMER(TIMER_ID, TimerRecordDialog::OnTimer)

   EVT_BUTTON(ID_AUTOSAVEPATH_BUTTON, TimerRecordDialog::OnAutoSavePathButton_Click)
   EVT_BUTTON(ID_AUTOEXPORTPATH_BUTTON, TimerRecordDialog::OnAutoExportPathButton_Click)

   EVT_CHECKBOX(ID_AUTOSAVE_CHECKBOX, TimerRecordDialog::OnAutoSaveCheckBox_Change)
   EVT_CHECKBOX(ID_AUTOEXPORT_CHECKBOX, TimerRecordDialog::OnAutoExportCheckBox_Change)

END_EVENT_TABLE()

TimerRecordDialog::TimerRecordDialog(wxWindow* parent, bool bAlreadySaved)
: wxDialogWrapper(parent, -1, _("Audacity Timer Record"), wxDefaultPosition,
           wxDefaultSize, wxCAPTION)
{
   SetName(GetTitle());

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

   // Do we allow the user to change the Automatic Save file?
   m_bProjectAlreadySaved = bAlreadySaved;

   ShuttleGui S(this, eIsCreating);
   this->PopulateOrExchange(S);

   // Set initial focus to "1" of "01h" in Duration NumericTextCtrl,
   // instead of OK button (default).
   m_pTimeTextCtrl_Duration->SetFocus();
   m_pTimeTextCtrl_Duration->SetFieldFocus(3);

   m_timer.SetOwner(this, TIMER_ID);
   m_timer.Start(kTimerInterval);

   // Do we need to tidy up when the timer recording has been completed?
   m_bProjectCleanupRequired = !(this->HaveFilesToRecover());

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
      m_pTimeTextCtrl_Start->SetValue(wxDateTime_to_AudacityTime(m_DateTime_Start));
      this->UpdateEnd(); // Keep Duration constant and update End for changed Start.
   }
}

void TimerRecordDialog::OnDatePicker_Start(wxDateEvent& WXUNUSED(event))
{
   m_DateTime_Start = m_pDatePickerCtrl_Start->GetValue();
   double dTime = m_pTimeTextCtrl_Start->GetValue();
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
   //v NumericTextCtrl doesn't implement upper ranges, i.e.,
   // if I tell it "024 h 060 m 060 s", then
   // user increments the hours past 23, it rolls over to 0
   // (although if you increment below 0, it stays at 0).
   // So instead, set the max to 99 and just catch hours > 24 and fix the ctrls.
   double dTime = m_pTimeTextCtrl_Start->GetValue();
   long days = (long)(dTime / (24.0 * 3600.0));
   if (days > 0) {
      dTime -= (double)days * 24.0 * 3600.0;
      m_DateTime_Start += wxTimeSpan::Days(days);
      m_pDatePickerCtrl_Start->SetValue(m_DateTime_Start);
      m_pTimeTextCtrl_Start->SetValue(dTime);
   }

   wxDateEvent dummyDateEvent;
   this->OnDatePicker_Start(dummyDateEvent);
}

void TimerRecordDialog::OnDatePicker_End(wxDateEvent& WXUNUSED(event))
{
   m_DateTime_End = m_pDatePickerCtrl_End->GetValue();
   double dTime = m_pTimeTextCtrl_End->GetValue();
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
      m_pTimeTextCtrl_End->SetValue(wxDateTime_to_AudacityTime(m_DateTime_End));
   }

   this->UpdateDuration(); // Keep Start constant and update Duration for changed End.
}

void TimerRecordDialog::OnTimeText_End(wxCommandEvent& WXUNUSED(event))
{
   //v NumericTextCtrl doesn't implement upper ranges, i.e.,
   // if I tell it "024 h 060 m 060 s", then
   // user increments the hours past 23, it rolls over to 0
   // (although if you increment below 0, it stays at 0).
   // So instead, set the max to 99 and just catch hours > 24 and fix the ctrls.
   double dTime = m_pTimeTextCtrl_End->GetValue();
   long days = (long)(dTime / (24.0 * 3600.0));
   if (days > 0) {
      dTime -= (double)days * 24.0 * 3600.0;
      m_DateTime_End += wxTimeSpan::Days(days);
      m_pDatePickerCtrl_End->SetValue(m_DateTime_End);
      m_pTimeTextCtrl_End->SetValue(dTime);
   }

   wxDateEvent dummyDateEvent;
   this->OnDatePicker_End(dummyDateEvent);
}

void TimerRecordDialog::OnTimeText_Duration(wxCommandEvent& WXUNUSED(event))
{
   double dTime = m_pTimeTextCtrl_Duration->GetValue();
   long hr = (long)(dTime / 3600.0);
   long min = (long)((dTime - (hr * 3600.0)) / 60.0);
   long sec = (long)(dTime - (hr * 3600.0) - (min * 60.0));
   m_TimeSpan_Duration = wxTimeSpan(hr, min, sec); //v milliseconds?

   this->UpdateEnd(); // Keep Start constant and update End for changed Duration.
}

// New events for timer recording automation
void TimerRecordDialog::OnAutoSavePathButton_Click(wxCommandEvent& WXUNUSED(event))
{
   wxString fName = FileSelector(_T("Save Timer Recording As"),
      m_fnAutoSaveFile.GetPath(),
      m_fnAutoSaveFile.GetFullName(),
      wxT("aup"),
      _("Audacity projects") + wxT(" (*.aup)|*.aup"),
      wxFD_SAVE | wxRESIZE_BORDER,
      this);

   if (fName == wxT(""))
      return;

   AudacityProject* pProject = GetActiveProject();

   // If project already exists then abort - we do not allow users to overwrite an existing project
   // unless it is the current project.
   if (wxFileExists(fName) && (pProject->GetFileName() != fName)) {
      wxMessageDialog m(
         NULL,
         _("The selected file name could not be used\nfor Timer Recording because it \
would overwrite another project.\nPlease try again and select an original name."),
         _("Error Saving Timer Recording Project"),
         wxOK|wxICON_ERROR);
      m.ShowModal();
      return;
   }

   // Set this boolean to false so we now do a SaveAs at the end of the recording
   // unless we're saving the current project.
   m_bProjectAlreadySaved = pProject->GetFileName() == fName? true : false;

   m_fnAutoSaveFile = fName;
   m_fnAutoSaveFile.SetExt(wxT("aup"));
   this->UpdateTextBoxControls();
}

void TimerRecordDialog::OnAutoExportPathButton_Click(wxCommandEvent& WXUNUSED(event))
{
   AudacityProject* pProject = GetActiveProject();
   Exporter eExporter;

   // Call the Exporter to set the options required
   if (eExporter.SetAutoExportOptions(pProject)) {
      // Populate the options so that we can destroy this instance of the Exporter
      m_fnAutoExportFile = eExporter.GetAutoExportFileName();
      m_iAutoExportFormat = eExporter.GetAutoExportFormat();
      m_iAutoExportSubFormat = eExporter.GetAutoExportSubFormat();
      m_iAutoExportFilterIndex = eExporter.GetAutoExportFilterIndex();

      // Update the text controls
      this->UpdateTextBoxControls();
   }
}

void TimerRecordDialog::OnAutoSaveCheckBox_Change(wxCommandEvent& WXUNUSED(event)) {
   EnableDisableAutoControls(m_pTimerAutoSaveCheckBoxCtrl->GetValue(), CONTROL_GROUP_SAVE);
}

void TimerRecordDialog::OnAutoExportCheckBox_Change(wxCommandEvent& WXUNUSED(event)) {
   EnableDisableAutoControls(m_pTimerAutoExportCheckBoxCtrl->GetValue(), CONTROL_GROUP_EXPORT);
}

void TimerRecordDialog::OnHelpButtonClick(wxCommandEvent& WXUNUSED(event))
{
   HelpSystem::ShowHelpDialog(this, wxT("Timer_Record"), true);
}

wxString TimerRecordDialog::GetHoursMinsString(int iMinutes) {
   
   wxString sFormatted = "";
   wxString sHours = "";
   wxString sMins = "";

   if (iMinutes < 1) {
      // Less than a minute...
      sFormatted = _("Less than 1 minute");
      return sFormatted;
   }

   // Calculate
   int iHours = iMinutes / 60;
   int iMins = iMinutes % 60;

   // Format the hours
   if (iHours == 1) {
      sHours = _("1 hour and ");
   }
   else if (iHours > 1) {
      sHours.Printf(_("%d hours and"), iHours);
   }

   // Format the minutes
   if (iMins == 1) {
      sMins = _("1 minute");
   }
   else if (iMins > 1) {
      sMins.Printf(_("%d minutes"), iMins);
   }

   // Build the string
   sFormatted.Printf("%s %s", sHours, sMins);
   return sFormatted;
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

   // Validate that we have a Save and/or Export path setup if the appropriate check box is ticked
   wxString sTemp = m_fnAutoSaveFile.GetFullPath();
   if (m_pTimerAutoSaveCheckBoxCtrl->IsChecked()) {
      if (!m_fnAutoSaveFile.IsOk() || m_fnAutoSaveFile.IsDir()) {
         wxMessageBox(_("Automatic Save path is invalid."),
            _("Error in Automatic Save"), wxICON_EXCLAMATION | wxOK);
         return;
      }
   }
   if (m_pTimerAutoExportCheckBoxCtrl->IsChecked()) {
      if (!m_fnAutoExportFile.IsOk() || m_fnAutoExportFile.IsDir()) {
         wxMessageBox(_("Automatic Export path is invalid."),
            _("Error in Automatic Export"), wxICON_EXCLAMATION | wxOK);
         return;
      }
   }

   // MY: Estimate here if we have enough disk space to
   // complete this Timer Recording.
   // If we dont think there is enough space then ask the user
   // if they want to continue.
   // We don't stop the user from starting the recording 
   // as its possible that they plan to free up some
   // space before the recording begins
   AudacityProject* pProject = GetActiveProject();

   // How many minutes do we have left on the disc?
   int iMinsLeft = pProject->GetEstimatedRecordingMinsLeftOnDisk();

   // How many minutes will this recording require?
   int iMinsRecording = m_TimeSpan_Duration.GetMinutes();

   // Do we have enough space?
   if (iMinsRecording >= iMinsLeft) {

      // Format the strings
      wxString sRemainingTime = "";
      sRemainingTime = GetHoursMinsString(iMinsLeft);
      wxString sPlannedTime = "";
      sPlannedTime = GetHoursMinsString(iMinsRecording);

      // Create the message string
      wxString sMessage = "";
      sMessage.Printf("You may not have enough free disk space to complete this Timer Recording, based on your current settings.\n\nDo you wish to continue?\n\nPlanned recording duration:   %s\nRecording time remaining on disk:   %s",
         sPlannedTime,
         sRemainingTime);

      wxMessageDialog dlgMessage(NULL,
         sMessage,
         _("Timer Recording Disk Space Warning"),
         wxYES_NO | wxNO_DEFAULT | wxICON_WARNING);
      if (dlgMessage.ShowModal() != wxID_YES) {
         // User decided not to continue - bail out!
         return;
      }
   }

   m_timer.Stop(); // Don't need to keep updating m_DateTime_Start to prevent backdating.
   this->EndModal(wxID_OK);
   wxLongLong duration = m_TimeSpan_Duration.GetSeconds();
   // this will assert if the duration won't fit in a long
   gPrefs->Write(wxT("/TimerRecord/LastDuration"), duration.ToLong());
   gPrefs->Flush();
}

void TimerRecordDialog::EnableDisableAutoControls(bool bEnable, int iControlGoup) {

   if (iControlGoup == CONTROL_GROUP_EXPORT) {
       m_pTimerExportPathTextCtrl->Enable( bEnable );
       m_pTimerExportPathButtonCtrl->Enable( bEnable);
   } else if (iControlGoup == CONTROL_GROUP_SAVE) {
       m_pTimerSavePathTextCtrl->Enable( bEnable);
       m_pTimerSavePathButtonCtrl->Enable(bEnable );
   }

   // Enable or disable the Choice box - if there is no Save or Export then this will be disabled
   if (m_pTimerAutoSaveCheckBoxCtrl->GetValue() || m_pTimerAutoExportCheckBoxCtrl->GetValue()) {
      m_pTimerAfterCompleteChoiceCtrl->Enable();
   } else {
      m_pTimerAfterCompleteChoiceCtrl->SetSelection(POST_TIMER_RECORD_NOTHING);
      m_pTimerAfterCompleteChoiceCtrl->Disable();
   }
}

void TimerRecordDialog::UpdateTextBoxControls() {
   // Will update the text box controls
   m_pTimerSavePathTextCtrl->SetValue(m_fnAutoSaveFile.GetFullPath());
   m_pTimerExportPathTextCtrl->SetValue(m_fnAutoExportFile.GetFullPath());

   // MY: Ensure we still display "Current Project" if this has already been saved
   if (m_bProjectAlreadySaved) {
      m_pTimerSavePathTextCtrl->SetValue(_("Current Project"));
   }
}

// Copied from AutoRecovery.cpp - for use with Timer Recording Improvements
bool TimerRecordDialog::HaveFilesToRecover()
{
   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened()) {
      wxMessageBox(_("Could not enumerate files in auto save directory."),
         _("Error"), wxICON_STOP);
      return false;
   }

   wxString filename;
   bool c = dir.GetFirst(&filename, wxT("*.autosave"), wxDIR_FILES);

   return c;
}

bool TimerRecordDialog::RemoveAllAutoSaveFiles()
{
   wxArrayString files;
   wxDir::GetAllFiles(FileNames::AutoSaveDir(), &files,
      wxT("*.autosave"), wxDIR_FILES);

   for (unsigned int i = 0; i < files.GetCount(); i++)
   {
      if (!wxRemoveFile(files[i]))
      {
         // I don't think this error message is actually useful.
         // -dmazzoni
         //wxMessageBox(wxT("Could not remove auto save file: " + files[i]),
         //             _("Error"), wxICON_STOP);
         return false;
      }
   }

   return true;
}

/// Runs the wait for start dialog.  Returns -1 if the user clicks stop while we are recording
/// or if the post recording actions fail.
int TimerRecordDialog::RunWaitDialog()
{
   AudacityProject* pProject = GetActiveProject();
   
   int updateResult = eProgressSuccess;

   if (m_DateTime_Start > wxDateTime::UNow())
      updateResult = this->WaitForStart();

   if (updateResult != eProgressSuccess)  {
      // Don't proceed, but don't treat it as canceled recording. User just canceled waiting.
      return POST_TIMER_RECORD_CANCEL_WAIT;
   } else {
      // Record for specified time.
      pProject->OnRecord();
      bool bIsRecording = true;

      wxString sPostAction = m_pTimerAfterCompleteChoiceCtrl->GetString(m_pTimerAfterCompleteChoiceCtrl->GetSelection());

      // Two column layout. Line spacing must match for both columns.
      // First column
      wxString strMsg = wxString::Format(_("Recording start:\n") +
                                         _("Duration:\n") +
                                         _("Recording end:\n\n") +
                                         _("Automatic Save enabled:\n") +
                                         _("Automatic Export enabled:\n") +
                                         _("Action after Timer Recording:"));

      strMsg += ProgressDialog::ColoumnSplitMarker;

      // Second column
      strMsg += wxString::Format(wxT("%s\n%s\n%s\n\n%s\n%s\n%s"),
                                       GetDisplayDate(m_DateTime_Start).c_str(),
                                       m_TimeSpan_Duration.Format(),
                                       GetDisplayDate(m_DateTime_End).c_str(),
                                       (m_bAutoSaveEnabled ? _("Yes") : _("No")),
                                       (m_bAutoExportEnabled ? _("Yes") : _("No")),
                                       sPostAction);

      TimerProgressDialog
         progress(m_TimeSpan_Duration.GetMilliseconds().GetValue(),
                  _("Audacity Timer Record Progress"),
                  strMsg,
                  pdlgHideCancelButton | pdlgConfirmStopCancel);

      // Make sure that start and end time are updated, so we always get the full
      // duration, even if there's some delay getting here.
      wxTimerEvent dummyTimerEvent;
      this->OnTimer(dummyTimerEvent);

      // Loop for progress display during recording.
      while (bIsRecording && (updateResult == eProgressSuccess)) {
         updateResult = progress.Update();
         wxMilliSleep(kTimerInterval);
         bIsRecording = (wxDateTime::UNow() <= m_DateTime_End); // Call UNow() again for extra accuracy...
      }
   }

   // Must do this AFTER the timer project dialog has been deleted to ensure the application
   // responds to the AUDIOIO events...see not about bug #334 in the ProgressDialog constructor.
   pProject->OnStop();

   // Let the caller handle cancellation or failure from recording progress.
   if (updateResult == eProgressCancelled || updateResult == eProgressFailed)
      return POST_TIMER_RECORD_CANCEL;

   return ExecutePostRecordActions((updateResult == eProgressStopped));
}

int TimerRecordDialog::ExecutePostRecordActions(bool bWasStopped) {
   // MY: We no longer automatically (and silently) call ->Save() when the 
   // timer recording is completed.  We can now Save and/or Export depending 
   // on the options selected by the user.
   // Once completed, we can also close Audacity, restart the system or
   // shutdown the system.
   // If there was any error with the auto save or export then we will not do
   // the actions requested and instead present an error mesasge to the user.
   // Finally, if there is no post-record action selected then we output
   // a dialog detailing what has been carried out instead.

   AudacityProject* pProject = GetActiveProject();

   bool bSaveOK = false;
   bool bExportOK = false;
   int iPostRecordAction = m_pTimerAfterCompleteChoiceCtrl->GetSelection();
   int iOverriddenAction = iPostRecordAction;
   bool bErrorOverride = false;

   // Do Automatic Save?
   if (m_bAutoSaveEnabled) {

      // MY: If this project has already been saved then simply execute a Save here
      if (m_bProjectAlreadySaved) {
         bSaveOK = pProject->Save();
      } else {
         bSaveOK = pProject->SaveFromTimerRecording(m_fnAutoSaveFile);
      }
   }

   // Do Automatic Export?
   if (m_bAutoExportEnabled) {
      bExportOK = pProject->ExportFromTimerRecording(m_fnAutoExportFile, m_iAutoExportFormat,
                                                     m_iAutoExportSubFormat, m_iAutoExportFilterIndex);
   }

   // Check if we need to override the post recording action
   bErrorOverride = ((m_bAutoSaveEnabled && !bSaveOK) || (m_bAutoExportEnabled && !bExportOK));
   if (bErrorOverride || bWasStopped) {
      iPostRecordAction = POST_TIMER_RECORD_NOTHING;
   }

   if (iPostRecordAction == POST_TIMER_RECORD_NOTHING) {
      // If there is no post-record action then we can show a message indicating what has been done

      wxString sMessage = (bWasStopped ? _("Timer Recording stopped.") :
                                         _("Timer Recording completed."));

      if (m_bAutoSaveEnabled) {
         if (bSaveOK) {
            sMessage.Printf("%s\n\nRecording saved: %s",
                            sMessage, m_fnAutoSaveFile.GetFullPath());
         } else {
            sMessage.Printf("%s\n\nError saving recording.", sMessage);
         }
      }
      if (m_bAutoExportEnabled) {
         if (bExportOK) {
            sMessage.Printf("%s\n\nRecording exported: %s",
                            sMessage, m_fnAutoExportFile.GetFullPath());
         } else {
            sMessage.Printf("%s\n\nError exporting recording.", sMessage);
         }
      }

      if (bErrorOverride) {

         if ((iOverriddenAction != iPostRecordAction) &&
             (iOverriddenAction != POST_TIMER_RECORD_NOTHING)) {
            // Inform the user that we have overridden the selected action
            sMessage.Printf("%s\n\n'%s' has been canceled due to the error(s) noted above.",
                            sMessage,
                            m_pTimerAfterCompleteChoiceCtrl->GetString(iOverriddenAction));
         }

         // Show Error Message Box
         wxMessageBox(sMessage, _("Error"), wxICON_EXCLAMATION | wxOK);
      } else {

         if (bWasStopped && (iOverriddenAction != POST_TIMER_RECORD_NOTHING)) {
            sMessage.Printf("%s\n\n'%s' has been canceled as the recording was stopped.",
                            sMessage,
                            m_pTimerAfterCompleteChoiceCtrl->GetString(iOverriddenAction));
         }

         wxMessageBox(sMessage, _("Timer Recording"), wxICON_INFORMATION | wxOK);
      }
   }

   // MY: Lets do some actions that only apply to Exit/Restart/Shutdown
   if (iPostRecordAction >= POST_TIMER_RECORD_CLOSE) {
      do {

         // Set the flags as appropriate based on what we have done
         wxUint32 eActionFlags = TR_ACTION_NOTHING;
         if (m_bAutoSaveEnabled && bSaveOK) {
            eActionFlags |= TR_ACTION_SAVED;
         }
         if (m_bAutoExportEnabled && bExportOK) {
            eActionFlags |= TR_ACTION_EXPORTED;
         }

         // Lets show a warning dialog telling the user what is about to happen.
         // If the user no longer wants to carry out this action then they can click
         // Cancel and we will do POST_TIMER_RECORD_NOTHING instead.
         int iDelayOutcome = PreActionDelay(iPostRecordAction, (TimerRecordCompletedActions)eActionFlags);
         if (iDelayOutcome != eProgressSuccess) {
            // Cancel the action!
            iPostRecordAction = POST_TIMER_RECORD_NOTHING;
            // Set this to true to avoid any chance of the temp files being deleted
            bErrorOverride = true;
            break;
         }

         // If we have simply recorded, exported and then plan to Exit/Restart/Shutdown
         // then we will have a temporary project setup.  Let's get rid of that!
         if (m_bAutoExportEnabled && !m_bAutoSaveEnabled) {
            DirManager::CleanTempDir();
         }
      } while (false);
   }

   // Do we need to cleanup the orphaned temporary project?
   if (m_bProjectCleanupRequired && !bErrorOverride) {
      RemoveAllAutoSaveFiles();
   }

   // Return the action as required
   return iPostRecordAction;
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

TimerRecordPathCtrl * TimerRecordDialog::NewPathControl(wxWindow *wParent, const int iID,
                                                        const wxString &sCaption, const wxString &sValue)
{
   TimerRecordPathCtrl * pTextCtrl;
   wxASSERT(wParent); // to justify safenew
   pTextCtrl = safenew TimerRecordPathCtrl(wParent, iID, sValue);
   pTextCtrl->SetName(sCaption);
   return pTextCtrl;
}

void TimerRecordDialog::PopulateOrExchange(ShuttleGui& S)
{
   bool bAutoSave = gPrefs->ReadBool("/TimerRecord/AutoSave", false);
   bool bAutoExport = gPrefs->ReadBool("/TimerRecord/AutoExport", false);
   int iPostTimerRecordAction = gPrefs->ReadLong("/TimerRecord/PostAction", 0);

   S.SetBorder(5);
   S.StartMultiColumn(2, wxCENTER);
   {
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
               safenew wxDatePickerCtrl(this, // wxWindow *parent,
               ID_DATEPICKER_START, // wxWindowID id,
               m_DateTime_Start); // const wxDateTime& dt = wxDefaultDateTime,
            // const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDP_DEFAULT | wxDP_SHOWCENTURY, const wxValidator& validator = wxDefaultValidator, const wxString& name = "datectrl")
            m_pDatePickerCtrl_Start->SetName(_("Start Date"));
            m_pDatePickerCtrl_Start->SetRange(wxDateTime::Today(), wxInvalidDateTime); // No backdating.
#if wxUSE_ACCESSIBILITY
            m_pDatePickerCtrl_Start->SetAccessible( safenew DatePickerCtrlAx(m_pDatePickerCtrl_Start));
#endif
            S.AddWindow(m_pDatePickerCtrl_Start);

            m_pTimeTextCtrl_Start = safenew NumericTextCtrl(
               NumericConverter::TIME, this, ID_TIMETEXT_START);
            m_pTimeTextCtrl_Start->SetName(_("Start Time"));
            m_pTimeTextCtrl_Start->SetFormatString(strFormat);
            m_pTimeTextCtrl_Start->
               SetValue(wxDateTime_to_AudacityTime(m_DateTime_Start));
            S.AddWindow(m_pTimeTextCtrl_Start);
            m_pTimeTextCtrl_Start->EnableMenu(false);
         }
         S.EndStatic();

         S.StartStatic(_("End Date and Time"), true);
         {
            m_pDatePickerCtrl_End =
               safenew wxDatePickerCtrl(this, // wxWindow *parent,
               ID_DATEPICKER_END, // wxWindowID id,
               m_DateTime_End); // const wxDateTime& dt = wxDefaultDateTime,
            // const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
            //                            long style = wxDP_DEFAULT | wxDP_SHOWCENTURY, 
            //                            const wxValidator& validator = wxDefaultValidator,
            //                            const wxString& name = "datectrl")
            m_pDatePickerCtrl_End->SetRange(m_DateTime_Start, wxInvalidDateTime); // No backdating.
            m_pDatePickerCtrl_End->SetName(_("End Date"));
#if wxUSE_ACCESSIBILITY
            m_pDatePickerCtrl_End->SetAccessible( safenew DatePickerCtrlAx(m_pDatePickerCtrl_End));
#endif
            S.AddWindow(m_pDatePickerCtrl_End);

            m_pTimeTextCtrl_End = safenew NumericTextCtrl(
               NumericConverter::TIME, this, ID_TIMETEXT_END);
            m_pTimeTextCtrl_End->SetName(_("End Time"));
            m_pTimeTextCtrl_End->SetFormatString(strFormat);
            m_pTimeTextCtrl_End->SetValue(wxDateTime_to_AudacityTime(m_DateTime_End));
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
            m_pTimeTextCtrl_Duration = safenew NumericTextCtrl(NumericConverter::TIME, this, ID_TIMETEXT_DURATION);
            m_pTimeTextCtrl_Duration->SetName(_("Duration"));
            m_pTimeTextCtrl_Duration->SetFormatString(strFormat1);
            m_pTimeTextCtrl_Duration->SetValue(m_TimeSpan_Duration.GetSeconds().ToDouble());
            S.AddWindow(m_pTimeTextCtrl_Duration);
            m_pTimeTextCtrl_Duration->EnableMenu(false);
         }
         S.EndStatic();
      }
      S.EndVerticalLay();

      S.StartVerticalLay(true);
      {
         S.StartStatic(_("Automatic Save"), true);
         {
            // If checked, the project will be saved when the recording is completed
            m_pTimerAutoSaveCheckBoxCtrl = S.Id(ID_AUTOSAVE_CHECKBOX).AddCheckBox(_("Enable &Automatic Save?"),
                                                                                    (bAutoSave ? "true" : "false"));
            S.StartMultiColumn(3, wxEXPAND);
            {
               wxString sInitialValue = wxT("");
               AudacityProject* pProject = GetActiveProject();
               wxString sSaveValue = pProject->GetFileName();
               if (sSaveValue != wxEmptyString) {
                  m_fnAutoSaveFile.Assign(sSaveValue);
                  sInitialValue = _("Current Project");
               }
               S.AddPrompt(_("Save Project As:"));
               m_pTimerSavePathTextCtrl = NewPathControl(this, ID_AUTOSAVEPATH_TEXT, _("Save Project As:"), sInitialValue);
               m_pTimerSavePathTextCtrl->SetEditable(false);
               S.AddWindow(m_pTimerSavePathTextCtrl);
               m_pTimerSavePathButtonCtrl = S.Id(ID_AUTOSAVEPATH_BUTTON).AddButton(_("Select..."));
               }
            S.EndMultiColumn();
         }
         S.EndStatic();

         S.StartStatic(_("Automatic Export"), true);
         {
            m_pTimerAutoExportCheckBoxCtrl = S.Id(ID_AUTOEXPORT_CHECKBOX).AddCheckBox(_("Enable Automatic &Export?"), (bAutoExport ? "true" : "false"));
            S.StartMultiColumn(3, wxEXPAND);
            {
               S.AddPrompt(_("Export Project As:"));
               m_pTimerExportPathTextCtrl = NewPathControl(this, ID_AUTOEXPORTPATH_TEXT, _("Export Project As:"), _(""));
               m_pTimerExportPathTextCtrl->SetEditable(false);
               S.AddWindow(m_pTimerExportPathTextCtrl);
               m_pTimerExportPathButtonCtrl = S.Id(ID_AUTOEXPORTPATH_BUTTON).AddButton(_("Select..."));
            }
            S.EndMultiColumn();
         }
         S.EndStatic();

         S.StartStatic(_("Options"), true);
         {

            wxArrayString arrayOptions;
            arrayOptions.Add(_("Do nothing"));
            arrayOptions.Add(_("Exit Audacity"));
            arrayOptions.Add(_("Restart system"));
            arrayOptions.Add(_("Shutdown system"));

            m_sTimerAfterCompleteOptionsArray.Add(arrayOptions.Item(0));
            m_sTimerAfterCompleteOptionsArray.Add(arrayOptions.Item(1));
#ifdef __WINDOWS__
            m_sTimerAfterCompleteOptionsArray.Add(arrayOptions.Item(2));
            m_sTimerAfterCompleteOptionsArray.Add(arrayOptions.Item(3));
#endif
            m_sTimerAfterCompleteOption = arrayOptions.Item(iPostTimerRecordAction);

            m_pTimerAfterCompleteChoiceCtrl = S.AddChoice(_("After Recording completes:"),
                                                          m_sTimerAfterCompleteOption,
                                                          &m_sTimerAfterCompleteOptionsArray);
         }
         S.EndStatic();

      }
      S.EndVerticalLay();
   }
   S.EndMultiColumn();

   // MY: Added the help button here
   S.AddStandardButtons(eOkButton | eCancelButton | eHelpButton);

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   EnableDisableAutoControls(bAutoSave, CONTROL_GROUP_SAVE);
   EnableDisableAutoControls(bAutoExport, CONTROL_GROUP_EXPORT);
}

bool TimerRecordDialog::TransferDataFromWindow()
{
   double dTime;
   long hr;
   long min;
   long sec;

   m_DateTime_Start = m_pDatePickerCtrl_Start->GetValue();
   dTime = m_pTimeTextCtrl_Start->GetValue();
   hr = (long)(dTime / 3600.0);
   min = (long)((dTime - (hr * 3600.0)) / 60.0);
   sec = (long)(dTime - (hr * 3600.0) - (min * 60.0));
   m_DateTime_Start.SetHour(hr);
   m_DateTime_Start.SetMinute(min);
   m_DateTime_Start.SetSecond(sec);

   m_DateTime_End = m_pDatePickerCtrl_End->GetValue();
   dTime = m_pTimeTextCtrl_End->GetValue();
   hr = (long)(dTime / 3600.0);
   min = (long)((dTime - (hr * 3600.0)) / 60.0);
   sec = (long)(dTime - (hr * 3600.0) - (min * 60.0));
   m_DateTime_End.SetHour(hr);
   m_DateTime_End.SetMinute(min);
   m_DateTime_End.SetSecond(sec);

   m_TimeSpan_Duration = m_DateTime_End - m_DateTime_Start;

   // Pull the settings from the auto save/export controls and write to the pref file
   m_bAutoSaveEnabled = m_pTimerAutoSaveCheckBoxCtrl->GetValue();
   m_bAutoExportEnabled = m_pTimerAutoExportCheckBoxCtrl->GetValue();

   // MY: Obtain the index from the choice control so we can save to the prefs file
   int iPostRecordAction = m_pTimerAfterCompleteChoiceCtrl->GetSelection();

   // Save the options back to the prefs file
   gPrefs->Write("/TimerRecord/AutoSave", m_bAutoSaveEnabled);
   gPrefs->Write("/TimerRecord/AutoExport", m_bAutoExportEnabled);
   gPrefs->Write("/TimerRecord/PostAction", iPostRecordAction);

   return true;
}

// Update m_TimeSpan_Duration and ctrl based on m_DateTime_Start and m_DateTime_End.
void TimerRecordDialog::UpdateDuration()
{
   m_TimeSpan_Duration = m_DateTime_End - m_DateTime_Start;
   m_pTimeTextCtrl_Duration->SetValue(m_TimeSpan_Duration.GetSeconds().ToDouble());
}

// Update m_DateTime_End and ctrls based on m_DateTime_Start and m_TimeSpan_Duration.
void TimerRecordDialog::UpdateEnd()
{
   //v Use remaining disk -> record time calcs from AudacityProject::OnTimer to set range?
   m_DateTime_End = m_DateTime_Start + m_TimeSpan_Duration;
   m_pDatePickerCtrl_End->SetValue(m_DateTime_End);
   m_pDatePickerCtrl_End->SetRange(m_DateTime_Start, wxInvalidDateTime); // No backdating.
   m_pDatePickerCtrl_End->Refresh();
   m_pTimeTextCtrl_End->SetValue(wxDateTime_to_AudacityTime(m_DateTime_End));
}

int TimerRecordDialog::WaitForStart()
{
   // MY: The Waiting For Start dialog now shows what actions will occur after recording has completed
   wxString sPostAction = m_pTimerAfterCompleteChoiceCtrl->GetString(m_pTimerAfterCompleteChoiceCtrl->GetSelection());

   // Two column layout. Line spacing must match for both columns.
   // First column
   wxString strMsg = wxString::Format(_("Waiting to start recording at:\n") +
                                      _("Recording duration:\n") +
                                      _("Scheduled to stop at:\n\n") +
                                      _("Automatic Save enabled:\n") +
                                      _("Automatic Export enabled:\n") +
                                      _("Action after Timer Recording:"));

   strMsg += ProgressDialog::ColoumnSplitMarker;

   // Second column
   strMsg += wxString::Format(wxT("%s\n%s\n%s\n\n%s\n%s\n%s"),
                                    GetDisplayDate(m_DateTime_Start).c_str(),
                                    m_TimeSpan_Duration.Format(),
                                    GetDisplayDate(m_DateTime_End).c_str(),
                                    (m_bAutoSaveEnabled ? _("Yes") : _("No")),
                                    (m_bAutoExportEnabled ? _("Yes") : _("No")),
                                    sPostAction);

   wxDateTime startWait_DateTime = wxDateTime::UNow();
   wxTimeSpan waitDuration = m_DateTime_Start - startWait_DateTime;
   TimerProgressDialog progress(waitDuration.GetMilliseconds().GetValue(),
      _("Audacity Timer Record - Waiting for Start"),
      strMsg,
      pdlgHideStopButton | pdlgConfirmStopCancel | pdlgHideElapsedTime,
      _("Recording will commence in:"));

   int updateResult = eProgressSuccess;
   bool bIsRecording = false;
   while (updateResult == eProgressSuccess && !bIsRecording)
   {
      updateResult = progress.Update();
      wxMilliSleep(10);
      bIsRecording = (m_DateTime_Start <= wxDateTime::UNow());
   }
   return updateResult;
}

int TimerRecordDialog::PreActionDelay(int iActionIndex, TimerRecordCompletedActions eCompletedActions)
{
   wxString sAction = m_pTimerAfterCompleteChoiceCtrl->GetString(iActionIndex);
   wxString sCountdownLabel;
   sCountdownLabel.Printf("%s in:", sAction);

   // Two column layout. Line spacing must match for both columns.
   // First column
   wxString strMsg = wxString::Format(_("Timer Recording completed.\n\n") +
                                      _("Recording Saved:\n") +
                                      _("Recording Exported:\n") +
                                      _("Action after Timer Recording:"));

   strMsg += ProgressDialog::ColoumnSplitMarker;

   // Second column
   strMsg +=  wxString::Format(wxT("\n\n%s\n%s\n%s"),
                                    ((eCompletedActions & TR_ACTION_SAVED) ? _("Yes") : _("No")),
                                    ((eCompletedActions & TR_ACTION_EXPORTED) ? _("Yes") : _("No")),
                                    sAction);

   wxDateTime dtNow = wxDateTime::UNow();
   wxTimeSpan tsWait = wxTimeSpan(0, 1, 0, 0);
   wxDateTime dtActionTime = dtNow.Add(tsWait);

   TimerProgressDialog dlgAction(tsWait.GetMilliseconds().GetValue(),
                          _("Audacity Timer Record - Waiting"),
                          strMsg,
                          pdlgHideStopButton | pdlgHideElapsedTime,
                          sCountdownLabel);

   int iUpdateResult = eProgressSuccess;
   bool bIsTime = false;
   while (iUpdateResult == eProgressSuccess && !bIsTime)
   {
      iUpdateResult = dlgAction.Update();
      wxMilliSleep(10);
      bIsTime = (dtActionTime <= wxDateTime::UNow());
   }
   return iUpdateResult;
}
