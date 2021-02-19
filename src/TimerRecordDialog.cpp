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


#include "TimerRecordDialog.h"

#include "FileNames.h"

#include <thread>
#include <wx/setup.h> // for wxUSE_* macros

#include <wx/app.h>
#include <wx/wxcrtvararg.h>
#include <wx/button.h>
#include <wx/calctrl.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/defs.h>
#include <wx/dir.h>
#include <wx/datectrl.h>
#include <wx/datetime.h>
#include <wx/dynlib.h> //<! For windows.h

#include "AudioIO.h"
#include "SelectFile.h"
#include "ShuttleGui.h"
#include "ProjectAudioManager.h"
#include "ProjectFileIO.h"
#include "ProjectFileManager.h"
#include "ProjectManager.h"
#include "ProjectRate.h"
#include "Prefs.h"
#include "Track.h"
#include "widgets/NumericTextCtrl.h"
#include "widgets/HelpSystem.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/ProgressDialog.h"
#include "wxTextCtrlWrapper.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

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

// The slow timer interval is used to update the start and end times, which only show
// time to the nearest second.  So we only need an update once a second.
const int kSlowTimerInterval = 1000; // ms

// This timer interval is used in some busy-wait loops and is much shorter.
constexpr auto kTimerInterval = std::chrono::milliseconds{50};

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

class DatePickerCtrlAx final : public WindowAccessible
{
public:
   DatePickerCtrlAx(wxDatePickerCtrl * ctrl) : WindowAccessible(ctrl), mCtrl(ctrl) {};

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

TimerRecordDialog::TimerRecordDialog(
   wxWindow* parent, AudacityProject &project, bool bAlreadySaved)
: wxDialogWrapper(parent, -1, XO("Audacity Timer Record"), wxDefaultPosition,
           wxDefaultSize, wxCAPTION)
, mProject{ project }
{
   SetName();

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
   m_timer.Start(kSlowTimerInterval);
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
   auto &projectFileIO = ProjectFileIO::Get(mProject);

   wxString fName = SelectFile(FileNames::Operation::Export,
      XO("Save Timer Recording As"),
      m_fnAutoSaveFile.GetPath(),
      m_fnAutoSaveFile.GetFullName(),
      wxT("aup3"),
      { FileNames::AudacityProjects },
      wxFD_SAVE | wxRESIZE_BORDER,
      this);

   if (fName.empty())
      return;

   // If project already exists then abort - we do not allow users to overwrite an existing project
   // unless it is the current project.
   if (wxFileExists(fName) && (projectFileIO.GetFileName() != fName)) {
      AudacityMessageDialog m(
         nullptr,
         XO("The selected file name could not be used\nfor Timer Recording because it \
would overwrite another project.\nPlease try again and select an original name."),
         XO("Error Saving Timer Recording Project"),
         wxOK|wxICON_ERROR );
      m.ShowModal();
      return;
   }

   // Set this boolean to false so we now do a SaveAs at the end of the recording
   // unless we're saving the current project.
   m_bProjectAlreadySaved = projectFileIO.GetFileName() == fName? true : false;

   m_fnAutoSaveFile = fName;
   m_fnAutoSaveFile.SetExt(wxT("aup3"));
   this->UpdateTextBoxControls();
}

void TimerRecordDialog::OnAutoExportPathButton_Click(wxCommandEvent& WXUNUSED(event))
{
   Exporter eExporter{ mProject };

   // Call the Exporter to set the options required
   if (eExporter.SetAutoExportOptions()) {
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
   HelpSystem::ShowHelp(this, L"Timer_Record", true);
}

void TimerRecordDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
   this->TransferDataFromWindow();
   if (!m_TimeSpan_Duration.IsPositive())
   {
      AudacityMessageBox(
         XO("Duration is zero. Nothing will be recorded."),
         XO("Error in Duration"),
         wxICON_EXCLAMATION | wxOK);
      return;
   }

   // Validate that we have a Save and/or Export path setup if the appropriate check box is ticked
   wxString sTemp = m_fnAutoSaveFile.GetFullPath();
   if (m_pTimerAutoSaveCheckBoxCtrl->IsChecked()) {
      if (!m_fnAutoSaveFile.IsOk() || m_fnAutoSaveFile.IsDir()) {
         AudacityMessageBox(
            XO("Automatic Save path is invalid."),
            XO("Error in Automatic Save"),
            wxICON_EXCLAMATION | wxOK);
         return;
      }
   }
   if (m_pTimerAutoExportCheckBoxCtrl->IsChecked()) {
      if (!m_fnAutoExportFile.IsOk() || m_fnAutoExportFile.IsDir()) {
         AudacityMessageBox(
            XO("Automatic Export path is invalid."),
            XO("Error in Automatic Export"),
            wxICON_EXCLAMATION | wxOK);
         return;
      }
   }

   // MY: Estimate here if we have enough disk space to
   // complete this Timer Recording.
   // If we don't think there is enough space then ask the user
   // if they want to continue.
   // We don't stop the user from starting the recording 
   // as its possible that they plan to free up some
   // space before the recording begins
   auto &projectManager = ProjectManager::Get( mProject );

   // How many minutes do we have left on the disc?
   int iMinsLeft = projectManager.GetEstimatedRecordingMinsLeftOnDisk();

   // How many minutes will this recording require?
   int iMinsRecording = m_TimeSpan_Duration.GetMinutes();

   // Do we have enough space?
   if (iMinsRecording >= iMinsLeft) {

      // Format the strings
      auto sRemainingTime = projectManager.GetHoursMinsString(iMinsLeft);
      auto sPlannedTime = projectManager.GetHoursMinsString(iMinsRecording);

      // Create the message string
      auto sMessage = XO(
"You may not have enough free disk space to complete this Timer Recording, based on your current settings.\n\nDo you wish to continue?\n\nPlanned recording duration:   %s\nRecording time remaining on disk:   %s")
         .Format( sPlannedTime, sRemainingTime );

      AudacityMessageDialog dlgMessage(
         nullptr,
         sMessage,
         XO("Timer Recording Disk Space Warning"),
         wxYES_NO | wxNO_DEFAULT | wxICON_WARNING);
      if (dlgMessage.ShowModal() != wxID_YES ) {
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

/// Runs the wait for start dialog.  Returns -1 if the user clicks stop while we are recording
/// or if the post recording actions fail.
int TimerRecordDialog::RunWaitDialog()
{
   auto updateResult = ProgressResult::Success;

   const auto gAudioIO = AudioIO::Get();
   gAudioIO->DelayActions(true);
   {
      auto cleanup = finally([gAudioIO]{ gAudioIO->DelayActions(false); });

      if (m_DateTime_Start > wxDateTime::UNow())
         updateResult = this->WaitForStart();

      if (updateResult != ProgressResult::Success)  {
         // Don't proceed, but don't treat it as canceled recording. User just canceled waiting.
         return POST_TIMER_RECORD_CANCEL_WAIT;
      } else {
         // Record for specified time.
         ProjectAudioManager::Get( mProject ).OnRecord(false);
         bool bIsRecording = true;

         auto sPostAction = Verbatim(
            m_pTimerAfterCompleteChoiceCtrl->GetStringSelection() );

         // Two column layout.
         TimerProgressDialog::MessageTable columns{
            {
               XO("Recording start:") ,
               XO("Duration:") ,
               XO("Recording end:") ,
               {} ,
               XO("Automatic Save enabled:") ,
               XO("Automatic Export enabled:") ,
               XO("Action after Timer Recording:") ,
            },
            {
               GetDisplayDate(m_DateTime_Start) ,
               Verbatim( m_TimeSpan_Duration.Format() ),
               GetDisplayDate(m_DateTime_End) ,
               {} ,
               (m_bAutoSaveEnabled ? XO("Yes") : XO("No")) ,
               (m_bAutoExportEnabled ? XO("Yes") : XO("No")) ,
               sPostAction ,
            }
         };

         TimerProgressDialog
            progress(m_TimeSpan_Duration.GetMilliseconds().GetValue(),
                     XO("Audacity Timer Record Progress"),
                     columns,
                     pdlgHideCancelButton | pdlgConfirmStopCancel);

         // Make sure that start and end time are updated, so we always get the full
         // duration, even if there's some delay getting here.
         wxTimerEvent dummyTimerEvent;
         this->OnTimer(dummyTimerEvent);

         // Loop for progress display during recording.
         while (bIsRecording && (updateResult == ProgressResult::Success)) {
            updateResult = progress.UpdateProgress();
            using namespace std::chrono;
            std::this_thread::sleep_for(kTimerInterval);
            bIsRecording = (wxDateTime::UNow() <= m_DateTime_End); // Call UNow() again for extra accuracy...
         }
      }
   }

   // Must do this AFTER the timer project dialog has been deleted to ensure the application
   // responds to the AUDIOIO events...see not about bug #334 in the ProgressDialog constructor.
   ProjectAudioManager::Get( mProject ).Stop();

   // Let the caller handle cancellation or failure from recording progress.
   if (updateResult == ProgressResult::Cancelled || updateResult == ProgressResult::Failed)
      return POST_TIMER_RECORD_CANCEL;

   return ExecutePostRecordActions((updateResult == ProgressResult::Stopped));
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

   bool bSaveOK = false;
   bool bExportOK = false;
   int iPostRecordAction = m_pTimerAfterCompleteChoiceCtrl->GetSelection();
   int iOverriddenAction = iPostRecordAction;
   bool bErrorOverride = false;

   // Do Automatic Save?
   if (m_bAutoSaveEnabled) {

      auto &projectFileManager = ProjectFileManager::Get( mProject );
      // MY: If this project has already been saved then simply execute a Save here
      if (m_bProjectAlreadySaved) {
         bSaveOK = projectFileManager.Save();
      } else {
         bSaveOK = projectFileManager.SaveFromTimerRecording(m_fnAutoSaveFile);
      }
   }

   // Do Automatic Export?
   if (m_bAutoExportEnabled) {
      Exporter e{ mProject };
      bExportOK = e.ProcessFromTimerRecording(
         false, 0.0, TrackList::Get( mProject ).GetEndTime(),
            m_fnAutoExportFile, m_iAutoExportFormat,
            m_iAutoExportSubFormat, m_iAutoExportFilterIndex);
   }

   // Check if we need to override the post recording action
   bErrorOverride = ((m_bAutoSaveEnabled && !bSaveOK) || (m_bAutoExportEnabled && !bExportOK));
   if (bErrorOverride || bWasStopped) {
      iPostRecordAction = POST_TIMER_RECORD_NOTHING;
   }

   if (iPostRecordAction == POST_TIMER_RECORD_NOTHING) {
      // If there is no post-record action then we can show a message indicating what has been done

      auto sMessage = (bWasStopped ? XO("Timer Recording stopped.") :
                                         XO("Timer Recording completed."));

      if (m_bAutoSaveEnabled) {
         if (bSaveOK) {
            sMessage = XO("%s\n\nRecording saved: %s").Format(
                            sMessage, m_fnAutoSaveFile.GetFullPath());
         } else {
            sMessage = XO("%s\n\nError saving recording.").Format( sMessage );
         }
      }
      if (m_bAutoExportEnabled) {
         if (bExportOK) {
            sMessage = XO("%s\n\nRecording exported: %s").Format(
                            sMessage, m_fnAutoExportFile.GetFullPath());
         } else {
            sMessage = XO("%s\n\nError exporting recording.").Format( sMessage );
         }
      }

      if (bErrorOverride) {

         if ((iOverriddenAction != iPostRecordAction) &&
             (iOverriddenAction != POST_TIMER_RECORD_NOTHING)) {
            // Inform the user that we have overridden the selected action
            sMessage = XO("%s\n\n'%s' has been canceled due to the error(s) noted above.").Format(
                            sMessage,
                            m_pTimerAfterCompleteChoiceCtrl->GetString(iOverriddenAction));
         }

         // Show Error Message Box
         AudacityMessageBox(
            sMessage,
            XO("Error"),
            wxICON_EXCLAMATION | wxOK);
      } else {

         if (bWasStopped && (iOverriddenAction != POST_TIMER_RECORD_NOTHING)) {
            sMessage = XO("%s\n\n'%s' has been canceled as the recording was stopped.").Format(
                            sMessage,
                            m_pTimerAfterCompleteChoiceCtrl->GetString(iOverriddenAction));
         }

         AudacityMessageBox(
            sMessage,
            XO("Timer Recording"),
            wxICON_INFORMATION | wxOK);
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
         auto iDelayOutcome = PreActionDelay(iPostRecordAction, (TimerRecordCompletedActions)eActionFlags);
         if (iDelayOutcome != ProgressResult::Success) {
            // Cancel the action!
            iPostRecordAction = POST_TIMER_RECORD_NOTHING;
            break;
         }
      } while (false);
   }

   // Return the action as required
   return iPostRecordAction;
}

TranslatableString TimerRecordDialog::GetDisplayDate( wxDateTime & dt )
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
         return Verbatim( s );
      }
   }
#endif

   // Use default formatting
wxPrintf(wxT("%s\n"), dt.Format());
   return Verbatim( dt.FormatDate() + wxT(" ") + dt.FormatTime() );
}

wxTextCtrlWrapper * TimerRecordDialog::NewPathControl(
   wxWindow *wParent, const int iID,
   const TranslatableString &sCaption, const TranslatableString &sValue)
{
   wxTextCtrlWrapper * pTextCtrl;
   wxASSERT(wParent); // to justify safenew
   pTextCtrl = safenew wxTextCtrlWrapper(wParent, iID, sValue.Translation());
   pTextCtrl->SetName(sCaption.Translation());
   return pTextCtrl;
}

void TimerRecordDialog::PopulateOrExchange(ShuttleGui& S)
{
   bool bAutoSave = gPrefs->ReadBool("/TimerRecord/AutoSave", false);
   bool bAutoExport = gPrefs->ReadBool("/TimerRecord/AutoExport", false);
   int iPostTimerRecordAction = gPrefs->ReadLong("/TimerRecord/PostAction", 0);

   S.SetBorder(5);
   using Options = NumericTextCtrl::Options;
   /* i18n-hint a format string for hours, minutes, and seconds */
   auto strFormat = XO("099 h 060 m 060 s");
   /* i18n-hint a format string for days, hours, minutes, and seconds */
   auto strFormat1 = XO("099 days 024 h 060 m 060 s");

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
         S.StartStatic(XO("Start Date and Time"), true);
         {
            m_pDatePickerCtrl_Start =
               safenew wxDatePickerCtrl(S.GetParent(), // wxWindow *parent,
               ID_DATEPICKER_START, // wxWindowID id,
               m_DateTime_Start); // const wxDateTime& dt = wxDefaultDateTime,
            // const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDP_DEFAULT | wxDP_SHOWCENTURY, const wxValidator& validator = wxDefaultValidator, const wxString& name = "datectrl")
            m_pDatePickerCtrl_Start->SetRange(wxDateTime::Today(), wxInvalidDateTime); // No backdating.
#if wxUSE_ACCESSIBILITY
            m_pDatePickerCtrl_Start->SetAccessible( safenew DatePickerCtrlAx(m_pDatePickerCtrl_Start));
#endif
            S.Name(XO("Start Date"))
               .AddWindow(m_pDatePickerCtrl_Start);

            m_pTimeTextCtrl_Start = safenew NumericTextCtrl(
               S.GetParent(), ID_TIMETEXT_START, NumericConverter::TIME,
               {}, 0, 44100,
               Options{}
                  .MenuEnabled(false)
                  .Format(strFormat)
                  .Value(true, wxDateTime_to_AudacityTime(m_DateTime_Start)));
            S.Name(XO("Start Time"))
               .AddWindow(m_pTimeTextCtrl_Start);
         }
         S.EndStatic();

         S.StartStatic(XO("End Date and Time"), true);
         {
            m_pDatePickerCtrl_End =
               safenew wxDatePickerCtrl(S.GetParent(), // wxWindow *parent,
               ID_DATEPICKER_END, // wxWindowID id,
               m_DateTime_End); // const wxDateTime& dt = wxDefaultDateTime,
            // const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
            //                            long style = wxDP_DEFAULT | wxDP_SHOWCENTURY, 
            //                            const wxValidator& validator = wxDefaultValidator,
            //                            const wxString& name = "datectrl")
            m_pDatePickerCtrl_End->SetRange(m_DateTime_Start, wxInvalidDateTime); // No backdating.
#if wxUSE_ACCESSIBILITY
            m_pDatePickerCtrl_End->SetAccessible( safenew DatePickerCtrlAx(m_pDatePickerCtrl_End));
#endif
            S.Name(XO("End Date"))
               .AddWindow(m_pDatePickerCtrl_End);

            m_pTimeTextCtrl_End = safenew NumericTextCtrl(
               S.GetParent(), ID_TIMETEXT_END, NumericConverter::TIME,
               {}, 0, 44100,
               Options{}
                  .MenuEnabled(false)
                  .Format(strFormat)
                  .Value(true, wxDateTime_to_AudacityTime(m_DateTime_End)));
            S.Name(XO("End Time"))
               .AddWindow(m_pTimeTextCtrl_End);
         }
         S.EndStatic();

         S.StartStatic(XO("Duration"), true);
         {
            m_pTimeTextCtrl_Duration = safenew NumericTextCtrl(
               S.GetParent(), ID_TIMETEXT_DURATION, NumericConverter::TIME,
               {}, 0, 44100,
               Options{}
                  .MenuEnabled(false)
                  .Format(strFormat1)
                  .Value(true, m_TimeSpan_Duration.GetSeconds().ToDouble()));
            /* i18n-hint: This string is used to configure the controls which shows the recording
            * duration. As such it is important that only the alphabetic parts of the string
            * are translated, with the numbers left exactly as they are.
            * The string 'days' indicates that the first number in the control will be the number of days,
            * then the 'h' indicates the second number displayed is hours, the 'm' indicates the third
            * number displayed is minutes, and the 's' indicates that the fourth number displayed is
            * seconds.
            */
            S.Name(XO("Duration"))
               .AddWindow(m_pTimeTextCtrl_Duration);
         }
         S.EndStatic();
      }
      S.EndVerticalLay();

      S.StartVerticalLay(true);
      {
         S.StartStatic(XO("Automatic Save"), true);
         {
            // If checked, the project will be saved when the recording is completed
            m_pTimerAutoSaveCheckBoxCtrl = S.Id(ID_AUTOSAVE_CHECKBOX).AddCheckBox(XXO("Enable &Automatic Save?"),
                                                                                    bAutoSave);
            S.StartMultiColumn(3, wxEXPAND);
            {
               TranslatableString sInitialValue;
               auto sSaveValue = ProjectFileIO::Get(mProject).GetFileName();
               if (!sSaveValue.empty()) {
                  m_fnAutoSaveFile.Assign(sSaveValue);
                  sInitialValue = XO("Current Project");
               }
               S.AddPrompt(XXO("Save Project As:"));
               m_pTimerSavePathTextCtrl = NewPathControl(
                  S.GetParent(), ID_AUTOSAVEPATH_TEXT,
                  XO("Save Project As:"), sInitialValue);
               m_pTimerSavePathTextCtrl->SetReadOnly(true);
               S.AddWindow(m_pTimerSavePathTextCtrl);
               m_pTimerSavePathButtonCtrl = S.Id(ID_AUTOSAVEPATH_BUTTON).AddButton(XXO("Select..."));
               }
            S.EndMultiColumn();
         }
         S.EndStatic();

         S.StartStatic(XO("Automatic Export"), true);
         {
            m_pTimerAutoExportCheckBoxCtrl = S.Id(ID_AUTOEXPORT_CHECKBOX).AddCheckBox(XXO("Enable Automatic &Export?"), bAutoExport);
            S.StartMultiColumn(3, wxEXPAND);
            {
               S.AddPrompt(XXO("Export Project As:"));
               m_pTimerExportPathTextCtrl = NewPathControl(
                  S.GetParent(), ID_AUTOEXPORTPATH_TEXT,
                  XO("Export Project As:"), {});
               m_pTimerExportPathTextCtrl->SetReadOnly(true);
               S.AddWindow(m_pTimerExportPathTextCtrl);
               m_pTimerExportPathButtonCtrl = S.Id(ID_AUTOEXPORTPATH_BUTTON).AddButton(XXO("Select..."));
            }
            S.EndMultiColumn();
         }
         S.EndStatic();

         S.StartStatic(XO("Options"), true);
         {

            S.StartMultiColumn(1, wxEXPAND);
            {
               S.SetStretchyCol( 0 );
               m_pTimerAfterCompleteChoiceCtrl = S.AddChoice(XXO("After Recording completes:"),
                     {
                        XO("Do nothing") ,
                        XO("Exit Audacity") ,
                  #ifdef __WINDOWS__
                        XO("Restart system") ,
                        XO("Shutdown system") ,
                  #endif
                     },
                     iPostTimerRecordAction
               );
            }
            S.EndMultiColumn();
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
   //wxLogDebug( "Time start %s end %s", 
   //   m_DateTime_Start.FormatISOCombined(' '),
   //   m_DateTime_End.FormatISOCombined(' ') );

   // Disable the range limitation (to fix Bug 1749 and 1978)
   // Otherwise SetVallue asserts when going back in time.
   m_pDatePickerCtrl_End->SetRange(wxInvalidDateTime, wxInvalidDateTime); 
   m_pDatePickerCtrl_End->SetValue(m_DateTime_End);
   // Re-enable range limitation to constrain user input.
   m_pDatePickerCtrl_End->SetRange(m_DateTime_Start, wxInvalidDateTime); // No backdating.
   m_pDatePickerCtrl_End->Refresh();
   m_pTimeTextCtrl_End->SetValue(wxDateTime_to_AudacityTime(m_DateTime_End));
}

ProgressResult TimerRecordDialog::WaitForStart()
{
   // MY: The Waiting For Start dialog now shows what actions will occur after recording has completed
   auto sPostAction = Verbatim(
         m_pTimerAfterCompleteChoiceCtrl->GetStringSelection() );

   // Two column layout.
   TimerProgressDialog::MessageTable columns{
      {
         XO("Waiting to start recording at:") ,
         XO("Recording duration:") ,
         XO("Scheduled to stop at:") ,
         {} ,
         XO("Automatic Save enabled:") ,
         XO("Automatic Export enabled:") ,
         XO("Action after Timer Recording:") ,
      },
      {
         GetDisplayDate(m_DateTime_Start),
         Verbatim( m_TimeSpan_Duration.Format() ),
         GetDisplayDate(m_DateTime_End) ,
         {} ,
         (m_bAutoSaveEnabled ? XO("Yes") : XO("No")) ,
         (m_bAutoExportEnabled ? XO("Yes") : XO("No")) ,
         sPostAction ,
      },
   };

   wxDateTime startWait_DateTime = wxDateTime::UNow();
   wxTimeSpan waitDuration = m_DateTime_Start - startWait_DateTime;
   TimerProgressDialog progress(waitDuration.GetMilliseconds().GetValue(),
      XO("Audacity Timer Record - Waiting for Start"),
      columns,
      pdlgHideStopButton | pdlgConfirmStopCancel | pdlgHideElapsedTime,
      /* i18n-hint: "in" means after a duration of time,
         which is shown below this string */
      XO("Recording will commence in:"));

   auto updateResult = ProgressResult::Success;
   bool bIsRecording = false;
   while (updateResult == ProgressResult::Success && !bIsRecording)
   {
      updateResult = progress.UpdateProgress();
      using namespace std::chrono;
      std::this_thread::sleep_for(kTimerInterval);
      bIsRecording = (m_DateTime_Start <= wxDateTime::UNow());
   }
   return updateResult;
}

ProgressResult TimerRecordDialog::PreActionDelay(int iActionIndex, TimerRecordCompletedActions eCompletedActions)
{
   auto sAction = Verbatim( m_pTimerAfterCompleteChoiceCtrl
      ->GetString(iActionIndex) );

   /* i18n-hint: %s is one of "Do nothing", "Exit Audacity", "Restart system",
      or "Shutdown system", and
      "in" means after a duration of time, shown below this string */
   auto sCountdownLabel = XO("%s in:").Format( sAction );

   // Two column layout.
   TimerProgressDialog::MessageTable columns{
      {
         XO("Timer Recording completed.") ,
         {} ,
         XO("Recording Saved:") ,
         XO("Recording Exported:") ,
         XO("Action after Timer Recording:") ,
      },
      {
         {} ,
         {} ,
         ((eCompletedActions & TR_ACTION_SAVED) ? XO("Yes") : XO("No")) ,
         ((eCompletedActions & TR_ACTION_EXPORTED) ? XO("Yes") : XO("No")) ,
         sAction ,
      },
   };


   wxDateTime dtNow = wxDateTime::UNow();
   wxTimeSpan tsWait = wxTimeSpan(0, 1, 0, 0);
   wxDateTime dtActionTime = dtNow.Add(tsWait);

   TimerProgressDialog dlgAction(tsWait.GetMilliseconds().GetValue(),
                          XO("Audacity Timer Record - Waiting"),
                          columns,
                          pdlgHideStopButton | pdlgHideElapsedTime,
                          sCountdownLabel);

   auto iUpdateResult = ProgressResult::Success;
   bool bIsTime = false;
   while (iUpdateResult == ProgressResult::Success && !bIsTime)
   {
      iUpdateResult = dlgAction.UpdateProgress();
      using namespace std::chrono;
      std::this_thread::sleep_for(kTimerInterval);
      bIsTime = (dtActionTime <= wxDateTime::UNow());
   }
   return iUpdateResult;
}

// Register a menu item

#include "commands/CommandContext.h"
#include "commands/CommandManager.h"
#include "CommonCommandFlags.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "ProjectWindow.h"
#include "UndoManager.h"

namespace {
void OnTimerRecord(const CommandContext &context)
{
   auto &project = context.project;
   const auto &settings = ProjectSettings::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   // MY: Due to improvements in how Timer Recording saves and/or exports
   // it is now safer to disable Timer Recording when there is more than
   // one open project.
   if (AllProjects{}.size() > 1) {
      AudacityMessageBox(
         XO(
"Timer Recording cannot be used with more than one open project.\n\nPlease close any additional projects and try again."),
         XO("Timer Recording"),
         wxICON_INFORMATION | wxOK);
      return;
   }

   // MY: If the project has unsaved changes then we no longer allow access
   // to Timer Recording.  This decision has been taken as the safest approach
   // preventing issues surrounding "dirty" projects when Automatic Save/Export
   // is used in Timer Recording.
   if ((undoManager.UnsavedChanges()) &&
       (TrackList::Get( project ).Any() || settings.EmptyCanBeDirty())) {
      AudacityMessageBox(
         XO(
"Timer Recording cannot be used while you have unsaved changes.\n\nPlease save or close this project and try again."),
         XO("Timer Recording"),
         wxICON_INFORMATION | wxOK);
      return;
   }

   // We check the selected tracks to see if there is enough of them to accommodate
   // all input channels and all of them have the same sampling rate.
   // Those checks will be later performed by recording function anyway,
   // but we want to warn the user about potential problems from the very start.
   const auto selectedTracks{ GetPropertiesOfSelected(project) };
   const int rateOfSelected{ selectedTracks.rateOfSelected };
   const int numberOfSelected{ selectedTracks.numberOfSelected };
   const bool allSameRate{ selectedTracks.allSameRate };

   if (!allSameRate) {
      AudacityMessageBox(XO("The tracks selected "
         "for recording must all have the same sampling rate"),
         XO("Mismatched Sampling Rates"),
         wxICON_ERROR | wxCENTRE);

      return;
   }

   const auto existingTracks{ ProjectAudioManager::ChooseExistingRecordingTracks(project, true, rateOfSelected) };
   if (existingTracks.empty()) {
      if (numberOfSelected > 0 && rateOfSelected !=
          ProjectRate::Get(project).GetRate()) {
         AudacityMessageBox(XO(
            "Too few tracks are selected for recording at this sample rate.\n"
            "(Audacity requires two channels at the same sample rate for\n"
            "each stereo track)"),
            XO("Too Few Compatible Tracks Selected"),
            wxICON_ERROR | wxCENTRE);

         return;
      }
   }
   
   // We use this variable to display "Current Project" in the Timer Recording
   // save project field
   bool bProjectSaved = !ProjectFileIO::Get( project ).IsModified();

   //we break the prompting and waiting dialogs into two sections
   //because they both give the user a chance to click cancel
   //and therefore remove the newly inserted track.

   TimerRecordDialog dialog(
      &window, project, bProjectSaved); /* parent, project, project saved? */
   int modalResult = dialog.ShowModal();
   if (modalResult == wxID_CANCEL)
   {
      // Cancelled before recording - don't need to do anything.
   }
   else
   {
      // Bug #2382
      // Allow recording to start at current cursor position.
      #if 0
      // Timer Record should not record into a selection.
      bool bPreferNewTrack;
      gPrefs->Read("/GUI/PreferNewTrackRecord",&bPreferNewTrack, false);
      if (bPreferNewTrack) {
         window.Rewind(false);
      } else {
         window.SkipEnd(false);
      }
      #endif

      int iTimerRecordingOutcome = dialog.RunWaitDialog();
      switch (iTimerRecordingOutcome) {
      case POST_TIMER_RECORD_CANCEL_WAIT:
         // Canceled on the wait dialog
         ProjectHistory::Get( project ).RollbackState();
         break;
      case POST_TIMER_RECORD_CANCEL:
         // RunWaitDialog() shows the "wait for start" as well as "recording"
         // dialog if it returned POST_TIMER_RECORD_CANCEL it means the user
         // cancelled while the recording, so throw out the fresh track.
         // However, we can't undo it here because the PushState() is called in TrackPanel::OnTimer(),
         // which is blocked by this function.
         // so instead we mark a flag to undo it there.
         ProjectAudioManager::Get( project ).SetTimerRecordCancelled();
         break;
      case POST_TIMER_RECORD_NOTHING:
         // No action required
         break;
      case POST_TIMER_RECORD_CLOSE:
         wxTheApp->CallAfter( []{
            // Simulate the application Exit menu item
            wxCommandEvent evt{ wxEVT_MENU, wxID_EXIT };
            wxTheApp->AddPendingEvent( evt );
         } );
         ProjectManager::Get(project).SetSkipSavePrompt(true);
         break;

#ifdef __WINDOWS__
      case POST_TIMER_RECORD_RESTART:
         // Restart System
         ProjectManager::Get(project).SetSkipSavePrompt(true);
         system("shutdown /r /f /t 30");
         break;
      case POST_TIMER_RECORD_SHUTDOWN:
         // Shutdown System
         ProjectManager::Get(project).SetSkipSavePrompt(true);
         system("shutdown /s /f /t 30");
         break;
#endif
      }
   }
}

const auto CanStopFlags = AudioIONotBusyFlag() | CanStopAudioStreamFlag();

using namespace MenuTable;
AttachedItem sAttachment{
   { wxT("Transport/Basic/Record"),
      { OrderingHint::After, wxT("Record2ndChoice") } },
   Command( wxT("TimerRecord"), XXO("&Timer Record..."),
      OnTimerRecord, CanStopFlags, wxT("Shift+T") )
};

}
