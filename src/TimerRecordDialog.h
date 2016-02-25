/**********************************************************************

  Audacity: A Digital Audio Editor

  TimerRecordDialog.h

  Copyright 2006 by Vaughan Johnson

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

**********************************************************************/

#ifndef __AUDACITY_TIMERRECORD_DIALOG__
#define __AUDACITY_TIMERRECORD_DIALOG__

#include <wx/dialog.h>
#include <wx/datectrl.h>
#include <wx/calctrl.h>
#include <wx/timer.h>
#include <wx/checkbox.h>
#include "export/Export.h"

class wxTimerEvent;

class NumericTextCtrl;
class ShuttleGui;

class TimerRecordDialog final : public wxDialog
{
public:
   TimerRecordDialog(wxWindow* parent);
   ~TimerRecordDialog();

   void OnTimer(wxTimerEvent& event);
   ///Runs the wait for start dialog.  Returns false if the user clicks stop.
   bool RunWaitDialog();

private:
   void OnDatePicker_Start(wxDateEvent& event);
   void OnTimeText_Start(wxCommandEvent& event);

   void OnDatePicker_End(wxDateEvent& event);
   void OnTimeText_End(wxCommandEvent& event);

   void OnTimeText_Duration(wxCommandEvent & event);

   void OnOK(wxCommandEvent& event);

   wxString GetDisplayDate(wxDateTime & dt);
   void PopulateOrExchange(ShuttleGui& S);
   bool TransferDataFromWindow();
   void UpdateDuration(); // Update m_TimeSpan_Duration and ctrl based on m_DateTime_Start and m_DateTime_End.
   void UpdateEnd(); // Update m_DateTime_End and ctrls based on m_DateTime_Start and m_TimeSpan_Duration.
   int WaitForStart();

   // Timer Recording Automation Control Events
   void OnAutoSavePathButton_Click(wxCommandEvent& event);
   void OnAutoExportPathButton_Click(wxCommandEvent& event);
   void OnAutoSaveCheckBox_Change(wxCommandEvent& event);
   void OnAutoExportCheckBox_Change(wxCommandEvent& event);
   // Timer Recording Automation Routines
   void EnableDisableAutoControls(bool bEnable, int iControlGoup);
   void UpdateTextBoxControls();

private:
   wxDateTime m_DateTime_Start;
   wxDateTime m_DateTime_End;
   wxTimeSpan m_TimeSpan_Duration;

   // controls
   wxDatePickerCtrl* m_pDatePickerCtrl_Start;
   NumericTextCtrl* m_pTimeTextCtrl_Start;

   wxDatePickerCtrl* m_pDatePickerCtrl_End;
   NumericTextCtrl* m_pTimeTextCtrl_End;

   NumericTextCtrl* m_pTimeTextCtrl_Duration;

   wxTimer m_timer;

   // New Controls for Auto Save/Export
   wxCheckBox *m_pTimerAutoSaveCheckBoxCtrl;
   wxTextCtrl *m_pTimerSavePathTextCtrl;
   wxButton *m_pTimerSavePathButtonCntrl;
   wxCheckBox *m_pTimerAutoExportCheckBoxCtrl;
   wxTextCtrl *m_pTimerExportPathTextCtrl;
   wxButton *m_pTimerExportPathButtonCtrl;

   // New variables for the Auto Save/Export
   bool m_bAutoSaveEnabled;
   wxFileName m_fnAutoSaveFile;
   bool m_bAutoExportEnabled;
   wxFileName m_fnAutoExportFile;
   int m_iAutoExportFormat;
   int m_iAutoExportSubFormat;
   int m_iAutoExportFilterIndex;

   DECLARE_EVENT_TABLE();
};

#endif
