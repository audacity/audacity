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

#include <wx/textctrl.h> // to inherit
#include <wx/timer.h> // member variable
#include "Export.h"
#include "wxPanelWrapper.h"
#include "BasicUI.h"

class wxCheckBox;
class wxChoice;
class wxDateEvent;
class wxDatePickerCtrl;
class wxTimerEvent;

class NumericTextCtrl;
class ShuttleGui;
class wxTextCtrlWrapper;

enum TimerRecordCompletedActions {
    TR_ACTION_NOTHING = 0x00000000,
    TR_ACTION_SAVED = 0x00000001,
    TR_ACTION_EXPORTED = 0x00000002
};

enum {
    POST_TIMER_RECORD_STOPPED = -3,
    POST_TIMER_RECORD_CANCEL_WAIT,
    POST_TIMER_RECORD_CANCEL,

    POST_TIMER_RECORD_NOTHING = 0,
    POST_TIMER_RECORD_CLOSE,

#ifdef __WINDOWS__
    POST_TIMER_RECORD_RESTART,
    POST_TIMER_RECORD_SHUTDOWN
#endif
};

class AudacityProject;

class TimerRecordDialog final : public wxDialogWrapper
{
public:
    using ProgressResult = BasicUI::ProgressResult;

    TimerRecordDialog(
        wxWindow* parent, AudacityProject& project, bool bAlreadySaved);
    ~TimerRecordDialog();

    void OnTimer(wxTimerEvent& event);
    ///Runs the wait for start dialog.  Returns false if the user clicks stop.
    int RunWaitDialog();

private:
    void OnDatePicker_Start(wxDateEvent& event);
    void OnTimeText_Start(wxCommandEvent& event);

    void OnDatePicker_End(wxDateEvent& event);
    void OnTimeText_End(wxCommandEvent& event);

    void OnTimeText_Duration(wxCommandEvent& event);

    void OnOK(wxCommandEvent& event);
    void OnHelpButtonClick(wxCommandEvent& event);

    TranslatableString GetDisplayDate(wxDateTime& dt);
    void PopulateOrExchange(ShuttleGui& S);

    bool TransferDataFromWindow() override;
    // no TransferDataFromWindow() because ??

    void UpdateDuration(); // Update m_TimeSpan_Duration and ctrl based on m_DateTime_Start and m_DateTime_End.
    void UpdateEnd(); // Update m_DateTime_End and ctrls based on m_DateTime_Start and m_TimeSpan_Duration.
    ProgressResult WaitForStart();

    // Timer Recording Automation Control Events
    void OnAutoSavePathButton_Click(wxCommandEvent& event);
    void OnAutoExportPathButton_Click(wxCommandEvent& event);
    void OnAutoSaveCheckBox_Change(wxCommandEvent& event);
    void OnAutoExportCheckBox_Change(wxCommandEvent& event);
    // Timer Recording Automation Routines
    void EnableDisableAutoControls(bool bEnable, int iControlGoup);
    void UpdateTextBoxControls();

    // Add Path Controls to Form
    wxTextCtrlWrapper* NewPathControl(
        wxWindow* wParent, const int iID, const TranslatableString& sCaption, const TranslatableString& sValue);

    int ExecutePostRecordActions(bool bWasStopped);
    ProgressResult PreActionDelay(int iActionIndex, TimerRecordCompletedActions eCompletedActions);

private:
    AudacityProject& mProject;

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

    // Controls for Auto Save/Export
    wxCheckBox* m_pTimerAutoSaveCheckBoxCtrl;
    wxTextCtrlWrapper* m_pTimerSavePathTextCtrl;
    wxButton* m_pTimerSavePathButtonCtrl;
    wxCheckBox* m_pTimerAutoExportCheckBoxCtrl;
    wxTextCtrlWrapper* m_pTimerExportPathTextCtrl;
    wxButton* m_pTimerExportPathButtonCtrl;

    // After Timer Record Options Choice
    wxChoice* m_pTimerAfterCompleteChoiceCtrl;

    // After Timer Record do we need to clean up?
    bool m_bProjectCleanupRequired;

    // Variables for the Auto Save/Export
    bool m_bAutoSaveEnabled;
    wxFileName m_fnAutoSaveFile;
    bool m_bAutoExportEnabled;
    wxFileName m_fnAutoExportFile;
    wxString m_sAutoExportFormat;
    int m_iAutoExportSampleRate{ 0 };
    int m_iAutoExportChannels{ 0 };
    ExportProcessor::Parameters m_AutoExportParameters;
    bool m_bProjectAlreadySaved;

    DECLARE_EVENT_TABLE()
};

#endif
