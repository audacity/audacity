/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2008 Audacity Team.
   License: GPL v2.  See License.txt.

   ScoreAlignDialog.cpp
   <TODO: authors>

******************************************************************//**

\class ScoreAlignDialog
\brief ScoreAlignDialog is \TODO.

It \TODO: description

*//*******************************************************************/

#include "../Audacity.h"
#include "../Experimental.h"

#ifdef EXPERIMENTAL_SCOREALIGN

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/brush.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/image.h>
#include <wx/dcmemory.h>
#include <wx/msgdlg.h>
#include <wx/file.h>
#include <wx/filedlg.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/statusbr.h>
#endif

#include <fstream>
#include <wx/dialog.h>
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "allegro.h"
#include "audioreader.h"
#include "scorealign.h"
#include "scorealign-glue.h"
#include "ScoreAlignDialog.h"

static std::unique_ptr<ScoreAlignDialog> gScoreAlignDialog{};

//IMPLEMENT_CLASS(ScoreAlignDialog, wxDialogWrapper)

ScoreAlignDialog::ScoreAlignDialog(ScoreAlignParams &params)
   : wxDialogWrapper(NULL, -1, _("Align MIDI to Audio"),
              wxDefaultPosition, wxDefaultSize,
              wxDEFAULT_DIALOG_STYLE)
{
   gScoreAlignDialog.reset(this); // Allows anyone to close dialog by calling
                             // CloseScoreAlignDialog()
   gPrefs->Read(wxT("/Tracks/Synchronize/FramePeriod"), &p.mFramePeriod,
                float(SA_DFT_FRAME_PERIOD));
   gPrefs->Read(wxT("/Tracks/Synchronize/WindowSize"), &p.mWindowSize,
                float(SA_DFT_WINDOW_SIZE));
   gPrefs->Read(wxT("/Tracks/Synchronize/SilenceThreshold"),
                &p.mSilenceThreshold, float(SA_DFT_SILENCE_THRESHOLD));
   gPrefs->Read(wxT("/Tracks/Synchronize/ForceFinalAlignment"),
                &p.mForceFinalAlignment, float(SA_DFT_FORCE_FINAL_ALIGNMENT));
   gPrefs->Read(wxT("/Tracks/Synchronize/IgnoreSilence"),
                &p.mIgnoreSilence, float(SA_DFT_IGNORE_SILENCE));
   gPrefs->Read(wxT("/Tracks/Synchronize/PresmoothTime"), &p.mPresmoothTime,
                float(SA_DFT_PRESMOOTH_TIME));
   gPrefs->Read(wxT("/Tracks/Synchronize/LineTime"), &p.mLineTime,
                float(SA_DFT_LINE_TIME));
   gPrefs->Read(wxT("/Tracks/Synchronize/SmoothTime"), &p.mSmoothTime,
                float(SA_DFT_SMOOTH_TIME));

   //wxButton *ok = safenew wxButton(this, wxID_OK, _("OK"));
   //wxButton *cancel = safenew wxButton(this, wxID_CANCEL, _("Cancel"));
   //wxSlider *sl = safenew wxSlider(this, ID_SLIDER, 0, 0, 100,
   //                     wxDefaultPosition, wxSize(20, 124),
   //                     wxSL_HORIZONTAL);

   ShuttleGui S(this, eIsCreating);
   //ok->SetDefault();

   S.SetBorder(5);
   S.StartVerticalLay(true);
   S.StartStatic(wxT("Align MIDI to Audio"));
   S.StartMultiColumn(3, wxEXPAND | wxALIGN_CENTER_VERTICAL);
   S.SetStretchyCol(1);

   mFramePeriodLabel = S.AddVariableText(_("Frame Period:"), true,
                                      wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
   S.SetStyle(wxSL_HORIZONTAL);
   mFramePeriodSlider = S.Id(ID_FRAMEPERIOD).AddSlider(wxT(""),
       /*pos*/ (int) (p.mFramePeriod * 100 + 0.5), /*max*/ 50, /*min*/ 5);
   S.SetSizeHints(300, -1);
   mFramePeriodSlider->SetName(_("Frame Period"));
   mFramePeriodText = S.AddVariableText(SA_DFT_FRAME_PERIOD_TEXT, true,
                                      wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   mWindowSizeLabel = S.AddVariableText(_("Window Size:"), true,
                                      wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
   S.SetStyle(wxSL_HORIZONTAL);
   mWindowSizeSlider = S.Id(ID_WINDOWSIZE).AddSlider(wxT(""),
       /*pos*/ (int) (p.mWindowSize * 100 + 0.5), /*max*/ 100, /*min*/ 5);
   mWindowSizeSlider->SetName(_("Window Size"));
   mWindowSizeText = S.AddVariableText(SA_DFT_WINDOW_SIZE_TEXT, true,
                                      wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   mForceFinalAlignmentCheckBox = S.Id(ID_FORCEFINALALIGNMENT).AddCheckBox(
                wxT("Force Final Alignment"),
                (p.mForceFinalAlignment ? wxT("true") : wxT("false")));
   mForceFinalAlignmentCheckBox->SetName(_("Force Final Alignment"));
   mIgnoreSilenceCheckBox = S.Id(ID_IGNORESILENCE).AddCheckBox(
                              wxT("Ignore Silence at Beginnings and Endings"),
                              (p.mIgnoreSilence ? wxT("true") : wxT("false")));
   mIgnoreSilenceCheckBox->SetName(
                     _("Ignore Silence at Beginnings and Endings"));
   // need a third column after checkboxes:
   S.AddVariableText(wxT(""), true, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);

   mSilenceThresholdLabel = S.AddVariableText(_("Silence Threshold:"),
                             true, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
   S.SetStyle(wxSL_HORIZONTAL);
   mSilenceThresholdSlider = S.Id(ID_SILENCETHRESHOLD).AddSlider(wxT(""),
         /*pos*/ (int) (p.mSilenceThreshold * 1000 + 0.5), /*max*/ 500);
   mSilenceThresholdSlider->SetName(_("Silence Threshold"));
   mSilenceThresholdText = S.AddVariableText(SA_DFT_SILENCE_THRESHOLD_TEXT,
                              true, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   /* i18n-hint: The English would be clearer if it had 'Duration' rather than 'Time'
      This is a NEW experimental effect, and until we have it documented in the user
      manual we don't have a clear description of what this parameter does.
      It is OK to leave it in English. */
   mPresmoothLabel = S.AddVariableText(_("Presmooth Time:"), true,
                                      wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
   S.SetStyle(wxSL_HORIZONTAL);
   mPresmoothSlider = S.Id(ID_PRESMOOTH).AddSlider(wxT(""),
               /*pos*/ (int) (p.mPresmoothTime * 100 + 0.5), /*max*/ 500);
   mPresmoothSlider->SetName(_("Presmooth Time"));
   mPresmoothText = S.AddVariableText(SA_DFT_PRESMOOTH_TIME_TEXT, true,
                                      wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   /* i18n-hint: The English would be clearer if it had 'Duration' rather than 'Time'
      This is a NEW experimental effect, and until we have it documented in the user
      manual we don't have a clear description of what this parameter does.
      It is OK to leave it in English. */
   mLineTimeLabel = S.AddVariableText(_("Line Time:"), true,
                                      wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
   S.SetStyle(wxSL_HORIZONTAL);
   mLineTimeSlider = S.Id(ID_LINETIME).AddSlider(wxT(""),
                    /*pos*/ (int) (p.mLineTime * 100 + 0.5), /*max*/ 500);
   mLineTimeSlider->SetName(_("Line Time"));
   mLineTimeText = S.AddVariableText(SA_DFT_LINE_TIME_TEXT, true,
                                      wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   /* i18n-hint: The English would be clearer if it had 'Duration' rather than 'Time'
      This is a NEW experimental effect, and until we have it documented in the user
      manual we don't have a clear description of what this parameter does.
      It is OK to leave it in English. */
   mSmoothTimeLabel = S.AddVariableText(_("Smooth Time:"), true,
                                      wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
   S.SetStyle(wxSL_HORIZONTAL);
   mSmoothTimeSlider = S.Id(ID_SMOOTHTIME).AddSlider(wxT(""),
                  /*pos*/ (int) (p.mSmoothTime * 100 + 0.5), /*max*/ 500);
   mSmoothTimeSlider->SetName(_("Smooth Time"));
   mSmoothTimeText = S.AddVariableText(SA_DFT_SMOOTH_TIME_TEXT, true,
                                      wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   S.EndMultiColumn();
   S.EndStatic();

   mDefaultButton = safenew wxButton(this, ID_DEFAULT, _("Use Defaults"));
   mDefaultButton->SetName(_("Restore Defaults"));

   S.AddStandardButtons(eOkButton | eCancelButton, mDefaultButton);
   S.EndVerticalLay();
   Layout();
   Fit();
   Center();

   TransferDataFromWindow(); // set labels according to actual initial values

   params.mStatus = p.mStatus = ShowModal();

   if (p.mStatus == wxID_OK) {
       // Retain the settings
       gPrefs->Write(wxT("/Tracks/Synchronize/FramePeriod"), p.mFramePeriod);
       gPrefs->Write(wxT("/Tracks/Synchronize/WindowSize"), p.mWindowSize);
       gPrefs->Write(wxT("/Tracks/Synchronize/SilenceThreshold"),
                     p.mSilenceThreshold);
       gPrefs->Write(wxT("/Tracks/Synchronize/ForceFinalAlignment"),
                     p.mForceFinalAlignment);
       gPrefs->Write(wxT("/Tracks/Synchronize/IgnoreSilence"),
                     p.mIgnoreSilence);
       gPrefs->Write(wxT("/Tracks/Synchronize/PresmoothTime"),
                     p.mPresmoothTime);
       gPrefs->Write(wxT("/Tracks/Synchronize/LineTime"), p.mLineTime);
       gPrefs->Write(wxT("/Tracks/Synchronize/SmoothTime"), p.mSmoothTime);
       gPrefs->Flush();

       params = p; // return all parameters through params
   }
}

ScoreAlignDialog::~ScoreAlignDialog()
{
}


//void ScoreAlignDialog::OnOK(wxCommandEvent & event)
//{
//   EndModal(wxID_OK);
//}

//void ScoreAlignDialog::OnCancel(wxCommandEvent & event)
//{
//   EndModal(wxID_CANCEL);
//}

void ScoreAlignDialog::OnSlider(wxCommandEvent & event)
{
   TransferDataFromWindow();
}


void ScoreAlignDialog::OnDefault(wxCommandEvent & event)
{
   mFramePeriodSlider->SetValue((int) (SA_DFT_FRAME_PERIOD * 100 + 0.5));
   mWindowSizeSlider->SetValue((int) (SA_DFT_WINDOW_SIZE * 100 + 0.5));
   mSilenceThresholdSlider->SetValue(
                          (int) (SA_DFT_SILENCE_THRESHOLD * 1000 + 0.5));
   mForceFinalAlignmentCheckBox->SetValue(SA_DFT_FORCE_FINAL_ALIGNMENT);
   mIgnoreSilenceCheckBox->SetValue(SA_DFT_IGNORE_SILENCE);
   mPresmoothSlider->SetValue((int) (SA_DFT_PRESMOOTH_TIME * 100 + 0.5));
   mLineTimeSlider->SetValue((int) (SA_DFT_LINE_TIME * 100 + 0.5));
   mSmoothTimeSlider->SetValue((int) (SA_DFT_SMOOTH_TIME * 100 + 0.5));

   TransferDataFromWindow();
}


bool ScoreAlignDialog::TransferDataFromWindow()
{
   p.mFramePeriod = (double) mFramePeriodSlider->GetValue() / 100.0;
   p.mWindowSize = (double) mWindowSizeSlider->GetValue() / 100.0;
   p.mSilenceThreshold = (double) mSilenceThresholdSlider->GetValue() / 1000.0;
   p.mForceFinalAlignment = (double) mForceFinalAlignmentCheckBox->GetValue();
   p.mIgnoreSilence = (double) mIgnoreSilenceCheckBox->GetValue();
   p.mPresmoothTime = (double) mPresmoothSlider->GetValue() / 100.0;
   p.mLineTime = (double) mLineTimeSlider->GetValue() / 100.0;
   p.mSmoothTime = (double) mSmoothTimeSlider->GetValue() / 100.0;

   mFramePeriodText->SetLabel(wxString::Format(_("%.2f secs"),
                                               p.mFramePeriod));
   mWindowSizeText->SetLabel(wxString::Format(_("%.2f secs"), p.mWindowSize));
   mSilenceThresholdText->SetLabel(wxString::Format(_("%.3f"),
                                                    p.mSilenceThreshold));
   mPresmoothText->SetLabel(p.mPresmoothTime > 0 ?
                            wxString::Format(_("%.2f secs"),
                                             p.mPresmoothTime) : wxT("(off)"));
   mLineTimeText->SetLabel(p.mLineTime > 0 ?
                           wxString::Format(_("%.2f secs"), p.mLineTime) :
                           wxT("(off)"));
   mSmoothTimeText->SetLabel(wxString::Format(_("%.2f secs"), p.mSmoothTime));
   return true;
}


void CloseScoreAlignDialog()
{
   gScoreAlignDialog.reset();
}


BEGIN_EVENT_TABLE(ScoreAlignDialog, wxDialogWrapper)
//   EVT_BUTTON(wxID_OK, ScoreAlignDialog::OnOK)
//   EVT_BUTTON(wxID_CANCEL, ScoreAlignDialog::OnCancel)
   EVT_BUTTON(ID_DEFAULT, ScoreAlignDialog::OnDefault)
   EVT_SLIDER(ID_FRAMEPERIOD, ScoreAlignDialog::OnSlider)
   EVT_SLIDER(ID_WINDOWSIZE, ScoreAlignDialog::OnSlider)
   EVT_SLIDER(ID_SILENCETHRESHOLD, ScoreAlignDialog::OnSlider)
   EVT_CHECKBOX(ID_FORCEFINALALIGNMENT, ScoreAlignDialog::OnSlider)
   EVT_CHECKBOX(ID_IGNORESILENCE, ScoreAlignDialog::OnSlider)
   EVT_SLIDER(ID_PRESMOOTH, ScoreAlignDialog::OnSlider)
   EVT_SLIDER(ID_LINETIME, ScoreAlignDialog::OnSlider)
   EVT_SLIDER(ID_SMOOTHTIME, ScoreAlignDialog::OnSlider)
END_EVENT_TABLE()

#endif
