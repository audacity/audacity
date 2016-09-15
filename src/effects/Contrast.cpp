/**********************************************************************

  Audacity: A Digital Audio Editor

  Contrast.cpp

\class ContrastDialog
\brief Dialog used for Contrast menu item

*//*******************************************************************/

#include "../Audacity.h"
#include "Contrast.h"

#include "../AudacityApp.h"

#include "../WaveTrack.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "../FileNames.h"
#include "../widgets/LinkingHtmlWindow.h"
#include "../widgets/HelpSystem.h"
#include "../widgets/NumericTextCtrl.h"
#include "../lib-src/FileDialog/FileDialog.h"

#include <cmath>
#include <limits>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

#include <wx/valtext.h>
#include <wx/log.h>

#include "../PlatformCompatibility.h"

#define DB_MAX_LIMIT 0.0   // Audio is massively distorted.
#define WCAG2_PASS 20.0    // dB difference required to pass WCAG2 test.

bool ContrastDialog::GetDB(float &dB)
{
   float rms = float(0.0);
   int numberSelecteTracks = 0;

   // For stereo tracks: sqrt((mean(L)+mean(R))/2)
   bool isStereo = false;
   double meanSq = 0.0;

   AudacityProject *p = GetActiveProject();
   SelectedTrackListOfKindIterator iter(Track::Wave, p->GetTracks());
   WaveTrack *t = (WaveTrack *) iter.First();
   while (t) {
      numberSelecteTracks++;
      if (numberSelecteTracks > 1 && !isStereo) {
         wxMessageDialog m(NULL, _("You can only measure one track at a time."), _("Error"), wxOK);
         m.ShowModal();
         return false;
      }
      isStereo = t->GetLinked();

      wxASSERT(mT0 <= mT1);

      // Ignore whitespace beyond ends of track.
      if(mT0 < t->GetStartTime())
         mT0 = t->GetStartTime();
      if(mT1 > t->GetEndTime())
         mT1 = t->GetEndTime();

      auto SelT0 = t->TimeToLongSamples(mT0);
      auto SelT1 = t->TimeToLongSamples(mT1);

      if(SelT0 > SelT1)
      {
         wxMessageDialog m(NULL, _("Invalid audio selection.\nPlease ensure that audio is selected."), _("Error"), wxOK);
         m.ShowModal();
         return false;
      }

      if(SelT0 == SelT1)
      {
         wxMessageDialog m(NULL, _("Nothing to measure.\nPlease select a section of a track."), _("Error"), wxOK);
         m.ShowModal();
         return false;
      }

      ((WaveTrack *)t)->GetRMS(&rms, mT0, mT1);
      meanSq += rms * rms;
      t = (WaveTrack *) iter.Next();
   }
   // TODO: This works for stereo, provided the audio clips are in both channels.
   // We should really count gaps between clips as silence.
   rms = (meanSq > 0.0)? sqrt(meanSq/(double)numberSelecteTracks) : 0.0;

   if(numberSelecteTracks == 0) {
      wxMessageDialog m(NULL, _("Please select an audio track."), _("Error"), wxOK);
      m.ShowModal();
      return false;
   }

   dB = (rms == 0.0)? -INFINITY : LINEAR_TO_DB(rms);
   return true;
}

void ContrastDialog::SetStartAndEndTime()
{
   AudacityProject *p = GetActiveProject();
   mT0 = p->mViewInfo.selectedRegion.t0();
   mT1 = p->mViewInfo.selectedRegion.t1();
}


// WDR: class implementations

//----------------------------------------------------------------------------
// ContrastDialog
//----------------------------------------------------------------------------

// WDR: event table for ContrastDialog

enum {
   ID_BUTTON_USECURRENTF = 10001,
   ID_BUTTON_USECURRENTB,
   ID_BUTTON_GETURL,
   ID_BUTTON_EXPORT,
   ID_BUTTON_RESET,
   ID_BUTTON_CLOSE,
   ID_FOREGROUNDSTART_T,
   ID_FOREGROUNDEND_T,
   ID_BACKGROUNDSTART_T,
   ID_BACKGROUNDEND_T,
   ID_FOREGROUNDDB_TEXT,
   ID_BACKGROUNDDB_TEXT,
   ID_RESULTS_TEXT,
   ID_RESULTSDB_TEXT
};

BEGIN_EVENT_TABLE(ContrastDialog,wxDialogWrapper)
   EVT_BUTTON(ID_BUTTON_USECURRENTF, ContrastDialog::OnGetForeground)
   EVT_BUTTON(ID_BUTTON_USECURRENTB, ContrastDialog::OnGetBackground)
   EVT_BUTTON(ID_BUTTON_GETURL, ContrastDialog::OnGetURL)
   EVT_BUTTON(ID_BUTTON_EXPORT, ContrastDialog::OnExport)
   EVT_BUTTON(ID_BUTTON_RESET, ContrastDialog::OnReset)
   EVT_BUTTON(ID_BUTTON_CLOSE, ContrastDialog::OnClose)
END_EVENT_TABLE()

/* i18n-hint: WCAG2 is the 'Web Content Accessibility Guidelines (WCAG) 2.0', see http://www.w3.org/TR/WCAG20/ */
ContrastDialog::ContrastDialog(wxWindow * parent, wxWindowID id,
                           const wxString & title,
                           const wxPoint & pos):
  wxDialogWrapper(parent, id, title, pos, wxDefaultSize,
     wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX )
{
   SetName(GetTitle());

   mT0 = 0.0;
   mT1 = 0.0;
   foregrounddB = 0.0;
   backgrounddB = 0.0;
   mForegroundIsDefined = false;
   mBackgroundIsDefined = false;

   // NULL out the control members until the controls are created.
   mForegroundStartT = NULL;
   mForegroundEndT = NULL;
   mBackgroundStartT = NULL;
   mBackgroundEndT = NULL;
   wxTextValidator vld(wxFILTER_NUMERIC);
   wxString number;

   AudacityProject *p = GetActiveProject();
   mProjectRate = p->GetRate();

   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("Contrast Analyzer, for measuring RMS volume differences between two selections of audio."));
   }
   S.EndHorizontalLay();
   S.StartStatic( _("Parameters") );
   {
      S.StartMultiColumn(5, wxEXPAND);
      {

         // Headings
         S.AddFixedText(wxT(""));   // spacer
         S.AddFixedText(_("Start"));
         S.AddFixedText(_("End"));
         S.AddFixedText(wxT(""));   // spacer
         S.AddFixedText(_("Volume    "));

         //Foreground
         S.AddFixedText(_("&Foreground:"), false);
         if (S.GetMode() == eIsCreating)
         {
            mForegroundStartT = safenew
               NumericTextCtrl(NumericConverter::TIME, this,
                         ID_FOREGROUNDSTART_T,
                         _("hh:mm:ss + hundredths"),
                         0.0,
                         mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mForegroundStartT->SetName(_("Foreground start time"));
            mForegroundStartT->EnableMenu(false);
            mForegroundStartT->SetReadOnly(true);
         }
         S.AddWindow(mForegroundStartT);

         if (S.GetMode() == eIsCreating)
         {
            mForegroundEndT = safenew
               NumericTextCtrl(NumericConverter::TIME, this,
                         ID_FOREGROUNDEND_T,
                         _("hh:mm:ss + hundredths"),
                         0.0,
                         mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mForegroundEndT->SetName(_("Foreground end time"));
            mForegroundEndT->EnableMenu(false);
            mForegroundEndT->SetReadOnly(true);
         }
         S.AddWindow(mForegroundEndT);

         m_pButton_UseCurrentF = S.Id(ID_BUTTON_USECURRENTF).AddButton(_("&Measure selection"));
         mForegroundRMSText=S.Id(ID_FOREGROUNDDB_TEXT).AddTextBox(wxT(""), wxT(""), 17);
         mForegroundRMSText->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));

         //Background
         S.AddFixedText(_("&Background:"));
         if (S.GetMode() == eIsCreating)
         {
            mBackgroundStartT = safenew
               NumericTextCtrl(NumericConverter::TIME, this,
                         ID_BACKGROUNDSTART_T,
                         _("hh:mm:ss + hundredths"),
                         0.0,
                         mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mBackgroundStartT->SetName(_("Background start time"));
            mBackgroundStartT->EnableMenu(false);
            mBackgroundStartT->SetReadOnly(true);
         }
         S.AddWindow(mBackgroundStartT);

         if (S.GetMode() == eIsCreating)
         {
            mBackgroundEndT = safenew
               NumericTextCtrl(NumericConverter::TIME, this,
                         ID_BACKGROUNDEND_T,
                         _("hh:mm:ss + hundredths"),
                         0.0,
                         mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mBackgroundEndT->SetName(_("Background end time"));
            mBackgroundEndT->EnableMenu(false);
            mBackgroundEndT->SetReadOnly(true);
         }
         S.AddWindow(mBackgroundEndT);

         m_pButton_UseCurrentB = S.Id(ID_BUTTON_USECURRENTB).AddButton(_("Mea&sure selection"));
         mBackgroundRMSText = S.Id(ID_BACKGROUNDDB_TEXT).AddTextBox(wxT(""), wxT(""), 17);
         mBackgroundRMSText->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   //Result
   S.StartStatic( _("Result") );
   {
      S.StartMultiColumn(3, wxCENTER);
      {
         S.AddFixedText(_("Co&ntrast Result:"));
         mPassFailText = S.Id(ID_RESULTS_TEXT).AddTextBox(wxT(""), wxT(""), 50);
         mPassFailText->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));
         m_pButton_Reset = S.Id(ID_BUTTON_RESET).AddButton(_("R&eset"));
         S.AddFixedText(_("&Difference:"));
         mDiffText = S.Id(ID_RESULTSDB_TEXT).AddTextBox(wxT(""), wxT(""), 50);
         mDiffText->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));
         m_pButton_Export = S.Id(ID_BUTTON_EXPORT).AddButton(_("E&xport..."));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(1);
      m_pButton_GetURL = S.Id(ID_BUTTON_GETURL).AddButton(_("&Help"));
      S.AddFixedText(wxT(" "));   // spacer
      m_pButton_Close = S.Id(ID_BUTTON_CLOSE).AddButton(_("&Close"));
   }
   S.EndMultiColumn();
   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

ContrastDialog::~ContrastDialog()
{
   mForegroundRMSText->Disconnect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));
   mBackgroundRMSText->Disconnect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));
   mPassFailText->Disconnect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));
   mDiffText->Disconnect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));
}

void ContrastDialog::OnGetURL(wxCommandEvent & WXUNUSED(event))
{
   // Original help page is back on-line (March 2016), but the manual should be more reliable.
   // http://www.eramp.com/WCAG_2_audio_contrast_tool_help.htm
   HelpSystem::ShowHelpDialog(this, wxT("Contrast"));
}

void ContrastDialog::OnClose(wxCommandEvent & WXUNUSED(event))
{
   wxCommandEvent dummyEvent;
   OnReset(dummyEvent);

   Show(false);
}

void ContrastDialog::OnGetForeground(wxCommandEvent & /*event*/)
{
   AudacityProject *p = GetActiveProject();
   SelectedTrackListOfKindIterator iter(Track::Wave, p->GetTracks());

   for (Track *t = iter.First(); t; t = iter.Next()) {
      mForegroundStartT->SetValue(p->mViewInfo.selectedRegion.t0());
      mForegroundEndT->SetValue(p->mViewInfo.selectedRegion.t1());
   }

   SetStartAndEndTime();
   mForegroundIsDefined = GetDB(foregrounddB);
   m_pButton_UseCurrentF->SetFocus();
   results();
}

void ContrastDialog::OnGetBackground(wxCommandEvent & /*event*/)
{
   AudacityProject *p = GetActiveProject();
   SelectedTrackListOfKindIterator iter(Track::Wave, p->GetTracks());

   for (Track *t = iter.First(); t; t = iter.Next()) {
      mBackgroundStartT->SetValue(p->mViewInfo.selectedRegion.t0());
      mBackgroundEndT->SetValue(p->mViewInfo.selectedRegion.t1());
   }

   SetStartAndEndTime();
   mBackgroundIsDefined = GetDB(backgrounddB);
   m_pButton_UseCurrentB->SetFocus();
   results();
}

void ContrastDialog::results()
{
   mPassFailText->SetName(wxT(""));
   mPassFailText->ChangeValue(wxT(""));
   mDiffText->ChangeValue(wxT(""));

   // foreground and background defined.
   if(mForegroundIsDefined && mBackgroundIsDefined) {
      float diffdB = std::fabs(foregrounddB - backgrounddB);
      if(foregrounddB > DB_MAX_LIMIT) {
         mPassFailText->ChangeValue(_("Foreground level too high"));
      }
      else if (backgrounddB > DB_MAX_LIMIT) {
         mPassFailText->ChangeValue(_("Background level too high"));
      }
      else if (backgrounddB > foregrounddB) {
         mPassFailText->ChangeValue(_("Background higher than foreground"));
      }
      else if(diffdB > WCAG2_PASS) {
         mPassFailText->ChangeValue(_("WCAG2 Pass"));
      }
      else {
         mPassFailText->ChangeValue(_("WCAG2 Fail"));
      }

      /* i18n-hint: i.e. difference in loudness at the moment. */
      mDiffText->SetName(_("Current difference"));
      if( diffdB != diffdB ) {   // test for NaN, reliant on IEEE implementation
         mDiffText->ChangeValue(wxString::Format(_("indeterminate")));
      }
      else {
         if( diffdB != std::numeric_limits<float>::infinity() ) {
            mDiffText->ChangeValue(wxString::Format(_("%.2f dB Average RMS"), diffdB));
         }
         else {
            mDiffText->ChangeValue(wxString::Format(_("Infinite dB difference")));
         }
      }
   }

   if (mForegroundIsDefined) {
      mForegroundRMSText->SetName(_("Measured foreground level"));   // Read by screen-readers
      if(std::isinf(- foregrounddB))
         mForegroundRMSText->ChangeValue(wxString::Format(_("zero")));
      else
         mForegroundRMSText->ChangeValue(wxString::Format(_("%.2f dB"), foregrounddB));   // i18n-hint: short form of 'decibels'        
   }
   else {
      mForegroundRMSText->SetName(_("No foreground measured"));   // Read by screen-readers
      mForegroundRMSText->ChangeValue(wxT(""));
      mPassFailText->ChangeValue(wxString::Format(_("Foreground not yet measured")));
   }

   if (mBackgroundIsDefined) {
      mBackgroundRMSText->SetName(_("Measured background level"));
      if(std::isinf(- backgrounddB))
         mBackgroundRMSText->ChangeValue(wxString::Format(_("zero")));
      else
         mBackgroundRMSText->ChangeValue(wxString::Format(_("%.2f dB"), backgrounddB));
   }
   else {
      mBackgroundRMSText->SetName(_("No background measured"));
      mBackgroundRMSText->ChangeValue(wxT(""));
      mPassFailText->ChangeValue(wxString::Format(_("Background not yet measured")));
   }
}

void ContrastDialog::OnExport(wxCommandEvent & WXUNUSED(event))
{
   // TODO: Handle silence checks better (-infinity dB)
   AudacityProject * project = GetActiveProject();
   wxString fName = wxT("contrast.txt");

   fName = FileSelector(_("Export Contrast Result As:"),
                        wxEmptyString,
                        fName,
                        wxT("txt"),
                        wxT("*.txt"),
                        wxFD_SAVE | wxRESIZE_BORDER,
                        this);

   if (fName == wxT(""))
      return;

   wxTextFile f(fName);
#ifdef __WXMAC__
   wxFile{}.Create(fName);
#else
   f.Create();
#endif
   f.Open();
   if (!f.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName);
      return;
   }

   f.AddLine(wxT("==================================="));
   f.AddLine(_("WCAG 2.0 Success Criteria 1.4.7 Contrast Results"));
   f.AddLine(wxT(""));
   f.AddLine(wxString::Format(_("Filename = %s."), project->GetFileName().c_str() ));
   f.AddLine(wxT(""));
   f.AddLine(_("Foreground"));
   float t = (float)mForegroundStartT->GetValue();
   int h = (int)(t/3600);  // there must be a standard function for this!
   int m = (int)((t - h*3600)/60);
   float s = t - h*3600.0 - m*60.0;
   f.AddLine(wxString::Format(_("Time started = %2d hour(s), %2d minute(s), %.2f seconds."), h, m, s ));
   t = (float)mForegroundEndT->GetValue();
   h = (int)(t/3600);
   m = (int)((t - h*3600)/60);
   s = t - h*3600.0 - m*60.0;
   f.AddLine(wxString::Format(_("Time ended = %2d hour(s), %2d minute(s), %.2f seconds."), h, m, s ));
   if(mForegroundIsDefined)
      if( fabs(foregrounddB) != std::numeric_limits<float>::infinity() )
         f.AddLine(wxString::Format(_("Average RMS = %.2f dB."), foregrounddB ));
      else
         f.AddLine(wxString::Format(_("Average RMS = zero.") ));
   else
      f.AddLine(wxString::Format(_("Average RMS =  dB.")));
   f.AddLine(wxT(""));
   f.AddLine(_("Background"));
   t = (float)mBackgroundStartT->GetValue();
   h = (int)(t/3600);
   m = (int)((t - h*3600)/60);
   s = t - h*3600.0 - m*60.0;
   f.AddLine(wxString::Format(_("Time started = %2d hour(s), %2d minute(s), %.2f seconds."), h, m, s ));
   t = (float)mBackgroundEndT->GetValue();
   h = (int)(t/3600);
   m = (int)((t - h*3600)/60);
   s = t - h*3600.0 - m*60.0;
   f.AddLine(wxString::Format(_("Time ended = %2d hour(s), %2d minute(s), %.2f seconds."), h, m, s ));
   if(mBackgroundIsDefined)
      if( fabs(backgrounddB) != std::numeric_limits<float>::infinity() )
         f.AddLine(wxString::Format(_("Average RMS = %.2f dB."), backgrounddB ));
      else
         f.AddLine(wxString::Format(_("Average RMS = zero.") ));
   else
      f.AddLine(wxString::Format(_("Average RMS =  dB.")));
   f.AddLine(wxT(""));
   f.AddLine(_("Results"));
   float diffdB = foregrounddB - backgrounddB;
   if( diffdB != diffdB ) //test for NaN, reliant on IEEE implementation
      f.AddLine(wxString::Format(_("Difference is indeterminate.") ));
   else
      if( fabs(diffdB) != std::numeric_limits<float>::infinity() )
         f.AddLine(wxString::Format(_("Difference = %.2f Average RMS dB."), diffdB ));
      else
         f.AddLine(wxString::Format(_("Difference = infinite Average RMS dB.")));
   if( diffdB > 20. )
      f.AddLine(_("Success Criteria 1.4.7 of WCAG 2.0: Pass"));
   else
      f.AddLine(_("Success Criteria 1.4.7 of WCAG 2.0: Fail"));

   f.AddLine(wxT(""));
   f.AddLine(_("Data gathered"));
   wxString sNow;
   wxDateTime now = wxDateTime::Now();
   int year = now.GetYear();
   wxDateTime::Month month = now.GetMonth();
   wxString monthName = now.GetMonthName(month);
   int dom = now.GetDay();
   int hour = now.GetHour();
   int minute = now.GetMinute();
   int second = now.GetSecond();
   sNow = wxString::Format(wxT("%d %s %02d %02dh %02dm %02ds"),
        dom, monthName.c_str(), year, hour, minute, second);
   f.AddLine(sNow);

   f.AddLine(wxT("==================================="));
   f.AddLine(wxT(""));

#ifdef __WXMAC__
   f.Write(wxTextFileType_Mac);
#else
   f.Write();
#endif
   f.Close();
}

void ContrastDialog::OnReset(wxCommandEvent & /*event*/)
{
   mForegroundStartT->SetValue(0.0);
   mForegroundEndT->SetValue(0.0);
   mBackgroundStartT->SetValue(0.0);
   mBackgroundEndT->SetValue(0.0);
   mForegroundIsDefined = false;
   mBackgroundIsDefined = false;

   mForegroundRMSText->SetName(_("No foreground measured"));   // Read by screen-readers
   mBackgroundRMSText->SetName(_("No background measured"));
   mForegroundRMSText->ChangeValue(wxT("")); // Displayed value
   mBackgroundRMSText->ChangeValue(wxT(""));
   mPassFailText->ChangeValue(wxT(""));
   mDiffText->ChangeValue(wxT(""));
}

void ContrastDialog::OnChar(wxKeyEvent & event)
{
   // Is this still required?
   if (event.GetKeyCode() == WXK_TAB) {
      event.Skip();
      return;
   }

   event.Skip(false);
   return;
}
