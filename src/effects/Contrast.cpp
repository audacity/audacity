/**********************************************************************

  Audacity: A Digital Audio Editor

  Contrast.cpp

\class ContrastDialog
\brief Dialog used for Contrast menu item

*//*******************************************************************/

#include "../Audacity.h"
#include "Contrast.h"

#include "../WaveTrack.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "../FileNames.h"
#include "../widgets/LinkingHtmlWindow.h"
#include "../widgets/HelpSystem.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/ErrorDialog.h"

#include <cmath>
#include <limits>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

#include <wx/button.h>
#include <wx/filedlg.h>
#include <wx/valtext.h>
#include <wx/log.h>

#include "../PlatformCompatibility.h"

#define DB_MAX_LIMIT 0.0   // Audio is massively distorted.
#define WCAG2_PASS 20.0    // dB difference required to pass WCAG2 test.


bool ContrastDialog::GetDB(float &dB)
{
   float rms = float(0.0);

   // For stereo tracks: sqrt((mean(L)+mean(R))/2)
   double meanSq = 0.0;

   AudacityProject *p = GetActiveProject();
   auto range =
      p->GetTracks()->SelectedLeaders< const WaveTrack >();
   auto numberSelectedTracks = range.size();
   if (numberSelectedTracks > 1) {
      AudacityMessageDialog m(NULL, _("You can only measure one track at a time."), _("Error"), wxOK);
      m.ShowModal();
      return false;
   }
   if(numberSelectedTracks == 0) {
      AudacityMessageDialog m(NULL, _("Please select an audio track."), _("Error"), wxOK);
      m.ShowModal();
      return false;
   }

   const auto channels = TrackList::Channels( *range.begin() );
   for ( auto t : channels ) {
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
         AudacityMessageDialog m(NULL, _("Invalid audio selection.\nPlease ensure that audio is selected."), _("Error"), wxOK);
         m.ShowModal();
         return false;
      }

      if(SelT0 == SelT1)
      {
         AudacityMessageDialog m(NULL, _("Nothing to measure.\nPlease select a section of a track."), _("Error"), wxOK);
         m.ShowModal();
         return false;
      }

      // Don't throw in this analysis dialog
      rms = t->GetRMS(mT0, mT1, false);
      meanSq += rms * rms;
   }
   // TODO: This works for stereo, provided the audio clips are in both channels.
   // We should really count gaps between clips as silence.
   rms = (meanSq > 0.0)
      ? sqrt( meanSq/static_cast<double>( channels.size() ) )
      : 0.0;

   // Gives warning C4056, Overflow in floating-point constant arithmetic
   // -INFINITY is intentional here.
   // Looks like we are stuck with this warning, as 
   // #pragma warning( disable : 4056)
   // even around the whole function does not disable it successfully.

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
   //ID_BUTTON_GETURL,
   ID_BUTTON_EXPORT,
   ID_BUTTON_RESET,
   //ID_BUTTON_CLOSE,
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
   EVT_BUTTON(wxID_HELP, ContrastDialog::OnGetURL)
   EVT_BUTTON(ID_BUTTON_EXPORT, ContrastDialog::OnExport)
   EVT_BUTTON(ID_BUTTON_RESET, ContrastDialog::OnReset)
   EVT_BUTTON(wxID_CANCEL, ContrastDialog::OnClose)
END_EVENT_TABLE()

static void OnChar(wxKeyEvent & event)
{
   // Is this still required?
   if (event.GetKeyCode() == WXK_TAB) {
      // pass to next handler
      event.Skip();
      return;
   }

   // ignore any other key
   event.Skip(false);
   return;
}

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
      /* i18n-hint: RMS abbreviates root mean square, a certain averaging method */
      S.AddTitle(_("Contrast Analyzer, for measuring RMS volume differences between two selections of audio."));
   }
   S.EndHorizontalLay();
   S.StartStatic( _("Parameters") );
   {
      S.StartMultiColumn(5, wxEXPAND);
      {

         // Headings
         S.AddFixedText( {} );   // spacer
         S.AddFixedText(_("Start"));
         S.AddFixedText(_("End"));
         S.AddFixedText( {} );   // spacer
         S.AddFixedText(_("Volume    "));

         const auto options = NumericTextCtrl::Options{}
            .AutoPos(true)
            .MenuEnabled(false)
            .ReadOnly(true);

         //Foreground
         S.AddFixedText(_("&Foreground:"), false);
         if (S.GetMode() == eIsCreating)
         {
            mForegroundStartT = safenew
               NumericTextCtrl(S.GetParent(), ID_FOREGROUNDSTART_T,
                         NumericConverter::TIME,
                         NumericConverter::HundredthsFormat(),
                         0.0,
                         mProjectRate,
                         options);
            mForegroundStartT->SetName(_("Foreground start time"));
         }
         S.AddWindow(mForegroundStartT);

         if (S.GetMode() == eIsCreating)
         {
            mForegroundEndT = safenew
               NumericTextCtrl(S.GetParent(), ID_FOREGROUNDEND_T,
                         NumericConverter::TIME,
                         NumericConverter::HundredthsFormat(),
                         0.0,
                         mProjectRate,
                         options);
            mForegroundEndT->SetName(_("Foreground end time"));
         }
         S.AddWindow(mForegroundEndT);

         m_pButton_UseCurrentF = S.Id(ID_BUTTON_USECURRENTF).AddButton(_("&Measure selection"));
         mForegroundRMSText=S.Id(ID_FOREGROUNDDB_TEXT).AddTextBox( {}, wxT(""), 17);
         mForegroundRMSText->Bind(wxEVT_KEY_DOWN, OnChar);

         //Background
         S.AddFixedText(_("&Background:"));
         if (S.GetMode() == eIsCreating)
         {
            mBackgroundStartT = safenew
               NumericTextCtrl(S.GetParent(), ID_BACKGROUNDSTART_T,
                         NumericConverter::TIME,
                         NumericConverter::HundredthsFormat(),
                         0.0,
                         mProjectRate,
                         options);
            mBackgroundStartT->SetName(_("Background start time"));
         }
         S.AddWindow(mBackgroundStartT);

         if (S.GetMode() == eIsCreating)
         {
            mBackgroundEndT = safenew
               NumericTextCtrl(S.GetParent(), ID_BACKGROUNDEND_T,
                         NumericConverter::TIME,
                         NumericConverter::HundredthsFormat(),
                         0.0,
                         mProjectRate,
                         options);
            mBackgroundEndT->SetName(_("Background end time"));
         }
         S.AddWindow(mBackgroundEndT);

         m_pButton_UseCurrentB = S.Id(ID_BUTTON_USECURRENTB).AddButton(_("Mea&sure selection"));
         mBackgroundRMSText = S.Id(ID_BACKGROUNDDB_TEXT).AddTextBox( {}, wxT(""), 17);
         mBackgroundRMSText->Bind(wxEVT_KEY_DOWN, OnChar);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   //Result
   S.StartStatic( _("Result") );
   {
      S.StartMultiColumn(3, wxCENTER);
      {
         wxString label = _("Co&ntrast Result:");
         S.AddFixedText(label);
         mPassFailText = S.Id(ID_RESULTS_TEXT).AddTextBox( {}, wxT(""), 50);
         mPassFailText->SetName(wxStripMenuCodes(label));
         mPassFailText->Bind(wxEVT_KEY_DOWN, OnChar);
         m_pButton_Reset = S.Id(ID_BUTTON_RESET).AddButton(_("R&eset"));

         label = _("&Difference:");
         S.AddFixedText(label);
         mDiffText = S.Id(ID_RESULTSDB_TEXT).AddTextBox( {}, wxT(""), 50);
         mDiffText->SetName(wxStripMenuCodes(label));
         mDiffText->Bind(wxEVT_KEY_DOWN, OnChar);
         m_pButton_Export = S.Id(ID_BUTTON_EXPORT).AddButton(_("E&xport..."));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.AddStandardButtons(eCloseButton |eHelpButton);
#if 0
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(1);
      m_pButton_GetURL = S.Id(ID_BUTTON_GETURL).AddButton(_("&Help"));
      S.AddFixedText(wxT(" "));   // spacer
      m_pButton_Close = S.Id(ID_BUTTON_CLOSE).AddButton(_("&Close"));
   }
   S.EndMultiColumn();
#endif
   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

void ContrastDialog::OnGetURL(wxCommandEvent & WXUNUSED(event))
{
   // Original help page is back on-line (March 2016), but the manual should be more reliable.
   // http://www.eramp.com/WCAG_2_audio_contrast_tool_help.htm
   HelpSystem::ShowHelp(this, wxT("Contrast"));
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

   if( p->GetTracks()->Selected< const WaveTrack >() ) {
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

   if( p->GetTracks()->Selected< const WaveTrack >() ) {
      mBackgroundStartT->SetValue(p->mViewInfo.selectedRegion.t0());
      mBackgroundEndT->SetValue(p->mViewInfo.selectedRegion.t1());
   }

   SetStartAndEndTime();
   mBackgroundIsDefined = GetDB(backgrounddB);
   m_pButton_UseCurrentB->SetFocus();
   results();
}

namespace {
   // PRL:  I gathered formatting into these functions, and eliminated some
   // repetitions, and removed the redundant word "Average" as applied to RMS.
   // Should these variations in formats be collapsed further?

   // Pass nullptr when value is not yet defined
   wxString FormatRMSMessage( float *pValue )
   {

      /* i18n-hint: RMS abbreviates root mean square, a certain averaging method */
      wxString format0{ _("RMS = %s.") };

      /* i18n-hint: dB abbreviates decibels */
      wxString format1{ _("%s dB") };

      wxString value;

      if ( pValue )
         if( fabs( *pValue ) != std::numeric_limits<float>::infinity() ) {
            auto number = wxString::Format( _("%.2f"), *pValue );
            value = wxString::Format( format1, number );
         }
         else
            value = _("zero");
      else
         value = wxString::Format( format1, "" );

      return wxString::Format( format0, value );
   }

   wxString FormatDifference( float diffdB )
   {
      if( diffdB != diffdB )   // test for NaN, reliant on IEEE implementation
         return _("indeterminate");
      else {
         if( diffdB != std::numeric_limits<float>::infinity() )
            /* i18n-hint: dB abbreviates decibels */
            /* i18n-hint: RMS abbreviates root mean square, a certain averaging method */
            return wxString::Format(_("%.2f dB RMS"), diffdB);
         else
            /* i18n-hint: dB abbreviates decibels */
            return _("Infinite dB difference");
      }
   }

   wxString FormatDifferenceForExport( float diffdB )
   {
      if( diffdB != diffdB ) //test for NaN, reliant on IEEE implementation
         return _("Difference is indeterminate.");
      else
         if( fabs(diffdB) != std::numeric_limits<float>::infinity() )
            /* i18n-hint: dB abbreviates decibels */
            /* i18n-hint: RMS abbreviates root mean square, a certain averaging method */
            return wxString::Format(_("Difference = %.2f RMS dB."), diffdB );
         else
            /* i18n-hint: dB abbreviates decibels */
            /* i18n-hint: RMS abbreviates root mean square, a certain averaging method */
            return _("Difference = infinite RMS dB.");
   }
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
         /* i18n-hint: WCAG abbreviates Web Content Accessibility Guidelines */
         mPassFailText->ChangeValue(_("WCAG2 Pass"));
      }
      else {
         /* i18n-hint: WCAG abbreviates Web Content Accessibility Guidelines */
         mPassFailText->ChangeValue(_("WCAG2 Fail"));
      }

      /* i18n-hint: i.e. difference in loudness at the moment. */
      mDiffText->SetName(_("Current difference"));
      mDiffText->ChangeValue( FormatDifference( diffdB ) );
   }

   if (mForegroundIsDefined) {
      mForegroundRMSText->SetName(_("Measured foreground level"));   // Read by screen-readers
      if(std::isinf(- foregrounddB))
         mForegroundRMSText->ChangeValue(_("zero"));
      else
         mForegroundRMSText->ChangeValue(wxString::Format(_("%.2f dB"), foregrounddB));   // i18n-hint: short form of 'decibels'        
   }
   else {
      mForegroundRMSText->SetName(_("No foreground measured"));   // Read by screen-readers
      mForegroundRMSText->ChangeValue(wxT(""));
      mPassFailText->ChangeValue(_("Foreground not yet measured"));
   }

   if (mBackgroundIsDefined) {
      mBackgroundRMSText->SetName(_("Measured background level"));
      if(std::isinf(- backgrounddB))
         mBackgroundRMSText->ChangeValue(_("zero"));
      else
         mBackgroundRMSText->ChangeValue(wxString::Format(_("%.2f dB"), backgrounddB));
   }
   else {
      mBackgroundRMSText->SetName(_("No background measured"));
      mBackgroundRMSText->ChangeValue(wxT(""));
      mPassFailText->ChangeValue(_("Background not yet measured"));
   }
}

void ContrastDialog::OnExport(wxCommandEvent & WXUNUSED(event))
{
   // TODO: Handle silence checks better (-infinity dB)
   AudacityProject * project = GetActiveProject();
   wxString fName = wxT("contrast.txt");

   fName = FileNames::SelectFile(FileNames::Operation::Export,
                        _("Export Contrast Result As:"),
                        wxEmptyString,
                        fName,
                        wxT("txt"),
                        wxT("*.txt"),
                        wxFD_SAVE | wxRESIZE_BORDER,
                        this);

   if (fName.empty())
      return;

   wxTextFile f(fName);
#ifdef __WXMAC__
   wxFile{}.Create(fName);
#else
   f.Create();
#endif
   f.Open();
   if (!f.IsOpened()) {
      AudacityMessageBox(
         wxString::Format( _("Couldn't write to file: %s"), fName) );
      return;
   }

   f.AddLine(wxT("==================================="));
   /* i18n-hint: WCAG abbreviates Web Content Accessibility Guidelines */
   f.AddLine(_("WCAG 2.0 Success Criteria 1.4.7 Contrast Results"));
   f.AddLine(wxT(""));
   f.AddLine(wxString::Format(_("Filename = %s."), project->GetFileName() ));
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
   f.AddLine( FormatRMSMessage( mForegroundIsDefined ? &foregrounddB : nullptr ) );
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
   f.AddLine( FormatRMSMessage( mBackgroundIsDefined ? &backgrounddB : nullptr ) );
   f.AddLine(wxT(""));
   f.AddLine(_("Results"));
   float diffdB = foregrounddB - backgrounddB;

   f.AddLine( FormatDifferenceForExport( diffdB ) );
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
        dom, monthName, year, hour, minute, second);
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
