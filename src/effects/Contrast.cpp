/**********************************************************************

  Audacity: A Digital Audio Editor

  Contrast.cpp

\class ContrastDialog
\brief Dialog used for Contrast menu item

*//*******************************************************************/


#include "Contrast.h"

#include "../CommonCommandFlags.h"
#include "WaveTrack.h"
#include "Prefs.h"
#include "Project.h"
#include "../ProjectFileIO.h"
#include "ProjectRate.h"
#include "../ProjectWindow.h"
#include "../SelectFile.h"
#include "ShuttleGui.h"
#include "FileNames.h"
#include "ViewInfo.h"
#include "../widgets/HelpSystem.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/VetoDialogHook.h"

#include <cmath>
#include <limits>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

#include <wx/button.h>
#include <wx/valtext.h>
#include <wx/log.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <wx/textctrl.h>

#include "PlatformCompatibility.h"

#define DB_MAX_LIMIT 0.0   // Audio is massively distorted.
#define WCAG2_PASS 20.0    // dB difference required to pass WCAG2 test.


bool ContrastDialog::GetDB(float &dB)
{
   float rms = float(0.0);

   // For stereo tracks: sqrt((mean(L)+mean(R))/2)
   double meanSq = 0.0;

   auto p = FindProjectFromWindow( this );
   auto range =
      TrackList::Get( *p ).SelectedLeaders< const WaveTrack >();
   auto numberSelectedTracks = range.size();
   if (numberSelectedTracks > 1) {
      AudacityMessageDialog m(
         nullptr,
         XO("You can only measure one track at a time."),
         XO("Error"),
         wxOK);
      m.ShowModal();
      return false;
   }
   if(numberSelectedTracks == 0) {
      AudacityMessageDialog m(
         nullptr,
         XO("Please select an audio track."),
         XO("Error"),
         wxOK);
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
         AudacityMessageDialog m(
            nullptr,
            XO("Invalid audio selection.\nPlease ensure that audio is selected."),
            XO("Error"),
            wxOK);
         m.ShowModal();
         return false;
      }

      if(SelT0 == SelT1)
      {
         AudacityMessageDialog m(
            nullptr,
            XO("Nothing to measure.\nPlease select a section of a track."),
            XO("Error"),
            wxOK);
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
   auto p = FindProjectFromWindow( this );
   auto &selectedRegion = ViewInfo::Get( *p ).selectedRegion;
   mT0 = selectedRegion.t0();
   mT1 = selectedRegion.t1();
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

void ContrastDialog::OnChar(wxKeyEvent &event)
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

ContrastDialog::ContrastDialog(wxWindow * parent, wxWindowID id,
                           const TranslatableString & title,
                           const wxPoint & pos):
  wxDialogWrapper(parent, id, title, pos, wxDefaultSize,
     wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX )
{
   SetName();

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
   wxString number;

   auto p = FindProjectFromWindow( this );
   mProjectRate = ProjectRate::Get(*p).GetRate();

      const auto options = NumericTextCtrl::Options{}
         .AutoPos(true)
         .MenuEnabled(false)
         .ReadOnly(true);

   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(
         /* i18n-hint: RMS abbreviates root mean square, a certain averaging method */
         XO("Contrast Analyzer, for measuring RMS volume differences between two selections of audio."));
   }
   S.EndHorizontalLay();
   S.StartStatic( XO("Parameters") );
   {
      S.StartMultiColumn(5, wxEXPAND);
      {

         // Headings
         S.AddFixedText( {} );   // spacer
         S.AddFixedText(XO("Start"));
         S.AddFixedText(XO("End"));
         S.AddFixedText( {} );   // spacer
         S.AddFixedText(XO("Volume    "));

         //Foreground
         S.AddFixedText(XO("&Foreground:"), false);
         if (S.GetMode() == eIsCreating)
         {
            mForegroundStartT = safenew
               NumericTextCtrl(S.GetParent(), ID_FOREGROUNDSTART_T,
                         NumericConverter::TIME,
                         NumericConverter::HundredthsFormat(),
                         0.0,
                         mProjectRate,
                         options);
         }
         S.Name(XO("Foreground start time"))
            .AddWindow(mForegroundStartT);

         if (S.GetMode() == eIsCreating)
         {
            mForegroundEndT = safenew
               NumericTextCtrl(S.GetParent(), ID_FOREGROUNDEND_T,
                         NumericConverter::TIME,
                         NumericConverter::HundredthsFormat(),
                         0.0,
                         mProjectRate,
                         options);
         }
         S.Name(XO("Foreground end time"))
            .AddWindow(mForegroundEndT);

         m_pButton_UseCurrentF = S.Id(ID_BUTTON_USECURRENTF).AddButton(XXO("&Measure selection"));
         mForegroundRMSText = S.Id(ID_FOREGROUNDDB_TEXT)
            .ConnectRoot(wxEVT_KEY_DOWN,
                         &ContrastDialog::OnChar)
            .AddTextBox( {}, wxT(""), 17);

         //Background
         S.AddFixedText(XO("&Background:"));
         if (S.GetMode() == eIsCreating)
         {
            mBackgroundStartT = safenew
               NumericTextCtrl(S.GetParent(), ID_BACKGROUNDSTART_T,
                         NumericConverter::TIME,
                         NumericConverter::HundredthsFormat(),
                         0.0,
                         mProjectRate,
                         options);
         }
         S.Name(XO("Background start time"))
            .AddWindow(mBackgroundStartT);

         if (S.GetMode() == eIsCreating)
         {
            mBackgroundEndT = safenew
               NumericTextCtrl(S.GetParent(), ID_BACKGROUNDEND_T,
                         NumericConverter::TIME,
                         NumericConverter::HundredthsFormat(),
                         0.0,
                         mProjectRate,
                         options);
         }
         S.Name(XO("Background end time"))
            .AddWindow(mBackgroundEndT);

         m_pButton_UseCurrentB = S.Id(ID_BUTTON_USECURRENTB).AddButton(XXO("Mea&sure selection"));
         mBackgroundRMSText = S.Id(ID_BACKGROUNDDB_TEXT)
            .ConnectRoot(wxEVT_KEY_DOWN,
                         &ContrastDialog::OnChar)
            .AddTextBox( {}, wxT(""), 17);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   //Result
   S.StartStatic( XO("Result") );
   {
      S.StartMultiColumn(3, wxCENTER);
      {
         auto label = XO("Co&ntrast Result:");
         S.AddFixedText(label);
         mPassFailText = S.Id(ID_RESULTS_TEXT)
            .Name(label)
            .ConnectRoot(wxEVT_KEY_DOWN,
                         &ContrastDialog::OnChar)
            .AddTextBox( {}, wxT(""), 50);
         m_pButton_Reset = S.Id(ID_BUTTON_RESET).AddButton(XXO("R&eset"));

         label = XO("&Difference:");
         S.AddFixedText(label);
         mDiffText = S.Id(ID_RESULTSDB_TEXT)
            .Name(label)
            .ConnectRoot(wxEVT_KEY_DOWN,
                         &ContrastDialog::OnChar)
            .AddTextBox( {}, wxT(""), 50);
         m_pButton_Export = S.Id(ID_BUTTON_EXPORT).AddButton(XXO("E&xport..."));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.AddStandardButtons(eCloseButton |eHelpButton);
#if 0
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(1);
      m_pButton_GetURL = S.Id(ID_BUTTON_GETURL).AddButton(XO("&Help"));
      S.AddFixedText({});   // spacer
      m_pButton_Close = S.Id(ID_BUTTON_CLOSE).AddButton(XO("&Close"));
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
   HelpSystem::ShowHelp(this, L"Contrast");
}

void ContrastDialog::OnClose(wxCommandEvent & WXUNUSED(event))
{
   wxCommandEvent dummyEvent;
   OnReset(dummyEvent);

   Show(false);
}

void ContrastDialog::OnGetForeground(wxCommandEvent & /*event*/)
{
   auto p = FindProjectFromWindow( this );
   auto &selectedRegion = ViewInfo::Get( *p ).selectedRegion;

   if( TrackList::Get( *p ).Selected< const WaveTrack >() ) {
      mForegroundStartT->SetValue(selectedRegion.t0());
      mForegroundEndT->SetValue(selectedRegion.t1());
   }

   SetStartAndEndTime();
   mForegroundIsDefined = GetDB(foregrounddB);
   m_pButton_UseCurrentF->SetFocus();
   results();
}

void ContrastDialog::OnGetBackground(wxCommandEvent & /*event*/)
{
   auto p = FindProjectFromWindow( this );
   auto &selectedRegion = ViewInfo::Get( *p ).selectedRegion;

   if( TrackList::Get( *p ).Selected< const WaveTrack >() ) {
      mBackgroundStartT->SetValue(selectedRegion.t0());
      mBackgroundEndT->SetValue(selectedRegion.t1());
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
   TranslatableString FormatRMSMessage( float *pValue )
   {

      /* i18n-hint: RMS abbreviates root mean square, a certain averaging method */
      auto format0 = XO("RMS = %s.");

      /* i18n-hint: dB abbreviates decibels */
      auto format1 = XO("%s dB");

      TranslatableString value;

      if ( pValue ) {
         if( fabs( *pValue ) != std::numeric_limits<float>::infinity() ) {
            auto number = wxString::Format( wxT("%.2f"), *pValue );
            value = format1.Format( number );
         }
         else
            value = XO("zero");
      }
      else
         value = format1.Format( "" );

      return format0.Format( value );
   }

   TranslatableString FormatDifference( float diffdB )
   {
      if( diffdB != diffdB )   // test for NaN, reliant on IEEE implementation
         return XO("indeterminate");
      else {
         if( diffdB != std::numeric_limits<float>::infinity() )
            /* i18n-hint: dB abbreviates decibels
             * RMS abbreviates root mean square, a certain averaging method */
            return XO("%.2f dB RMS").Format( diffdB );
         else
            /* i18n-hint: dB abbreviates decibels */
            return XO("Infinite dB difference");
      }
   }

   TranslatableString FormatDifferenceForExport( float diffdB )
   {
      if( diffdB != diffdB ) //test for NaN, reliant on IEEE implementation
         return XO("Difference is indeterminate.");
      else
         if( fabs(diffdB) != std::numeric_limits<float>::infinity() )
            /* i18n-hint: dB abbreviates decibels
               RMS abbreviates root mean square, a certain averaging method */
            return XO("Difference = %.2f RMS dB.").Format( diffdB );
         else
            /* i18n-hint: dB abbreviates decibels
               RMS abbreviates root mean square, a certain averaging method */
            return XO("Difference = infinite RMS dB.");
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
         /* i18n-hint: WCAG2 is the 'Web Content Accessibility Guidelines (WCAG) 2.0', see http://www.w3.org/TR/WCAG20/ */
         mPassFailText->ChangeValue(_("WCAG2 Pass"));
      }
      else {
         /* i18n-hint: WCAG abbreviates Web Content Accessibility Guidelines */
         mPassFailText->ChangeValue(_("WCAG2 Fail"));
      }

      /* i18n-hint: i.e. difference in loudness at the moment. */
      mDiffText->SetName(_("Current difference"));
      mDiffText->ChangeValue( FormatDifference( diffdB ).Translation() );
   }

   if (mForegroundIsDefined) {
      mForegroundRMSText->SetName(_("Measured foreground level"));   // Read by screen-readers
      if(std::isinf(- foregrounddB))
         mForegroundRMSText->ChangeValue(_("zero"));
      else
         // i18n-hint: short form of 'decibels'
         mForegroundRMSText->ChangeValue(wxString::Format(_("%.2f dB"), foregrounddB));
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
   auto project = FindProjectFromWindow( this );
   wxString fName = wxT("contrast.txt");

   fName = SelectFile(FileNames::Operation::Export,
      XO("Export Contrast Result As:"),
      wxEmptyString,
      fName,
      wxT("txt"),
      { FileNames::TextFiles, FileNames::AllFiles },
      wxFD_SAVE | wxRESIZE_BORDER,
      this);

   if (fName.empty())
      return;

   wxFFileOutputStream ffStream{ fName };

   if (!ffStream.IsOk()) {
      AudacityMessageBox( XO("Couldn't write to file: %s").Format( fName ) );
      return;
   }

   wxTextOutputStream ss(ffStream);

   ss
      << wxT("===================================") << '\n'
   /* i18n-hint: WCAG abbreviates Web Content Accessibility Guidelines */
      << XO("WCAG 2.0 Success Criteria 1.4.7 Contrast Results") << '\n'
      << '\n'
      << XO("Filename = %s.").Format( ProjectFileIO::Get(*project).GetFileName() ) << '\n'
      << '\n'
      << XO("Foreground") << '\n';

   float t = (float)mForegroundStartT->GetValue();
   int h = (int)(t/3600);  // there must be a standard function for this!
   int m = (int)((t - h*3600)/60);
   float s = t - h*3600.0 - m*60.0;

   ss
      << XO("Time started = %2d hour(s), %2d minute(s), %.2f seconds.")
         .Format( h, m, s ) << '\n';

   t = (float)mForegroundEndT->GetValue();
   h = (int)(t/3600);
   m = (int)((t - h*3600)/60);
   s = t - h*3600.0 - m*60.0;

   ss
      << XO("Time ended = %2d hour(s), %2d minute(s), %.2f seconds.")
         .Format( h, m, s ) << '\n'
      << FormatRMSMessage( mForegroundIsDefined ? &foregrounddB : nullptr ) << '\n'
      << '\n'
      << XO("Background") << '\n';

   t = (float)mBackgroundStartT->GetValue();
   h = (int)(t/3600);
   m = (int)((t - h*3600)/60);
   s = t - h*3600.0 - m*60.0;

   ss
      << XO("Time started = %2d hour(s), %2d minute(s), %.2f seconds.")
         .Format( h, m, s ) << '\n';

   t = (float)mBackgroundEndT->GetValue();
   h = (int)(t/3600);
   m = (int)((t - h*3600)/60);
   s = t - h*3600.0 - m*60.0;

   ss
      << XO("Time ended = %2d hour(s), %2d minute(s), %.2f seconds.")
         .Format( h, m, s ) << '\n'
      << FormatRMSMessage( mBackgroundIsDefined ? &backgrounddB : nullptr ) << '\n'
      << '\n'
      << XO("Results") << '\n';

   float diffdB = foregrounddB - backgrounddB;

   ss
      << FormatDifferenceForExport( diffdB ) << '\n'
      << (( diffdB > 20. )
         ? XO("Success Criteria 1.4.7 of WCAG 2.0: Pass")
         : XO("Success Criteria 1.4.7 of WCAG 2.0: Fail")) << '\n'
      << '\n'
      << XO("Data gathered") << '\n';

   wxDateTime now = wxDateTime::Now();
   int year = now.GetYear();
   wxDateTime::Month month = now.GetMonth();
   wxString monthName = now.GetMonthName(month);
   int dom = now.GetDay();
   int hour = now.GetHour();
   int minute = now.GetMinute();
   int second = now.GetSecond();
   /* i18n-hint: day of month, month, year, hour, minute, second */
   auto sNow = XO("%d %s %02d %02dh %02dm %02ds")
      .Format( dom, monthName, year, hour, minute, second );

   ss <<
      sNow << '\n'
      << wxT("===================================") << '\n'
      << '\n';
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

// Remaining code hooks this add-on into the application
#include "commands/CommandContext.h"
#include "commands/CommandManager.h"
#include "ProjectWindows.h"

namespace {

// Contrast window attached to each project is built on demand by:
AttachedWindows::RegisteredFactory sContrastDialogKey{
   []( AudacityProject &parent ) -> wxWeakRef< wxWindow > {
      auto &window = ProjectWindow::Get( parent );
      return safenew ContrastDialog(
         &window, -1, XO("Contrast Analysis (WCAG 2 compliance)"),
         wxPoint{ 150, 150 }
      );
   }
};

// Define our extra menu item that invokes that factory
namespace {
   void OnContrast(const CommandContext &context)
   {
      auto &project = context.project;
      CommandManager::Get(project).RegisterLastAnalyzer(context);  //Register Contrast as Last Analyzer
      auto contrastDialog = &GetAttachedWindows(project)
         .Get< ContrastDialog >( sContrastDialogKey );

      contrastDialog->CentreOnParent();
      if( VetoDialogHook::Call( contrastDialog ) )
         return;
      contrastDialog->Show();
   }
}

// Register that menu item

using namespace MenuTable;
AttachedItem sAttachment{ wxT("Analyze/Analyzers/Windows"),
   Command( wxT("ContrastAnalyser"), XXO("Contrast..."),
      OnContrast,
      AudioIONotBusyFlag() | WaveTracksSelectedFlag() | TimeSelectedFlag(),
      wxT("Ctrl+Shift+T") )
};

}
