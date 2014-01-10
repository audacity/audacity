/**********************************************************************

  Audacity: A Digital Audio Editor

  Contrast.cpp

\class ContrastDialog
\brief Dialog used for Contrast menu item

*//*******************************************************************/

#include "../Audacity.h"
#include "../AudacityApp.h"

#include "Contrast.h"

#include "../Envelope.h"
#include "../FFT.h"
#include "../WaveTrack.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../FileNames.h"
#include "../widgets/LinkingHtmlWindow.h"
#include "FileDialog.h"

#include <math.h>
#include <limits>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

// all these headers may not be needed
#include <wx/file.h>
#include <wx/ffile.h>
#include <wx/bitmap.h>
#include <wx/brush.h>
#include <wx/button.h>
#include <wx/datetime.h>
#include <wx/dcmemory.h>
#include <wx/file.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>

#include "../PlatformCompatibility.h"

static ContrastDialog *gContrastDialog = NULL;

void InitContrastDialog(wxWindow * parent)
{
   if(!gContrastDialog)
   {
      wxPoint where;

      where.x = 150;
      where.y = 150;

      gContrastDialog = new ContrastDialog(parent, -1, _("Contrast Analysis (WCAG 2 compliance)"), where);

      gContrastDialog->bFGset = false;
      gContrastDialog->bBGset = false;
   }

   // Zero dialog boxes.  Do we need to do this here? 
   if( !gContrastDialog->bFGset )
   {
      gContrastDialog->mForegroundStartT->SetTimeValue(0.0);
      gContrastDialog->mForegroundEndT->SetTimeValue(0.0);
   }
   if( !gContrastDialog->bBGset )
   {
      gContrastDialog->mBackgroundStartT->SetTimeValue(0.0);
      gContrastDialog->mBackgroundEndT->SetTimeValue(0.0);
   }

   gContrastDialog->CentreOnParent();
   gContrastDialog->Show();
}

void CloseContrastDialog()
{
   if (gContrastDialog) {
      delete gContrastDialog;
      gContrastDialog = NULL;
   }
}

float ContrastDialog::GetDB()
{
   // FIXME: what if more than one track?
   float rms = float(0.0);

   AudacityProject *p = GetActiveProject();
   TrackListOfKindIterator iter(Track::Wave, p->GetTracks());
   Track *t = iter.First();
   if(mT0 > mT1)
   {
      wxMessageDialog m(NULL, _("Start time after end time!\nPlease enter reasonable times."), _("Error"), wxOK);
      m.ShowModal();
      return 1234.0; // 'magic number', but the whole +ve dB range will 'almost' never occur
   }
   if(mT0 < t->GetStartTime())
      mT0 = t->GetStartTime();
   if(mT1 > t->GetEndTime())
      mT1 = t->GetEndTime();
   if(mT0 > mT1)
   {
      wxMessageDialog m(NULL, _("Times are not reasonable!\nPlease enter reasonable times."), _("Error"), wxOK);
      m.ShowModal();
      return 1234.0;
   }
   if(mT0 == mT1)
   {
      wxMessageDialog m(NULL, _("Nothing to measure.\nPlease select a section of a track."), _("Error"), wxOK);
      m.ShowModal();
      return 1234.0;
   }
   bool mSelected = false;
   while(t) {  
      if( ((WaveTrack *)t)->GetSelected() )
      {
         if( mSelected == true ) // already measured one track
         {
            wxMessageDialog m(NULL, _("You can only measure one track at a time."), _("Error"), wxOK);
            m.ShowModal();
            return 1234.0;
         }
         else
         {
            ((WaveTrack *)t)->GetRMS(&rms, mT0, mT1);
            mSelected = true;
         }
      }
      t = iter.Next();
   }
   if( mSelected )
   {
      if( rms < 1.0E-30 )
         return -60.0;
      return 20.0*log10(rms);
   }
   else
   {
      wxMessageDialog m(NULL, _("Please select something to be measured."), _("Error"), wxOK);
      m.ShowModal();
      return 1234.0;
   }
}

double ContrastDialog::GetStartTime()
{
   return(mT0);
}

void ContrastDialog::SetStartTime(double t)
{
   mT0 = t;
}

double ContrastDialog::GetEndTime()
{
   return(mT1);
}

void ContrastDialog::SetEndTime(double t)
{
   mT1 = t;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// ContrastDialog
//----------------------------------------------------------------------------

// WDR: event table for ContrastDialog

enum {
   ID_BUTTON_GETFOREGROUND = 10001,
   ID_BUTTON_GETBACKGROUND,
   ID_BUTTON_USECURRENTF,
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

BEGIN_EVENT_TABLE(ContrastDialog,wxDialog)
   EVT_BUTTON(ID_BUTTON_GETFOREGROUND, ContrastDialog::OnGetForegroundDB)
   EVT_BUTTON(ID_BUTTON_GETBACKGROUND, ContrastDialog::OnGetBackgroundDB)
   EVT_BUTTON(ID_BUTTON_USECURRENTF, ContrastDialog::OnUseSelectionF)
   EVT_BUTTON(ID_BUTTON_USECURRENTB, ContrastDialog::OnUseSelectionB)
   EVT_BUTTON(ID_BUTTON_GETURL, ContrastDialog::OnGetURL)
   EVT_BUTTON(ID_BUTTON_EXPORT, ContrastDialog::OnExport)
   EVT_BUTTON(ID_BUTTON_RESET, ContrastDialog::OnReset)
   EVT_BUTTON(ID_BUTTON_CLOSE, ContrastDialog::OnClose)
END_EVENT_TABLE()

/* i18n-hint: WCAG2 is the 'Web Content Accessibility Guidelines (WCAG) 2.0', see http://www.w3.org/TR/WCAG20/ */
ContrastDialog::ContrastDialog(wxWindow * parent, wxWindowID id,
                           const wxString & title,
                           const wxPoint & pos):
  wxDialog(parent, id, title, pos, wxDefaultSize,
     wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX )
{
   foregrounddB = 1234.0;
   backgrounddB = 1234.0;

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
      S.AddTitle(_("Contrast Analyzer, for measuring rms volume differences between two selections of audio."));
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
         if (mForegroundStartT == NULL)
         {
            mForegroundStartT = new
            TimeTextCtrl(this,
                         ID_FOREGROUNDSTART_T,
                         _("hh:mm:ss + hundredths"),
                         0.0,
                         mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mForegroundStartT->SetName(_("Foreground start time"));
            mForegroundStartT->EnableMenu(false);
         }
         S.AddWindow(mForegroundStartT);

         if (mForegroundEndT == NULL)
         {
            mForegroundEndT = new
            TimeTextCtrl(this,
                         ID_FOREGROUNDEND_T,
                         _("hh:mm:ss + hundredths"),
                         0.0,
                         mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mForegroundEndT->SetName(_("Foreground end time"));
            mForegroundEndT->EnableMenu(false);
         }
         S.AddWindow(mForegroundEndT);

         m_pButton_UseCurrentF = S.Id(ID_BUTTON_USECURRENTF).AddButton(_("&Measure selection"));
         mForegroundRMSText=S.Id(ID_FOREGROUNDDB_TEXT).AddTextBox(wxT(""), wxT(""), 12);
         mForegroundRMSText->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));

         //Background
         S.AddFixedText(_("&Background:"));
         if (mBackgroundStartT == NULL)
         {
            mBackgroundStartT = new
            TimeTextCtrl(this,
                         ID_BACKGROUNDSTART_T,
                         _("hh:mm:ss + hundredths"),
                         0.0,
                         mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mBackgroundStartT->SetName(_("Background start time"));
            mBackgroundStartT->EnableMenu(false);
         }
         S.AddWindow(mBackgroundStartT);

         if (mBackgroundEndT == NULL)
         {
            mBackgroundEndT = new
            TimeTextCtrl(this,
                         ID_BACKGROUNDEND_T,
                         _("hh:mm:ss + hundredths"),
                         0.0,
                         mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mBackgroundEndT->SetName(_("Background end time"));
            mBackgroundEndT->EnableMenu(false);
         }
         S.AddWindow(mBackgroundEndT);

         m_pButton_UseCurrentB = S.Id(ID_BUTTON_USECURRENTB).AddButton(_("Mea&sure selection"));
         mBackgroundRMSText = S.Id(ID_BACKGROUNDDB_TEXT).AddTextBox(wxT(""), wxT(""), 12);
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
         mPassFailText = S.Id(ID_RESULTS_TEXT).AddTextBox(wxT(""), wxT(""), 40);
         mPassFailText->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));
         m_pButton_Reset = S.Id(ID_BUTTON_RESET).AddButton(_("R&eset"));
         S.AddFixedText(_("&Difference:"));
         mDiffText = S.Id(ID_RESULTSDB_TEXT).AddTextBox(wxT(""), wxT(""), 30);
         mDiffText->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(ContrastDialog::OnChar));
         m_pButton_Export = S.Id(ID_BUTTON_EXPORT).AddButton(_("E&xport..."));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(1);
      m_pButton_GetURL = S.Id(ID_BUTTON_GETURL).AddButton(_("WCAG 2 &Help"));
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

void ContrastDialog::OnGetForegroundDB( wxCommandEvent & WXUNUSED(event))
{
   SetStartTime(mForegroundStartT->GetTimeValue());
   SetEndTime(mForegroundEndT->GetTimeValue());
   foregrounddB = GetDB();
   m_pButton_UseCurrentF->SetFocus();
   results();
}

void ContrastDialog::OnGetBackgroundDB( wxCommandEvent & WXUNUSED(event))
{
   SetStartTime(mBackgroundStartT->GetTimeValue());
   SetEndTime(mBackgroundEndT->GetTimeValue());
   backgrounddB = GetDB();
   m_pButton_UseCurrentB->SetFocus();
   results();
}

void ContrastDialog::OnGetURL(wxCommandEvent & WXUNUSED(event))
{
   wxString page = wxT("http://www.eramp.com/WCAG_2_audio_contrast_tool_help.htm");
   ::OpenInDefaultBrowser(page);
}

void ContrastDialog::OnClose(wxCommandEvent & WXUNUSED(event))
{
   Show(false);
}

void ContrastDialog::OnUseSelectionF(wxCommandEvent & event)
{
   AudacityProject *p = GetActiveProject();
   TrackListIterator iter(p->GetTracks());
   Track *t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         mForegroundStartT->SetTimeValue(p->mViewInfo.sel0);
         mForegroundEndT->SetTimeValue(p->mViewInfo.sel1);
         break;
      }
      t = iter.Next();
   }
   bFGset = true;
   OnGetForegroundDB(event);
}

void ContrastDialog::OnUseSelectionB(wxCommandEvent & event)
{
   AudacityProject *p = GetActiveProject();
   TrackListIterator iter(p->GetTracks());
   Track *t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         mBackgroundStartT->SetTimeValue(p->mViewInfo.sel0);
         mBackgroundEndT->SetTimeValue(p->mViewInfo.sel1);
         break;
      }
      t = iter.Next();
   }
   bBGset = true;
   OnGetBackgroundDB(event);
}

void ContrastDialog::results()
{
   if(foregrounddB == 1234.0) // magic number, but OK for now
   {
      mForegroundRMSText->SetName(_("No foreground measured"));
      mForegroundRMSText->ChangeValue(wxString::Format(wxT(" ")));
   }
   else
   {
      mForegroundRMSText->SetName(_("Measured foreground level"));
      if( -foregrounddB != std::numeric_limits<float>::infinity() )
         mForegroundRMSText->ChangeValue(wxString::Format(_("%.1f dB"), foregrounddB));   // i18n-hint: short form of 'decibels'
      else
         mForegroundRMSText->ChangeValue(wxString::Format(_("zero")));
   }
   if(backgrounddB == 1234.0)
   {
      mBackgroundRMSText->SetName(_("No background measured"));
      mBackgroundRMSText->ChangeValue(wxString::Format(wxT(" ")));
   }
   else
   {
      mBackgroundRMSText->SetName(_("Measured background level"));
      if( backgrounddB * backgrounddB != std::numeric_limits<float>::infinity() )
         mBackgroundRMSText->ChangeValue(wxString::Format(_("%.1f dB"), backgrounddB));
      else
         mBackgroundRMSText->ChangeValue(wxString::Format(_("zero")));
   }
   if( (foregrounddB != 1234.0) && (backgrounddB != 1234.0) )
   {
      if(foregrounddB - backgrounddB > 20)
         mPassFailText->ChangeValue(_("WCAG2 Pass"));
      else
         mPassFailText->ChangeValue(_("WCAG2 Fail"));
      /* i18n-hint: i.e. difference in loudness at the moment. */
      mDiffText->SetName(_("Current difference"));
      float diffdB = foregrounddB - backgrounddB;
      if( diffdB != diffdB )  // test for NaN, reliant on IEEE implementation
         mDiffText->ChangeValue(wxString::Format(_("indeterminate")));
      else
         if( fabs(diffdB) != std::numeric_limits<float>::infinity() )
            mDiffText->ChangeValue(wxString::Format(_("%.1f dB Average rms"), diffdB));
         else
            mDiffText->ChangeValue(wxString::Format(_("infinite dB difference")));
   }
   else
   {
      mPassFailText->SetName(wxT(""));
      mPassFailText->ChangeValue(_("Please enter valid times."));
      mDiffText->ChangeValue(wxT(""));
   }
}

void ContrastDialog::OnExport(wxCommandEvent & WXUNUSED(event))
{
   AudacityProject * project = GetActiveProject();
   wxString fName = wxT("contrast.txt");

   fName = FileSelector(_("Export Contrast Result As:"),
                        NULL, fName, wxT("txt"), wxT("*.txt"), wxFD_SAVE | wxRESIZE_BORDER, this);

   if (fName == wxT(""))
      return;

   wxTextFile f(fName);
#ifdef __WXMAC__
   wxFile *temp = new wxFile();
   temp->Create(fName);
   delete temp;
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
   float t = (float)mForegroundStartT->GetTimeValue();
   int h = (int)(t/3600);  // there must be a standard function for this!
   int m = (int)((t - h*3600)/60);
   float s = t - h*3600.0 - m*60.0;
   f.AddLine(wxString::Format(_("Time started = %2d hour(s), %2d minute(s), %.2f seconds."), h, m, s ));
   t = (float)mForegroundEndT->GetTimeValue();
   h = (int)(t/3600);
   m = (int)((t - h*3600)/60);
   s = t - h*3600.0 - m*60.0;
   f.AddLine(wxString::Format(_("Time ended = %2d hour(s), %2d minute(s), %.2f seconds."), h, m, s ));
   if(foregrounddB != 1234.0) // see other instances of '1234.0' in here
      if( fabs(foregrounddB) != std::numeric_limits<float>::infinity() )
         f.AddLine(wxString::Format(_("Average rms = %.1f dB."), foregrounddB ));
      else
         f.AddLine(wxString::Format(_("Average rms = zero.") ));
   else
      f.AddLine(wxString::Format(_("Average rms =  dB.")));
   f.AddLine(wxT(""));
   f.AddLine(_("Background"));
   t = (float)mBackgroundStartT->GetTimeValue();
   h = (int)(t/3600);
   m = (int)((t - h*3600)/60);
   s = t - h*3600.0 - m*60.0;
   f.AddLine(wxString::Format(_("Time started = %2d hour(s), %2d minute(s), %.2f seconds."), h, m, s ));
   t = (float)mBackgroundEndT->GetTimeValue();
   h = (int)(t/3600);
   m = (int)((t - h*3600)/60);
   s = t - h*3600.0 - m*60.0;
   f.AddLine(wxString::Format(_("Time ended = %2d hour(s), %2d minute(s), %.2f seconds."), h, m, s ));
   if(backgrounddB != 1234.0)
      if( fabs(backgrounddB) != std::numeric_limits<float>::infinity() )
         f.AddLine(wxString::Format(_("Average rms = %.1f dB."), backgrounddB ));
      else
         f.AddLine(wxString::Format(_("Average rms = zero.") ));
   else
      f.AddLine(wxString::Format(_("Average rms =  dB.")));
   f.AddLine(wxT(""));
   f.AddLine(_("Results"));
   float diffdB = foregrounddB - backgrounddB;
   if( diffdB != diffdB ) //test for NaN, reliant on IEEE implementation
      f.AddLine(wxString::Format(_("Difference is indeterminate.") ));
   else
      if( fabs(diffdB) != std::numeric_limits<float>::infinity() )
         f.AddLine(wxString::Format(_("Difference = %.1f Average rms dB."), diffdB ));
      else
         f.AddLine(wxString::Format(_("Difference = infinite Average rms dB.")));
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

void ContrastDialog::OnReset(wxCommandEvent & event)
{
   bFGset = false;
   bBGset = false;

   mForegroundStartT->SetTimeValue(0.0);
   mForegroundEndT->SetTimeValue(0.0);
   mBackgroundStartT->SetTimeValue(0.0);
   mBackgroundEndT->SetTimeValue(0.0);

   wxCommandEvent dummyEvt;
   OnGetForegroundDB(event);
   OnGetBackgroundDB(event);
   results();
}

void ContrastDialog::OnChar(wxKeyEvent & event)
{
   if (event.GetKeyCode() == WXK_TAB) {
      event.Skip();
      return;
   }

   event.Skip(false);
   return;
}
