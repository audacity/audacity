/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpSystem.cpp

  Jimmy Johnson
  Leland Lucius
  Richard Ash

*//********************************************************************/

#include "../Audacity.h" // for USE_* macros
#include "HelpSystem.h"

#include "../Experimental.h"

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/bmpbuttn.h>
#include <wx/icon.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/utils.h>
#include <wx/html/htmlwin.h>
#include <wx/settings.h>
#include <wx/statusbr.h>
#include <wx/regex.h>

#include "../FileNames.h"
#include "LinkingHtmlWindow.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../ShuttleGui.h"
#include "../HelpText.h"
#include "../Project.h"
#include "../Prefs.h"

#include "ErrorDialog.h"

#ifdef USE_ALPHA_MANUAL
const wxString HelpSystem::HelpHostname = wxT("alphamanual.audacityteam.org");
const wxString HelpSystem::HelpServerHomeDir = wxT("/man/");
const wxString HelpSystem::HelpServerManDir = wxT("/man/");
#else
const wxString HelpSystem::HelpHostname = wxT("manual.audacityteam.org");
const wxString HelpSystem::HelpServerHomeDir = wxT("/");
const wxString HelpSystem::HelpServerManDir = wxT("/man/");
#endif
const wxString HelpSystem::LocalHelpManDir = wxT("/man/");
const wxString HelpSystem::ReleaseSuffix = wxT(".html");

/// Mostly we use this so that we have the code for resizability
/// in one place.  Other considerations like screen readers are also
/// handled by having the code in one place.
void HelpSystem::ShowInfoDialog( wxWindow *parent,
                     const wxString &dlogTitle,
                     const wxString &shortMsg,
                     const wxString &message,
                     const int xSize, const int ySize)
{
   wxDialogWrapper dlog(parent, wxID_ANY,
                dlogTitle,
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX /*| wxDEFAULT_FRAME_STYLE */);

   dlog.SetName(dlog.GetTitle());
   ShuttleGui S(&dlog, eIsCreating);

   S.StartVerticalLay(1);
   {
      S.AddTitle( shortMsg);
      S.SetStyle( wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH | wxTE_RICH2 | 
         wxTE_AUTO_URL | wxTE_NOHIDESEL | wxHSCROLL );
      S.AddTextWindow(message);

      S.SetBorder( 0 );
      S.StartHorizontalLay(wxALIGN_CENTER_HORIZONTAL, 0);
         S.AddStandardButtons(eOkButton);
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   // Smallest size is half default size.  Seems reasonable.
   dlog.SetMinSize( wxSize(xSize/2, ySize/2) );
   dlog.SetSize( wxSize(xSize, ySize) );
   dlog.Center();
   dlog.ShowModal();
}

void HelpSystem::ShowHtmlText(wxWindow *pParent,
                  const wxString &Title,
                  const wxString &HtmlText,
                  bool bIsFile,
                  bool bModal)
{
   LinkingHtmlWindow *html;

   wxASSERT(pParent); // to justify safenew
   // JKC: ANSWER-ME: Why do we create a fake 'frame' and then put a BrowserDialog
   // inside it, rather than have a variant of the BrowserDialog that is a
   // frame??
   // Bug 1412 seems to be related to the extra frame.
   auto pFrame = safenew wxFrame {
      pParent, wxID_ANY, Title, wxDefaultPosition, wxDefaultSize,
#if defined(__WXMAC__)
      // On OSX, the html frame can go behind the help dialog and if the help
      // html frame is modal, you can't get back to it.  Pressing escape gets
      // you out of this, but it's just easier to add the wxSTAY_ON_TOP flag
      // to prevent it from falling behind the dialog.  Not the perfect solution
      // but acceptable in this case.
      wxSTAY_ON_TOP |
#endif
      wxDEFAULT_FRAME_STYLE
   };

   BrowserDialog * pWnd;
   if( bModal )
      pWnd = safenew HtmlTextHelpDialog{ pFrame, Title };
   else
      pWnd = safenew BrowserDialog{ pFrame, Title };

   // Bug 1412 workaround for 'extra window'.  Hide the 'fake' window.
   pFrame->SetTransparent(0);
   ShuttleGui S( pWnd, eIsCreating );

   S.SetStyle( wxNO_BORDER | wxTAB_TRAVERSAL );
   wxPanel *pPan = S.Prop(true).StartPanel();
   {
      S.StartHorizontalLay( wxEXPAND, false );
      {
         wxButton * pWndBackwards = S.Id( wxID_BACKWARD ).AddButton( _("<") );
         wxButton * pWndForwards  = S.Id( wxID_FORWARD  ).AddButton( _(">") );
         pWndForwards->Enable( false );
         pWndBackwards->Enable( false );
         #if wxUSE_TOOLTIPS
         pWndForwards->SetToolTip( _("Forwards" ));
         pWndBackwards->SetToolTip( _("Backwards" ));
         #endif
      }
      S.EndHorizontalLay();

      html = safenew LinkingHtmlWindow(pPan, wxID_ANY,
                                   wxDefaultPosition,
                                   bIsFile ? wxSize(500, 400) : wxSize(480, 240),
                                   wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER);

      html->SetRelatedFrame( pFrame, wxT("Help: %s") );
      if( bIsFile )
         html->LoadFile( HtmlText );
      else
         html->SetPage( HtmlText);

      S.Prop(1).AddWindow( html, wxEXPAND );

      S.Id( wxID_CANCEL ).AddButton( _("Close") )->SetDefault();
   }
   S.EndPanel();

   // -- START of ICON stuff -----
   // If this section (providing an icon) causes compilation errors on linux, comment it out for now.
   // it will just mean that the icon is missing.  Works OK on Windows.
   #ifdef __WXMSW__
   wxIcon ic{ wxICON(AudacityLogo) };
   #else
   wxIcon ic{};
      ic.CopyFromBitmap(theTheme.Bitmap(bmpAudacityLogo48x48));
   #endif
   pFrame->SetIcon( ic );
   // -- END of ICON stuff -----


   pWnd->mpHtml = html;
   pWnd->SetBackgroundColour( wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));

   pFrame->CreateStatusBar();
   pFrame->Centre();
   pFrame->Layout();
   pFrame->SetSizeHints(pWnd->GetSize());

   pFrame->SetName(Title);
   if (bModal)
      pWnd->ShowModal();
   else {
      pWnd->Show(true);
      pFrame->Show(true);
   }

   html->SetRelatedStatusBar( 0 );
   html->SetFocus();

   return;
}

// Shows help in browser, or possibly in own dialog.
void HelpSystem::ShowHelp(wxWindow *parent,
                    const wxString &localFileName,
                    const wxString &remoteURL,
                    bool bModal,
                    bool alwaysDefaultBrowser)
{
   wxASSERT(parent); // to justify safenew
   AudacityProject * pProj = GetActiveProject();
   wxString HelpMode = wxT("Local");

   if( pProj )
   {
      HelpMode = pProj->mHelpPref;
      // these next lines are for legacy cfg files (pre 2.0) where we had different modes
      if( (HelpMode == wxT("Standard")) || (HelpMode == wxT("InBrowser")) )
      {
         HelpMode = wxT("Local");
         pProj->mHelpPref = HelpMode;
         gPrefs->Write(wxT("/GUI/Help"), HelpMode);
         gPrefs->Flush();
      }
   }

   // Anchors (URLs with a '#' in them) are not supported by many OSs for local file names
   // See, for example, https://groups.google.com/forum/#!topic/wx-users/pC0uOZJalRQ
   // Problems have been reported on Win, Mac and some versions of Linux.
   // So we set HelpMode to use the internet if an anchor is found.
   if (localFileName.Find('#', true) != wxNOT_FOUND)
      HelpMode = wxT("FromInternet");
   // Until a solution is found for this, the next few lines are irrelevant.

   // Obtain the local file system file name, without the anchor if present.
   wxString localfile;
   if (localFileName.Find('#', true) != wxNOT_FOUND)
      localfile = localFileName.BeforeLast('#');
   else
      localfile = localFileName;

   if( (HelpMode == wxT("FromInternet")) && !remoteURL.empty() )
   {
      // Always go to remote URL.  Use External browser.
      OpenInDefaultBrowser( remoteURL );
   }
   else if( localfile.empty() || !wxFileExists( localfile ))
   {
      // If you give an empty remote URL, you should have already ensured
      // that the file exists!
      wxASSERT( !remoteURL.empty() );
      // I can't find it'.
      // Use Built-in browser to suggest you use the remote url.
      wxString Text = HelpText( wxT("remotehelp") );
      Text.Replace( wxT("*URL*"), remoteURL );
      ShowHtmlText( parent, _("Help on the Internet"), Text, false, bModal );
   }
   else if( HelpMode == wxT("Local") || alwaysDefaultBrowser)
   {
      // Local file, External browser
      OpenInDefaultBrowser( wxString(wxT("file:"))+localFileName );
   }
   else
   {
      // Local file, Built-in browser
      ShowHtmlText( parent, wxT(""), localFileName, true, bModal );
   }
}

void HelpSystem::ShowHelp(wxWindow *parent,
                          const wxString &PageName,
                          bool bModal)
{
   wxString localHelpPage;
   wxString webHelpPath;
   wxString webHelpPage;
   wxString releasePageName;
   wxString anchor;	// optional part of URL after (and including) the '#'
   if (PageName.Find('#', true) != wxNOT_FOUND)
   {	// need to split anchor off into separate variable
      releasePageName= PageName.BeforeLast('#');
      anchor = wxT("#") + PageName.AfterLast('#');
   }
   else
   {
      releasePageName = PageName;
      anchor = wxT("");
   }
   // The wiki pages are transformed to static HTML by
   // scripts/mw2html_audacity/mw2html.py
   // The name is first transformed to lower case, then all
   // 'special characters' are replaced by underscores. Spaces are
   // transformed to "+".
   //
   // The transformations are handled in mw2html by first applying
   // 'urllib.parse.quote_plus' (escape chars that are not in "always safe" list)
   // then replacing escape characters (%xx) with underscores,
   // and finally removing duplicate / redundant underscores.
   //
   // The front page and 'quick_help' are treated as special cases and placed in
   // the root of the help directory rather than the "/man/" sub-directory.
   if (releasePageName == wxT("Main_Page"))
   {
      releasePageName = wxT("index") + HelpSystem::ReleaseSuffix + anchor;
      localHelpPage = wxFileName(FileNames::HtmlHelpDir(), releasePageName).GetFullPath();
      webHelpPath = wxT("https://")+HelpSystem::HelpHostname+HelpSystem::HelpServerHomeDir;
   }
   else if (releasePageName == wxT("Quick_Help"))
   {
// DA: No bundled help, by default, and different quick-help URL.
#ifdef EXPERIMENTAL_DA
      releasePageName = wxT("video") + HelpSystem::ReleaseSuffix + anchor;
      localHelpPage = wxFileName(FileNames::HtmlHelpDir(), releasePageName).GetFullPath();
      webHelpPath = wxT("http://www.darkaudacity.com/");
#else
      releasePageName = wxT("quick_help") + HelpSystem::ReleaseSuffix + anchor;
      localHelpPage = wxFileName(FileNames::HtmlHelpDir(), releasePageName).GetFullPath();
      webHelpPath = wxT("https://")+HelpSystem::HelpHostname+HelpSystem::HelpServerHomeDir;
#endif
   }
   // not a page name, but rather a full path (e.g. to wiki)
   // in which case do not do any substitutions.
   else if (releasePageName.StartsWith( "http" ) )
   {
      localHelpPage = "";
      webHelpPage = releasePageName + anchor;
   }
   else
   {
      // Handle all other pages.
      // Change to lower case.
      releasePageName = releasePageName.Lower();
      wxRegEx re;
      // replace 'special characters' with underscores.
      // RFC 2396 defines the characters a-z, A-Z, 0-9 and ".-_" as "always safe"
      // mw2html also replaces "-" with "_" so replace that too.
      
      // If PageName contains a %xx code, mw2html will transform it:
      // '%xx' => '%25xx' => '_'
      re.Compile(wxT("%.."));
      re.ReplaceAll(&releasePageName, (wxT("_")));
      // Now replace all other 'not-safe' characters.
      re.Compile(wxT("[^[:alnum:] . [:space:]]"));
      re.ReplaceAll(&releasePageName, (wxT("_")));
      // Replace spaces with "+"
      releasePageName.Replace(wxT(" "), wxT("+"), true);
      // Reduce multiple underscores to single underscores
      re.Compile(wxT("__+"));
      re.ReplaceAll(&releasePageName, (wxT("_")));
      // Replace "_." with "."
      releasePageName.Replace(wxT("_."), wxT("."), true);
      // Concatenate file name with file extension and anchor.
      releasePageName = releasePageName + HelpSystem::ReleaseSuffix + anchor;
      // Other than index and quick_help, all local pages are in subdirectory 'LocalHelpManDir'.
      localHelpPage = wxFileName(FileNames::HtmlHelpDir() + LocalHelpManDir, releasePageName).GetFullPath();
      // Other than index and quick_help, all on-line pages are in subdirectory 'HelpServerManDir'.
      webHelpPath = wxT("https://")+HelpSystem::HelpHostname+HelpSystem::HelpServerManDir;
   }

#ifdef USE_ALPHA_MANUAL
   webHelpPage = webHelpPath + PageName;
#else
   webHelpPage = webHelpPath + releasePageName;
#endif

   wxLogMessage(wxT("Help button pressed: PageName %s, releasePageName %s"),
              PageName, releasePageName);
   wxLogMessage(wxT("webHelpPage %s, localHelpPage %s"),
              webHelpPage, localHelpPage);

   wxASSERT(parent); // to justify safenew

   HelpSystem::ShowHelp(
      parent, 
      localHelpPage,
      webHelpPage,
      bModal
      );
}

#include "../ShuttleGui.h"
// These three are all for the OnReloadPreferences command.
#include "../Project.h"
#include "../commands/CommandContext.h"
#include "../Menus.h"

#define FixButtonID           7001
#define HelpButtonID          7011
#define FakeButtonID          7021

BEGIN_EVENT_TABLE(QuickFixDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK,                                            QuickFixDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL,                                        QuickFixDialog::OnCancel)
   EVT_BUTTON(wxID_HELP,                                          QuickFixDialog::OnHelp)
   EVT_COMMAND_RANGE(FixButtonID,  HelpButtonID-1, wxEVT_BUTTON,  QuickFixDialog::OnFix)
   EVT_COMMAND_RANGE(HelpButtonID, FakeButtonID-1, wxEVT_BUTTON,  QuickFixDialog::OnHelp)
END_EVENT_TABLE();

QuickFixDialog::QuickFixDialog(wxWindow * pParent) :
      wxDialogWrapper(pParent, wxID_ANY, _("Do you have these problems?"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE )
{
   const long SNAP_OFF = 0;

   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &mbSyncLocked, false);
   mbInSnapTo = gPrefs->Read(wxT("/SnapTo"), SNAP_OFF) !=0;
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"), &mbSoundActivated, false);

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);

   Fit();
   auto sz = GetSize();
   SetMinSize( sz );
   SetMaxSize( sz );

   // The close button has the cancel id and acts exactly the same as cancel.
   wxButton * pWin = (wxButton*)FindWindowById( wxID_CANCEL );
   if( pWin )
      pWin->SetFocus( );
   Center();
}

void QuickFixDialog::AddStuck( ShuttleGui & S, bool & bBool, wxString Pref, wxString Prompt, wxString Help )
{
   mItem++;
   if( !bBool)
      return;
   S.AddFixedText( Prompt );
   S.Id(FixButtonID + mItem).AddButton( _("Fix") )->SetClientObject(
      safenew wxStringClientData(Pref));

   {
     // Replace standard Help button with smaller icon button.
      // bs->AddButton(safenew wxButton(parent, wxID_HELP));
      auto b = safenew wxBitmapButton(S.GetParent(), HelpButtonID+mItem, theTheme.Bitmap( bmpHelpIcon ));
      b->SetToolTip( _("Help") );
      b->SetLabel(_("Help"));       // for screen readers
      b->SetClientObject( safenew wxStringClientData( Help ));
      S.AddWindow( b );
   }
}

void QuickFixDialog::PopulateOrExchange(ShuttleGui & S)
{

   S.StartVerticalLay(1);
   S.StartStatic( _("Quick Fixes"));

   // These aren't all possible modes one can be stuck in, but they are some of them.
   bool bStuckInMode = mbSyncLocked || mbInSnapTo || mbSoundActivated;

   if( !bStuckInMode ){
      SetLabel(_("Nothing to do"));
      S.AddFixedText(_("No quick, easily fixed problems were found"));
   }
   else {
      S.StartMultiColumn(3, wxALIGN_CENTER);
      {
         mItem = -1;

         // Use # in the URLs to ensure we go to the online version of help.
         // Local help may well not be installed.
         AddStuck( S, mbSyncLocked, "/GUI/SyncLockTracks", _("Clocks on the Tracks"), "Quick_Fix#sync_lock" );
         AddStuck( S, mbInSnapTo, "/SnapTo", _("Can't select precisely"), "Quick_Fix#snap_to" );
         AddStuck( S, mbSoundActivated, "/AudioIO/SoundActivatedRecord", _("Recording stops and starts"), "Quick_Fix#sound_activated_recording" );
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartHorizontalLay(wxALIGN_CENTER_HORIZONTAL, 0);
      S.AddStandardButtons(eCloseButton + (bStuckInMode ? 0 : eHelpButton));
   S.EndHorizontalLay();

   S.EndVerticalLay();

   wxButton * pBtn = (wxButton*)FindWindowById( wxID_HELP );
   if( pBtn )
      pBtn->SetClientObject( safenew wxStringClientData( "Quick_Fix#" ));

}

void QuickFixDialog::OnOk(wxCommandEvent &event)
{
   (void)event;// Compiler food
   EndModal(wxID_OK);
}

void QuickFixDialog::OnCancel(wxCommandEvent &event)
{
   (void)event;// Compiler food
   EndModal(wxID_CANCEL);
}

wxString QuickFixDialog::StringFromEvent( wxCommandEvent &event )
{
   wxButton * pBtn = (wxButton*)event.GetEventObject();
   if( !pBtn ){
      wxFAIL_MSG( "Event Object not found");
      return "";
   }
   wxStringClientData * pStrCd = (wxStringClientData*)(pBtn->GetClientObject());
   if( !pStrCd ){
      wxFAIL_MSG( "Client Data not found");
      return "";
   }
   wxString Str = pStrCd->GetData();
   if( Str.empty()){
      wxFAIL_MSG( "String data empty");
      return "";
   }
   return Str;
}

void QuickFixDialog::OnHelp(wxCommandEvent &event)
{
   HelpSystem::ShowHelp(this, StringFromEvent( event ), true);
}

void QuickFixDialog::OnFix(wxCommandEvent &event)
{
   wxString Str = StringFromEvent( event );
   gPrefs->Write( Str, 0);
   gPrefs->Flush();

   if ( auto pProject = GetActiveProject() ) {
      // Sadly SnapTo has to be handled specially, as it is not part of the standard
      // preference dialogs.
      if( Str == "/SnapTo" )
      {
         pProject->SetSnapTo( 0 );
      }
      else
      {
         // This is overkill (aka slow), as all preferences are reloaded and all 
         // toolbars recreated.
         // Overkill probably doesn't matter, as this command is infrequently used.
         EditActions::DoReloadPreferences( *pProject );
      }
   }
   
   // Change the label after doing the fix, as the fix may take a second or two.
   wxButton * pBtn = (wxButton*)event.GetEventObject();
   if( pBtn )
      pBtn->SetLabel( _("Fixed") );

   // The close button has the cancel id and acts exactly the same as cancel.
   wxButton * pWin = (wxButton*)FindWindowById( wxID_CANCEL );
   if( pWin )
      pWin->SetFocus( );
}
