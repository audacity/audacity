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
#include <wx/button.h>
#include <wx/frame.h>
#include <wx/icon.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/utils.h>
#include <wx/html/htmlwin.h>
#include <wx/settings.h>
#include <wx/statusbr.h>
#include <wx/regex.h>

#include "../FileNames.h"
#include "LinkingHtmlWindow.h"
#include "../AllThemeResources.h"
#include "../ShuttleGui.h"
#include "../HelpText.h"
#include "../Prefs.h"
#include "../wxFileNameWrapper.h"

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

namespace {

// Helper class to make browser "simulate" a modal dialog
class HtmlTextHelpDialog final : public BrowserDialog
{
public:
   HtmlTextHelpDialog(wxWindow *pParent, const wxString &title)
      : BrowserDialog{ pParent, title }
   {
#if !wxCHECK_VERSION(3, 0, 0)
      MakeModal( true );
#endif
   }
   virtual ~HtmlTextHelpDialog()
   {
#if !wxCHECK_VERSION(3, 0, 0)
      MakeModal( false );
#endif
      // On Windows, for some odd reason, the Audacity window will be sent to
      // the back.  So, make sure that doesn't happen.
      GetParent()->Raise();
   }
};

}

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
   wxString HelpMode = wxT("Local");

// DA: Default for DA is manual from internet.
#ifdef EXPERIMENTAL_DA
   gPrefs->Read(wxT("/GUI/Help"), &HelpMode, wxT("FromInternet") );
#else
   gPrefs->Read(wxT("/GUI/Help"), &HelpMode, wxT("Local") );
#endif

   {
      // these next lines are for legacy cfg files (pre 2.0) where we had different modes
      if( (HelpMode == wxT("Standard")) || (HelpMode == wxT("InBrowser")) )
      {
         HelpMode = wxT("Local");
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
