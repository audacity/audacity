/**********************************************************************

  Audacity: A Digital Audio Editor

  SplashDialog.cpp

  James Crook

********************************************************************//**

\class SplashDialog
\brief The SplashDialog shows help information for Audacity when
Audacity starts up.

It was written for the benefit of NEW users who do not want to
read the manual.  The text of the dialog is kept short to increase the
chance of it being read.  The content is designed to reduce the
most commonly asked questions about Audacity.

*//********************************************************************/



#include "SplashDialog.h"



#include <wx/frame.h>
#include <wx/html/htmlwin.h>
#include <wx/statbmp.h>
#include <wx/colour.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>

#include "AllThemeResources.h"
#include "Internat.h"
#include "Theme.h"

#include "FileNames.h"
#include "Project.h"
#include "ProjectWindows.h"
#include "ShuttleGui.h"
#include "AudacityMessageBox.h"
#include "HelpSystem.h"

#include "AllThemeResources.h"
#include "Prefs.h"
#include "HelpText.h"

#include "../images/AudacityLogoWithName.xpm"

#ifdef HAS_WHATS_NEW

#include "MemoryX.h"
#include <wx/fs_mem.h>

const char* WhatsNewURL = "https://audacityteam.org/3.5.0-video";

namespace
{
#   include "../images/WhatsNewBtn.jpeg.h"

struct FSHelper final
{
   FSHelper()
       : mMemoryFSHandler(std::make_unique<wxMemoryFSHandler>())
   {
      wxFileSystem::AddHandler(mMemoryFSHandler.get());

      wxMemoryFSHandler::AddFile(
         "whats_new_btn.jpeg", bin2c_whats_new_btn_jpeg,
         sizeof(bin2c_whats_new_btn_jpeg));
   }

   ~FSHelper()
   {
      wxMemoryFSHandler::RemoveFile("whats_new_btn.jpeg");
      wxFileSystem::RemoveHandler(mMemoryFSHandler.get());
   }

private:
   std::unique_ptr<wxMemoryFSHandler> mMemoryFSHandler;
};

} // namespace

constexpr int HTMLWindowHeight = 425;
#else
constexpr int HTMLWindowHeight = 280;
#endif

namespace
{
wxString MakeWhatsNewText()
{
   // PRL:  Is it necessary to define these outside of conditional compilation
   // so that both get into the .pot file?
   const auto alphamsg = XO(
      "<br><br>The version of Audacity you are using is an <b>Alpha test version</b>.");
   const auto betamsg = XO(
      "<br><br>The version of Audacity you are using is a <b>Beta test version</b>.");

   wxStringOutputStream o;
   wxTextOutputStream s(o);
   s << wxT("<body bgcolor=") << theTheme.Colour(clrTrackInfo).GetAsString(wxC2S_HTML_SYNTAX) << wxT(">")
     << wxT("<font color=") << theTheme.Colour(clrTrackPanelText).GetAsString(wxC2S_HTML_SYNTAX) << wxT(">")
     << wxT("<p>")
#if defined(IS_ALPHA) || defined(IS_BETA)
     << wxT("<center><h3>")
     << XO("Get the Official Released Version of Audacity")
     << wxT("</h3></center>")
     << wxT("<center>[[https://www.audacityteam.org/download|") << XO("Check Online") << wxT("]]</center>")
#   ifdef IS_ALPHA
     << alphamsg
#   else
     << betamsg
#   endif
     << wxT(" ")
     << XO("We strongly recommend that you use our latest stable released version, which has full documentation and support.<br/><br/>")
     << XO("You can help us get Audacity ready for release by joining our [[https://www.audacityteam.org/community/|community]].<hr/><br/><br/>")
#endif

     << wxT("<center><h3>")
#ifndef HAS_WHATS_NEW
     << wxT("Audacity ") << AUDACITY_VERSION_STRING
#else
     /* i18n-hint: %s is replaced with Audacity version */
     << XO("What's new in Audacity %s").Format(AUDACITY_VERSION_STRING)
     << wxT(R"(</h3><p><a href=")") << WhatsNewURL << wxT(R"(">)")
     << wxT(
           R"(<img src="memory:whats_new_btn.jpeg" width="263" height="148" /></a></p>)")
     << XO("<p>In this release we added Cloud saving, tempo detection and more.<br/>")
     << XO("Watch the [[%s|release video]] or read the [[https://support.audacityteam.org/additional-resources/changelog|changelog]] to learn more!</p>").Format(WhatsNewURL)
#endif
     << wxT("<h3>") << XO("How to get help") << wxT("</h3></center>")
     << XO("These are our support methods:")
     << wxT("<p><ul><li>")
     /* i18n-hint: Preserve '[[help:Quick_Help|' as it's the name of a
        link.*/
     << XO("[[help:Quick_Help|Quick Help]]") << wxT("</li><li>")
     << XO(
           /* i18n-hint: Preserve '[[help:Main_Page|' as it's the name of a
              link.*/
           " [[help:Main_Page|Manual]]")
     << wxT("</li><li>")
     << XO("[[https://support.audacityteam.org/|Tutorials & How-tos]]")
     << wxT("</li><li>")
     << XO(" [[https://forum.audacityteam.org/|Forum]] - ask your question directly, online.")
     << wxT("</li></ul></p>") << wxT("</p></font></body>");

   return FormatHtmlText(o.GetString());
}
}

SplashDialog * SplashDialog::pSelf=NULL;

enum
{
   DontShowID=1000,
};

BEGIN_EVENT_TABLE(SplashDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, SplashDialog::OnOK)
   EVT_CHECKBOX( DontShowID, SplashDialog::OnDontShow )
END_EVENT_TABLE()

IMPLEMENT_CLASS(SplashDialog, wxDialogWrapper)

void SplashDialog::DoHelpWelcome( AudacityProject &project )
{
   Show2( &GetProjectFrame( project ) );
}

SplashDialog::SplashDialog(wxWindow * parent)
   :  wxDialogWrapper(parent, -1, XO("Welcome to Audacity!"),
      wxPoint( -1, 60 ), // default x position, y position 60 pixels from top of screen.
      wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   SetName();
   m_pLogo = NULL; //v
   ShuttleGui S( this, eIsCreating );
   Populate( S );
   Fit();
   this->Centre();
   int x,y;
   GetPosition( &x, &y );
   Move( x, 60 );
}

void SplashDialog::OnChar(wxMouseEvent &event)
{
   if ( event.ShiftDown() && event.ControlDown() )
      wxLaunchDefaultBrowser("https://www.audacityteam.org");
}

void SplashDialog::Populate( ShuttleGui & S )
{
   bool bShow;
   gPrefs->Read(wxT("/GUI/ShowSplashScreen"), &bShow, true );
   S.StartVerticalLay(1);

   //v For now, change to AudacityLogoWithName via old-fashioned ways, not Theme.
   m_pLogo = std::make_unique<wxBitmap>((const char **) AudacityLogoWithName_xpm); //v


   //Setup to scale the logo larger and smaller as necessary
   const float fScale=1.0f;
   wxImage RescaledImage( m_pLogo->ConvertToImage() );
   wxColour MainColour( 
      RescaledImage.GetRed(1,1), 
      RescaledImage.GetGreen(1,1), 
      RescaledImage.GetBlue(1,1));
   this->SetBackgroundColour(MainColour);

   // wxIMAGE_QUALITY_HIGH not supported by wxWidgets 2.6.1, or we would use it here.
   RescaledImage.Rescale( (int)(LOGOWITHNAME_WIDTH * fScale), (int)(LOGOWITHNAME_HEIGHT *fScale) );
   wxBitmap RescaledBitmap( RescaledImage );
   wxStaticBitmap *const icon =
       safenew wxStaticBitmap(S.GetParent(), -1,
                          //*m_pLogo, //v theTheme.Bitmap(bmpAudacityLogoWithName),
                          RescaledBitmap,
                          wxDefaultPosition,
                          wxSize((int)(LOGOWITHNAME_WIDTH*fScale), (int)(LOGOWITHNAME_HEIGHT*fScale)));

   S.Prop(0)
#if  (0)
      .ConnectRoot( wxEVT_LEFT_DOWN, &SplashDialog::OnChar)
#endif
      .AddWindow( icon );

   mpHtml = safenew LinkingHtmlWindow(S.GetParent(), -1,
                                         wxDefaultPosition, wxSize(506, HTMLWindowHeight),
                                         wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER );
   mpHtml->SetPage(HelpText( wxT("welcome") ));
   S.Prop(1)
      .Position( wxEXPAND )
      .AddWindow( mpHtml );
   S.Prop(0).StartMultiColumn(2, wxEXPAND);
   S.SetStretchyCol( 1 );// Column 1 is stretchy...
   {
      S.SetBorder( 5 );
      S.Id( DontShowID).AddCheckBox( XXO("Don't show this again at start up"), !bShow );
      S.SetBorder( 5 );

      S.Id(wxID_OK)
         .Prop(0)
         .AddButton(XXO("OK"), wxALIGN_RIGHT| wxALL, true);
   }
   S.EndVerticalLay();
}

SplashDialog::~SplashDialog()
{
}

void SplashDialog::OnDontShow( wxCommandEvent & Evt )
{
   bool bShow = !Evt.IsChecked();
   gPrefs->Write(wxT("/GUI/ShowSplashScreen"), bShow );
   gPrefs->Flush();
}

void SplashDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   Show( false );

}

void SplashDialog::Show2( wxWindow * pParent )
{
#ifdef HAS_WHATS_NEW
   FSHelper helper;
#endif // HAS_WHATS_NEW

   if( pSelf == NULL )
   {
      // pParent owns it
      wxASSERT(pParent);
      pSelf = safenew SplashDialog( pParent );
   }
   pSelf->mpHtml->SetPage(MakeWhatsNewText());
   pSelf->Show( true );
}
