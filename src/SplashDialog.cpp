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


#include "Audacity.h"

#include <wx/dialog.h>
#include <wx/html/htmlwin.h>
#include <wx/button.h>
#include <wx/dcclient.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/intl.h>
#include <wx/image.h>

#include "SplashDialog.h"
#include "FileNames.h"
#include "Internat.h"
#include "ShuttleGui.h"
#include "widgets/LinkingHtmlWindow.h"

#include "Theme.h"
#include "AllThemeResources.h"
#include "Prefs.h"
#include "HelpText.h"

#include "../images/AudacityLogoWithName.xpm"

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

SplashDialog::SplashDialog(wxWindow * parent)
   :  wxDialogWrapper(parent, -1, _("Welcome to Audacity!"),
      wxPoint( -1, 60 ), // default x position, y position 60 pixels from top of screen.
      wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   SetName(GetTitle());
   this->SetBackgroundColour(theTheme.Colour( clrAboutBoxBackground ));
   m_pLogo = NULL; //v
   ShuttleGui S( this, eIsCreating );
   Populate( S );
   Fit();
   this->Centre();
   int x,y;
   GetPosition( &x, &y );
   Move( x, 60 );
}

void SplashDialog::Populate( ShuttleGui & S )
{
   this->SetBackgroundColour(theTheme.Colour( clrAboutBoxBackground ));
   bool bShow;
   gPrefs->Read(wxT("/GUI/ShowSplashScreen"), &bShow, true );
   S.StartVerticalLay(1);

   //v For now, change to AudacityLogoWithName via old-fashioned ways, not Theme.
   m_pLogo = std::make_unique<wxBitmap>((const char **) AudacityLogoWithName_xpm); //v

   // JKC: Resize to 50% of size.  Later we may use a smaller xpm as
   // our source, but this allows us to tweak the size - if we want to.
   // It also makes it easier to revert to full size if we decide to.
   const float fScale=0.5f;// smaller size.
   wxImage RescaledImage( m_pLogo->ConvertToImage() );
   // wxIMAGE_QUALITY_HIGH not supported by wxWidgets 2.6.1, or we would use it here.
   RescaledImage.Rescale( (int)(LOGOWITHNAME_WIDTH * fScale), (int)(LOGOWITHNAME_HEIGHT *fScale) );
   wxBitmap RescaledBitmap( RescaledImage );
   wxStaticBitmap *const icon =
       safenew wxStaticBitmap(S.GetParent(), -1,
                          //*m_pLogo, //v theTheme.Bitmap(bmpAudacityLogoWithName),
                          RescaledBitmap,
                          wxDefaultPosition,
                          wxSize((int)(LOGOWITHNAME_WIDTH*fScale), (int)(LOGOWITHNAME_HEIGHT*fScale)));

   S.Prop(0).AddWindow( icon );

   mpHtml = safenew LinkingHtmlWindow(S.GetParent(), -1,
                                         wxDefaultPosition,
                                         wxSize(506, 280),
                                         wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER );
   mpHtml->SetPage(HelpText( wxT("welcome") ));
   S.Prop(1).AddWindow( mpHtml, wxEXPAND );
   S.Prop(0).StartMultiColumn(2, wxEXPAND);
   S.SetStretchyCol( 1 );// Column 1 is stretchy...
   {
      S.SetBorder( 5 );
      S.Id( DontShowID).AddCheckBox( _("Don't show this again at start up"), bShow ? wxT("false") : wxT("true") );
      wxButton *ok = safenew wxButton(S.GetParent(), wxID_OK);
      ok->SetDefault();
      S.SetBorder( 5 );
      S.Prop(0).AddWindow( ok, wxALIGN_RIGHT| wxALL );
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
   if( pSelf == NULL )
   {
      // pParent owns it
      wxASSERT(pParent);
      pSelf = safenew SplashDialog( pParent );
   }
   pSelf->mpHtml->SetPage(HelpText( wxT("welcome") ));
   pSelf->Show( true );
}
