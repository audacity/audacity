/**********************************************************************

  Audacity: A Digital Audio Editor

  ErrorDialog.cpp

  Jimmy Johnson
  Leland Lucius

*******************************************************************//**

\class ErrorDialog
\brief Gives an Error message with an option for help.

*//********************************************************************/


#include "ErrorDialog.h"

#include <wx/app.h>
#include <wx/collpane.h>
#include <wx/icon.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/stattext.h>
#include <wx/html/htmlwin.h>
#include <wx/settings.h>
#include <wx/statusbr.h>
#include <wx/textctrl.h>
#include <wx/artprov.h>

#include "AllThemeResources.h"
#include "CodeConversions.h"
#include "ShuttleGui.h"
#include "HelpText.h"
#include "Prefs.h"
#include "HelpSystem.h"

BEGIN_EVENT_TABLE(ErrorDialog, wxDialogWrapper)
   EVT_COLLAPSIBLEPANE_CHANGED( wxID_ANY, ErrorDialog::OnPane )
   EVT_BUTTON( wxID_OK, ErrorDialog::OnOk)
   EVT_BUTTON( wxID_HELP, ErrorDialog::OnHelp)
END_EVENT_TABLE()

ErrorDialog::ErrorDialog(
   wxWindow *parent,
   const TranslatableString & dlogTitle,
   const TranslatableString & message,
   const ManualPageID & helpPage,
   const std::wstring & log,
   const bool Close, const bool modal)
:  wxDialogWrapper(parent, wxID_ANY, dlogTitle,
                   wxDefaultPosition, wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   SetName();

   long buttonMask;

   // only add the help button if we have a URL
   buttonMask = (helpPage.empty()) ? eOkButton : (eHelpButton | eOkButton);
   dhelpPage = helpPage;
   dClose = Close;
   dModal = modal;

   ShuttleGui S(this, eIsCreating);

   S.SetBorder(2);
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.SetBorder(20);
      wxBitmap bitmap = wxArtProvider::GetBitmap(wxART_WARNING);
      S.AddWindow(safenew wxStaticBitmap(S.GetParent(), -1, bitmap));

      S.SetBorder(20);
      S.AddFixedText(message, false, 500);
   }
   S.EndHorizontalLay();

   S.SetBorder(2);
   if (!log.empty())
   {
      S.StartHorizontalLay(wxEXPAND, 1);
      {
         S.SetBorder(5);

         auto pane = safenew wxCollapsiblePane(S.GetParent(),
                                               wxID_ANY,
                                               XO("Show &Log...").Translation());
         S.Style(wxEXPAND | wxALIGN_LEFT);
         S.Prop(1);
         S.AddWindow(pane);

         ShuttleGui SI(pane->GetPane(), eIsCreating);
         auto text = SI.AddTextWindow(log);
         text->SetInsertionPointEnd();
         text->ShowPosition(text->GetLastPosition());
         text->SetMinSize(wxSize(700, 250));
      }
      S.EndHorizontalLay();
   }

   S.SetBorder(2);
   S.AddStandardButtons(buttonMask);

   Layout();
   GetSizer()->Fit(this);
   SetMinSize(GetSize());
   Center();
}

void ErrorDialog::OnPane(wxCollapsiblePaneEvent & event)
{
   if (!event.GetCollapsed())
   {
      Center();
   }
}

void ErrorDialog::OnOk(wxCommandEvent & WXUNUSED(event))
{
   if (dModal)
      EndModal(true);
   else
      Destroy();
}

void ErrorDialog::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   const auto &str = dhelpPage.GET();
   if( str.StartsWith(wxT("innerlink:")) )
   {
      HelpSystem::ShowHtmlText(
         this,
         TitleText(str.Mid( 10 ) ),
         HelpText( str.Mid( 10 )),
         false,
         true );
      return;
   }
   HelpSystem::ShowHelp( this, dhelpPage, dClose );
   //OpenInDefaultBrowser( dhelpURL );
   if(dClose)
      EndModal(true);
}
