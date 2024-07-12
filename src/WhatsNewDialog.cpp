/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WhatsNewDialog.cpp

  Vitaly Sverchinsky

**********************************************************************/

#include "WhatsNewDialog.h"

#include <wx/fs_mem.h>
#include <wx/settings.h>
#include <wx/mstream.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>
#include <wx/hyperlink.h>
#include <wx/checkbox.h>
#include <wx/frame.h>
#include <wx/statbmp.h>
#include <wx/stattext.h>

#include "HelpSystem.h"
#include "HelpText.h"
#include "ProjectWindows.h"
#include "ShuttleGui.h"
#include "Theme.h"
#include "AllThemeResources.h"

#include "../images/WhatsNewBtn.jpeg.h"
#include "../images/MuseHub.jpeg.h"

namespace
{

#if defined (__WXOSX__) || defined(__WXMSW__)
#  define SHOW_MUSEHUB
#endif

const char* WhatsNewURL = "https://audacityteam.org/3.6.0-video";
const char* MuseHubURL = "https://www.musehub.com";

#if defined(SHOW_MUSEHUB)
   constexpr auto WindowWidth = 720;
#else
   constexpr auto WindowWidth = 400;
#endif

}
AttachedWindows::RegisteredFactory sWhatsNewWindow{
   []( AudacityProject &project ) -> wxWeakRef< wxWindow > {
      auto &window = GetProjectFrame(project);
      return safenew WhatsNewDialog(&window, wxID_ANY);
   }
};


namespace
{

struct FSHelper final
{
   FSHelper()
       : mMemoryFSHandler(std::make_unique<wxMemoryFSHandler>())
   {
      wxFileSystem::AddHandler(mMemoryFSHandler.get());

      wxMemoryFSHandler::AddFile(
         "whats_new_btn.jpeg", bin2c_whats_new_btn_jpeg,
         sizeof(bin2c_whats_new_btn_jpeg));
      wxMemoryFSHandler::AddFile(
         "musehub.jpeg", bin2c_musehub_jpeg,
         sizeof(bin2c_musehub_jpeg));
   }

   ~FSHelper()
   {
      wxMemoryFSHandler::RemoveFile("whats_new_btn.jpeg");
      wxMemoryFSHandler::RemoveFile("musehub.jpeg");
      wxFileSystem::RemoveHandler(mMemoryFSHandler.get());
   }

private:
   std::unique_ptr<wxMemoryFSHandler> mMemoryFSHandler;
};

wxString MakeWhatsNewText()
{
   wxStringOutputStream o;
   wxTextOutputStream s(o);
   s
      << wxT("<body>")
      << wxT("<p><center>")
      << wxT(R"(<p><a href=")") << WhatsNewURL << wxT(R"(">)")
      // Bug: (Windows) specified width and height should match exactly to the size of the image
      << wxT(R"(<img src="memory:whats_new_btn.jpeg" width="263" height="148" /><br></a></p>)")
      << wxT("<h3>") << XO("What's new in Audacity %s").Format(AUDACITY_VERSION_STRING) << wxT("</h3>")
      << wxT("<p>")
      << XO("Watch the [[%s|release video]] or read the [[https://support.audacityteam.org/additional-resources/changelog|changelog]] to learn more!</p>").Format(WhatsNewURL);

   return FormatHtmlText(o.GetString());
}

wxString MakeGetPluginsText()
{
   wxStringOutputStream o;
   wxTextOutputStream s(o);
   s
      << wxT("<body>")
      << wxT("<p><center>")
      << wxT(R"(<p><a href=")") << MuseHubURL << wxT(R"(">)")
      // Bug: (Windows) specified width and height should match exactly to the size of the image
      << wxT(R"(<img src="memory:musehub.jpeg" width="263" height="148" /><br></a></p>)")
      << wxT("<h3>") << XO("Get free plugins & sounds")<< wxT("</h3>")
      << XO("<p>Check out our [[%s|Muse Hub app]] for a wide range of audio plugins for Audacity users</p>").Format(MuseHubURL);

   return FormatHtmlText(o.GetString());
}
}

BEGIN_EVENT_TABLE(WhatsNewDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, WhatsNewDialog::OnOK)
END_EVENT_TABLE()

WhatsNewDialog::WhatsNewDialog(wxWindow* parent, wxWindowID id)
   : wxDialogWrapper(parent, id, XO("Welcome to Audacity!"))
{
#if defined(__WXOSX__)
   SetSize(WindowWidth, 400);
#else
   SetSize(FromDIP(wxSize(WindowWidth, 430)));
#endif

#if defined(__WXMSW__)
   //On Windows UI controls doesn't use same theme preference
   //as per application, we should use the latter one to get 
   //match with LinkingHtmlWindow's theme
   SetBackgroundColour(theTheme.Colour(clrMedium));
#endif
   SetName();
   ShuttleGui S( this, eIsCreating );
   Populate( S );
   Centre();
}

WhatsNewDialog::~WhatsNewDialog() = default;

void WhatsNewDialog::Show(AudacityProject& project)
{
   auto dialog = &GetAttachedWindows(project)
      .Get<WhatsNewDialog>(sWhatsNewWindow);
   dialog->CenterOnParent();
   dialog->wxDialogWrapper::Show();
}

void WhatsNewDialog::Populate(ShuttleGui& S)
{
   bool showSplashScreen;
   gPrefs->Read(wxT("/GUI/ShowSplashScreen"), &showSplashScreen, true );

   FSHelper helper;

   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetBorder(20);
      const auto whatsnew = safenew LinkingHtmlWindow(S.GetParent());
      whatsnew->SetPage(MakeWhatsNewText());
      whatsnew->SetBackgroundColour(S.GetParent()->GetBackgroundColour());
      S
         .Prop(1)
#if defined(SHOW_MUSEHUB)
         .Position(wxEXPAND | wxLEFT | wxTOP | wxBOTTOM)
#else
         .Position(wxEXPAND | wxALL)
#endif
         .AddWindow(whatsnew);

#if defined(SHOW_MUSEHUB)
      S.AddSpace(20);

      const auto getplugins = safenew LinkingHtmlWindow(S.GetParent());
      getplugins->SetPage(MakeGetPluginsText());
      getplugins->SetBackgroundColour(S.GetParent()->GetBackgroundColour());
      S
         .Prop(1)
         .Position(wxEXPAND | wxTOP | wxRIGHT | wxBOTTOM)
         .AddWindow(getplugins);
#endif
   }
   S.EndHorizontalLay();

   const auto line = safenew wxWindow(S.GetParent(), wxID_ANY);
   line->SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNSHADOW));
   line->SetSize(-1, 1);

   S
      .Prop(0)
      .Position(wxEXPAND)
      .AddWindow(line);

   S.StartHorizontalLay(wxALIGN_CENTRE, 0);
   {
      S.SetBorder(10);
      const auto tutorialsLink = safenew wxHyperlinkCtrl(
         S.GetParent(),
         wxID_ANY,
         _("View tutorials"),
         "https://support.audacityteam.org/");
      S
         .Position(wxTOP | wxBOTTOM)
         .AddWindow(tutorialsLink);

      S.AddSpace(25);

      const auto forumLink = safenew wxHyperlinkCtrl(
         S.GetParent(),
         wxID_ANY,
         _("Visit our forum"),
         "https://forum.audacityteam.org/");
      S
         .Position(wxTOP | wxBOTTOM)
         .AddWindow(forumLink);
   }
   S.EndHorizontalLay();

   S.Position(wxEXPAND).StartPanel(2);
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetBorder(4);
         mDontShowAgain = S
            .Position(wxALL | wxALIGN_CENTRE)
            .AddCheckBox( XXO("Don't show this again at start up"), !showSplashScreen);
         
         S.AddSpace(1,1,1);

         S
            .Id(wxID_OK)
            .AddButton(XXO("OK"), wxALL, true);
      }
      S.EndHorizontalLay();
   }
   S.EndPanel();
}

void WhatsNewDialog::OnOK(wxCommandEvent& evt)
{
   gPrefs->Write(wxT("/GUI/ShowSplashScreen"), !mDontShowAgain->IsChecked() );
   gPrefs->Flush();
   wxDialogWrapper::Show(false);
}
