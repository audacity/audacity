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
#include "GradientButton.h"
#include "HyperLink.h"

#include "../images/ACE.jpg.h"
#include "../images/Audacity_3.7.2_Thumb.jpg.h"
#include "../images/Cloud.xpm"
#include "../images/Cloud_low_res.xpm"

namespace
{

#if defined (__WXOSX__) || defined(__WXMSW__)
#  define SHOW_MUSEHUB
#endif

enum {
   WhatsNewID_WatchReleaseVideo = wxID_HIGHEST + 1,
   WhatsNewID_GoToMuseHub,
   WhatsNewID_GoToAudioCom,
};

const char* WhatsNewURL = "https://audacityteam.org/3.7.0-video";
const char* ChangeLogURL = "https://support.audacityteam.org/additional-resources/changelog";
//const char* MuseHubURL = "https://www.audacityteam.org/mh-whatsnew";
const char* MuseHubURL = "https://www.musehub.com/app/ace-studio"; //! TODO AU3: update ACE URL
const char* PromoURL = "https://audacityteam.org/audacitypromo";
const char* AudioComURL = "https://audio.com/auth/sign-in"; //! TODO AU3:  update cloud URL

#if defined(SHOW_MUSEHUB)
   constexpr auto WindowWidth = 800;
#else
   constexpr auto WindowWidth = 500;
#endif

// wxHTML renders text with smaller line spacing on macOS
#if defined (__WXOSX__)
constexpr auto WindowHeight = 636;
#elif defined (__WXMSW__)
constexpr auto WindowHeight = 676;
# else
constexpr auto WindowHeight = 450;
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
         "audacity_3_7_2_thumb.jpeg", bin2c_Audacity_3_7_2_Thumb_jpg,
         sizeof(bin2c_Audacity_3_7_2_Thumb_jpg));
      wxMemoryFSHandler::AddFile(
         "ace.jpeg", bin2c_ACE_jpg,
         sizeof(bin2c_ACE_jpg));
   }

   ~FSHelper()
   {
      wxMemoryFSHandler::RemoveFile("audacity_3_7_2_thumb.jpeg");
      wxMemoryFSHandler::RemoveFile("ace.jpeg");
      wxFileSystem::RemoveHandler(mMemoryFSHandler.get());
   }

private:
   std::unique_ptr<wxMemoryFSHandler> mMemoryFSHandler;
};

wxString MakeSignUpToCloudText()
{
   wxStringOutputStream o;
   wxTextOutputStream s(o);
   s
      << wxT("<body>")
      << wxT("<p><center>")
#if defined (__WXOSX__) || defined(__WXMSW__)
      << wxT("<h3>")
#else
      << wxT("<p>")
#endif
      << XO("<font color=\"#000000\">")
      << XO("Sign up to Audacity's cloud saving platform")
      << wxT("<br>") << XO("and access your projects from anywhere!")
      << XO("</font>")
#if defined (__WXOSX__) || defined(__WXMSW__)
      << wxT("</h3>");
#else
      << wxT("</p>");
#endif
   
   return FormatHtmlText(o.GetString());
}

wxString MakeWhatsNewText()
{
   wxStringOutputStream o;
   wxTextOutputStream s(o);
   s
      << wxT("<body>")
      << wxT("<p><center>")
      << wxT(R"(<p><a href=")") << WhatsNewURL << wxT(R"(">)")
      // Bug: (Windows) specified width and height should match exactly to the size of the image
#if 0
   << wxT(R"(<img src="memory:whats_new_btn.jpeg" width="352" height="198" /><br></a></p>)")
   << wxT("<h3>") << XO("What's new in Audacity %s").Format(AUDACITY_VERSION_STRING) << wxT("</h3>")
   << wxT("<p>")
   << XO("Watch the [[%s|release video]] or read the [[%s|changelog]] to learn more about what we have included in the latest release!</p>").Format(WhatsNewURL, ChangeLogURL);
#endif
      << wxT(R"(<img src="memory:audacity_3_7_2_thumb.jpeg" width="352" height="198" /><br></a></p>)")
      << wxT("<h3>") << XO("What's new in Audacity") << wxT("</h3>")
      << wxT("<p>")
      << XO("You can also read the [[%s|changelog]]</p>").Format(ChangeLogURL);

   return FormatHtmlText(o.GetString());
}

wxString MakeGetPluginsText()
{
   wxStringOutputStream o;
   wxTextOutputStream s(o);
   s
      << wxT("<body>")
      << wxT("<p><center>")
      << wxT(R"(<p><a href=")") << PromoURL << wxT(R"(">)")
      // Bug: (Windows) specified width and height should match exactly to the size of the image
      << wxT(R"(<img src="memory:ace.jpeg" width="352" height="198" /><br></a></p>)")
#if 0
      //we want to keep these strings, but not display them at the moment
      << wxT("<h3>") << XO("Get free plugins & sounds")<< wxT("</h3>")
      << XO("<p>Check out our [[%s|Muse Hub app]] for a wide range of audio plugins for Audacity users</p>").Format(MuseHubURL);

      /*i18n-hint: MuseHub is a name of a product, see musehub.com. */
      // << XO("Available on [[%s|MuseHub]].").Format(MuseHubURL) << wxT("</p>");

      //we also may need these strings in the future. Adding them to the catalogue now so we don't have to send out multiple translation CTAs

      << wxT("<h3>") << wxT("VocalStrip - Solid State Logic")<< wxT("</h3>")
      << wxT("<p>") << XO("A vocal processing effect that enhances vocal clarity and depth.") << wxT(" ")
      << wxT("<h3>") << wxT("Crystalline Next-gen Reverb")<< wxT("</h3>")
      << wxT("<p>") << XO("A multipurpose reverb plugin for spacious, natural-sounding effects.") << wxT(" ")
      << wxT("<h3>") << wxT("Ampkit Guitar Modeller")<< wxT("</h3>")
      << wxT("<p>") << XO("A guitar amp and effects modelling plugin for realistic tones and customizable sounds.") << wxT(" ")
      << wxT("<h3>") << wxT("BOREALIS Dynamic Reverb")<< wxT("</h3>")
      << wxT("<p>") << XO("A responsive reverb plugin that adapts to track dynamics for immersive soundscapes.") << wxT(" ")
      << wxT("<h3>") << wxT("LANDR FX Voice")<< wxT("</h3>")
      << wxT("<p>") << XO("A vocal multi-effect plugin that enhances vocal tracks for professional results.") << wxT(" ")
      << wxT("<h3>") << wxT("LANDR FX Bass")<< wxT("</h3>")
      << wxT("<p>") << XO("A bass effect plugin designed to deliver punchy, clear low-end sound.") << wxT(" ")
      << wxT("<h3>") << wxT("LANDR FX Acoustic:")<< wxT("</h3>")
      << wxT("<p>") << XO("A plugin for acoustic sound perfection, enhancing warmth and clarity.") << wxT(" ")
      << wxT("<h3>") << wxT("Pristine Voice")<< wxT("</h3>")
      << wxT("<p>") << XO("An amazing voice enhancement plugin for crystal-clear, professional vocal effects.") << wxT(" ")
      << wxT("<h3>") << wxT("Remix Source Separation")<< wxT("</h3>")
      << wxT("<p>") << XO("A real-time stem separation tool to isolate vocals, drums, and instruments.") << wxT(" ")
      << wxT("<h3>") << wxT("Recommended effects plugins for Audacity")<< wxT("</h3>")
      << wxT("<p>") << XO("Check out a variety of plugins by well known developers available on [[%s|MuseHub]].").Format(MuseHubURL)
      << wxT("<h3>") << wxT("Polyspectral Multiband Compressor")<< wxT("</h3>")
      << wxT("<p>") << XO("A multiband compressor offering precise control over frequencies.");

#endif

      << wxT("<h3>") << wxT("Ace Studio")<< wxT("</h3>")
      << wxT("<p>") << XO("Ace Studio - The World's No.1 AI Singing Voice Generator.");

   return FormatHtmlText(o.GetString());
}

const wxColour infoBlue(224, 228, 255);
#if defined (__WXOSX__) || defined(__WXMSW__)
const wxSize infoWindowSize(746, 176);
#else
const wxSize infoWindowSize(1050, 250);
#endif
class InfoWindow : public wxWindow
{
public:
    InfoWindow(wxWindow* parent)
        : wxWindow(parent, wxID_ANY, wxDefaultPosition, infoWindowSize)
    {
       ShuttleGui S( this, eIsCreating);

       S.StartHorizontalLay(wxEXPAND);
       {
          S.AddSpace(150);

          S.StartVerticalLay(wxEXPAND);
          {
#if defined(__WXMSW__)
             S.AddSpace(30);
#else
             S.AddSpace(35);
#endif
             
             const auto text = safenew LinkingHtmlWindow(S.GetParent());
             text->SetPage(MakeSignUpToCloudText());
             text->Layout();
             text->Fit();
             text->SetBackgroundColour(infoBlue);
             S
               .Prop(1)
               .Position(wxEXPAND | wxLEFT | wxBOTTOM)
               .AddWindow(text);

	     TranslatableString cloudLabel = XXO("Continue for Cloud Storage");
             S
                .Id(WhatsNewID_GoToAudioCom)
                .Position(wxALL | wxALIGN_CENTER)
#if defined (__WXOSX__) || defined(__WXMSW__)
                .AddGradientButton(cloudLabel, wxALL, true, true /* set padding */);
#else
                .AddButton(cloudLabel, wxALL, true);
#endif

#if defined(__WXMSW__)
             S.AddSpace(20);
#else
             S.AddSpace(25);
#endif
          }
          S.EndVerticalLay();
          
          S.AddSpace(150);
       }
       S.EndHorizontalLay();
       
       Layout();
       
#if defined (__WXOSX__)
       wxImage image = wxBitmap(Cloud).ConvertToImage();
       wxImage resizedImage = image.Scale(189, 104, wxIMAGE_QUALITY_HIGH);
       m_bitmap = wxBitmap(resizedImage);
#else
       m_bitmap = wxBitmap(Cloud_low_res);
#endif

       Bind(wxEVT_PAINT, &InfoWindow::OnPaint, this);
    }

private:
    void OnPaint(wxPaintEvent& event)
    {
        wxPaintDC dc(this);
       
        wxColour outline(212, 212, 212);

        dc.SetBrush(infoBlue);
        dc.SetPen(outline);
        dc.DrawRoundedRectangle(wxPoint(0, 0), infoWindowSize, 10);
       
        dc.DrawBitmap(m_bitmap, -5, 40, true);
    }
   
    wxBitmap m_bitmap;
};
}

BEGIN_EVENT_TABLE(WhatsNewDialog, wxDialogWrapper)
   EVT_BUTTON(WhatsNewID_WatchReleaseVideo, WhatsNewDialog::OnWatchReleaseVideo)
   EVT_BUTTON(WhatsNewID_GoToMuseHub, WhatsNewDialog::OnGoToMuseHub)
   EVT_BUTTON(WhatsNewID_GoToAudioCom, WhatsNewDialog::OnGoToAudioCom)
   EVT_BUTTON(wxID_OK, WhatsNewDialog::OnOK)
END_EVENT_TABLE()

WhatsNewDialog::WhatsNewDialog(wxWindow* parent, wxWindowID id)
   : wxDialogWrapper(parent, id, XO("Welcome to Audacity!"))
{

   SetSize(FromDIP(wxSize(WindowWidth, WindowHeight)));

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

   S.AddSpace(10);
   
   S.StartHorizontalLay(wxALIGN_CENTER, 0);
   {
      const auto cloudInfo = safenew InfoWindow(S.GetParent());
      S
         .Prop(1)
         .Position(wxALL | wxALIGN_CENTER)
         .AddWindow(cloudInfo);
   }
   S.EndHorizontalLay();
   S.StartHorizontalLay(wxEXPAND);
   {
      S.StartVerticalLay(wxEXPAND | wxLEFT);
      {
         const auto whatsnew = safenew LinkingHtmlWindow(S.GetParent());
         whatsnew->SetPage(MakeWhatsNewText());
         whatsnew->SetBackgroundColour(S.GetParent()->GetBackgroundColour());
         S
            .Prop(1)
#if defined(SHOW_MUSEHUB)
            .Position(wxEXPAND | wxLEFT | wxTOP)
#else
            .Position(wxEXPAND | wxALL)
#endif
            .AddWindow(whatsnew);

         TranslatableString releaseLabel = XXO("Watch the release video");
         S
            .Id(WhatsNewID_WatchReleaseVideo)
            .Position(wxALL | wxALIGN_CENTER)
#if defined (__WXOSX__) || defined(__WXMSW__)
            .AddGradientButton(releaseLabel, wxALL, true, true /* set padding */);
#else
            .AddButton(releaseLabel, wxALL, true);
#endif
      }
      S.EndVerticalLay();
#if defined(SHOW_MUSEHUB)
      S.StartVerticalLay(wxEXPAND);
      {
         const auto getplugins = safenew LinkingHtmlWindow(S.GetParent());
         getplugins->SetPage(MakeGetPluginsText());
         getplugins->SetBackgroundColour(S.GetParent()->GetBackgroundColour());
         S
            .Prop(1)
            .Position(wxEXPAND | wxTOP | wxRIGHT)
            .AddWindow(getplugins);
         TranslatableString museHubLabel = XXO("Available on MuseHub");
         S
            .Id(WhatsNewID_GoToMuseHub)
            .Position(wxALL | wxALIGN_CENTER)
#if defined (__WXOSX__) || defined(__WXMSW__)
            .AddGradientButton(museHubLabel, wxALL, true, true /* set padding */);
#else
            .AddButton(museHubLabel, wxALL, true);
#endif
      }
      S.EndVerticalLay();
#endif
   }
   S.EndHorizontalLay();
   S.AddSpace(15);
   const auto line = safenew wxWindow(S.GetParent(), wxID_ANY);
   line->SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNSHADOW));
   line->SetSize(-1, 1);

   S
      .Prop(0)
      .Position(wxEXPAND)
      .AddWindow(line);

   S.StartHorizontalLay(wxALIGN_CENTER, 0);
   {
      S.SetBorder(10);
      const auto tutorialsLink = safenew HyperLink(
         S.GetParent(),
         wxID_ANY,
         _("View tutorials"),
         "https://support.audacityteam.org/");
      S
         .Position(wxTOP | wxBOTTOM)
         .AddWindow(tutorialsLink);

      S.AddSpace(25);

      const auto forumLink = safenew HyperLink(
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
            .Position(wxALL | wxALIGN_CENTER)
            .AddCheckBox( XXO("Don't show this again at start up"), !showSplashScreen);
         
         S.AddSpace(1,1,1);

         TranslatableString okLabel = XXO("OK");
         S
            .Id(wxID_OK)
#if defined (__WXOSX__) || defined(__WXMSW__)
            .AddGradientButton(okLabel, wxALL, true);
#else
            .AddButton(okLabel, wxALL, true);
#endif
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

void WhatsNewDialog::OnWatchReleaseVideo(wxCommandEvent& evt)
{
   OpenInDefaultBrowser(WhatsNewURL);
}

void WhatsNewDialog::OnGoToMuseHub(wxCommandEvent& evt)
{
   OpenInDefaultBrowser(MuseHubURL);
}

void WhatsNewDialog::OnGoToAudioCom(wxCommandEvent& evt)
{
   OpenInDefaultBrowser(AudioComURL);
}

