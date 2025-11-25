/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WhatsNewDialog.cpp

  Vitaly Sverchinsky

**********************************************************************/

#include "WhatsNewDialog.h"

#include <wx/fs_mem.h>
#include <wx/image.h>
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
#include "ModuleManager.h"
#include "PluginProvider.h"
#include "menus/GetEffectsHelper.h"
#include "ProjectWindows.h"
#include "RoundedStaticBitmap.h"
#include "ShuttleGui.h"
#include "Theme.h"
#include "AllThemeResources.h"
#include "GradientButton.h"
#include "ImageCarousel.h"
#include "HyperLink.h"

#include "../images/Cloud.xpm"
#include "../images/Cloud_low_res.xpm"

#include "../images/Audacity40Video.h"
#include "../images/OpenVinoMH.h"
#include "../images/AudacityMerchStore.h"
#include "../images/AudioDotComPromo.h"
#include "../images/AudacityPromo.h"
#include "../images/AudacityFeatureSurvey.h"
#include "../images/MuseHubPromo.h"
#include "menus/CloudLoginHelper.h"

namespace
{

#if defined (__WXOSX__) || defined(__WXMSW__)
#define SHOW_MUSEHUB true
#else
#define SHOW_MUSEHUB false
#endif

#if defined(HAS_AUDIOCOM_UPLOAD)
#define SHOW_CLOUD_PROMO true
#else
#define SHOW_CLOUD_PROMO false
#endif

const char* WhatsNewURL = "https://youtu.be/QYM3TWf_G38?utm_source=au-app-au4-video&utm_medium=au-app-au4-video&utm_campaign=au-app-au4-video";
const char* ChangeLogURL = "https://support.audacityteam.org/additional-resources/changelog";
const char* OpenVinoURL = "https://www.musehub.com/en-gb/plugin/openvino-ai-tools?utm_source=au-app-3-7-6-mh-welcome-open-vino&utm_medium=au-app-3-7-6-mh-welcome-open-vino&utm_campaign=au-app-3-7-6-mh-welcome-open-vino";
const char* PromoURL = "https://audacityteam.org/audacitypromo";
const char* AudioComURL = "https://audio.com/audacity/auth/sign-in?mtm_campaign=audacitydesktop&mtm_content=app_launch_popup";
const char* AudacitySurveyURL = "http://audacityteam.org/survey?utm_source=au-app-survey&utm_medium=survey&utm_campaign=au-app-welcome-au-app-survey-survey&utm_id=au-app-welcome";
const char* AudacityMerchStoreURL = "https://audacity-shop.fourthwall.com/en-gbp/?utm_source=au-app-merch-store&utm_medium=merch-25y&utm_campaign=au-app-welcome-au-app-merch-store-merch-25y&utm_id=au-app-welcome";

constexpr auto WindowWidth = 812;

#if defined(__WXOSX__)
// wxHTML renders text with smaller line spacing on macOS
   constexpr auto WindowHeight = 612;
#elif defined(__WXMSW__)
   constexpr auto WindowHeight = 656;
#else
   constexpr auto WindowHeight = 640;
#endif

}
AttachedWindows::RegisteredFactory sWhatsNewWindow{
   []( AudacityProject &project ) -> wxWeakRef< wxWindow > {
      auto &window = GetProjectFrame(project);
      return safenew WhatsNewDialog(&window, wxID_ANY);
   }
};


class NotFocusableWindow : public wxWindow
{
public:
   NotFocusableWindow(wxWindow* parent, wxWindowID id)
      : wxWindow(parent, id)
   {}

   bool AcceptsFocus() const override { return false; }
};

BEGIN_EVENT_TABLE(WhatsNewDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, WhatsNewDialog::OnOK)
END_EVENT_TABLE()

WhatsNewDialog::WhatsNewDialog(wxWindow* parent, wxWindowID id)
   : wxDialogWrapper(parent, id, XO("Welcome to Audacity!"))
{

   SetSize(FromDIP(wxSize(WindowWidth, WindowHeight)));

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

#if defined (__WXOSX__) || defined(__WXMSW__)
   const int width = 572;
   const int height = 322;
#else
   const int width = FromDIP(572);
   const int height = FromDIP(322);
#endif

   std::vector<CarouselSnapshot> snapshots;
   snapshots.reserve(5);

   snapshots.push_back(CarouselSnapshot(
      XXO("Video: how we're redesigning Audacity for the future"),
      RoundedImage(Rescale(LoadEmbeddedPNG(Audacity40Video_png, Audacity40Video_png_len), width, height), 12),
      WhatsNewURL,
      XXO("Watch video"),
      XXO("")
   ));

   if (ModuleManager::Get().CheckModuleLoaded("mod-cloud-audiocom")) {

      auto displayLoginDialog = []() {
         CloudLoginHelper::Get().ShowLoginDialog();
      };

      snapshots.push_back(CarouselSnapshot(
         XXO("Complete your Audacity cloud setup with audio.com"),
         RoundedImage(Rescale(LoadEmbeddedPNG(AudioDotComPromo_png, AudioDotComPromo_png_len), width, height), 12),
         displayLoginDialog,
         XXO("Continue"),
         XXO("")
      ));
   }

#if defined (__WXOSX__) || defined(__WXMSW__)
   snapshots.push_back(CarouselSnapshot(
      XXO("Get our free OpenVino AI tools"),
      RoundedImage(Rescale(LoadEmbeddedPNG(OpenVinoMH_png, OpenVinoMH_png_len), width, height), 12),
      OpenVinoURL,
      XXO("Get it on MuseHub"),
      XXO("")
   ));
#endif

   if (ModuleManager::Get().CheckModuleLoaded("mod-musehub-ui")) {

      auto displayMuseHub = []() {
         GetEffectsHelper::Get().GetEffects();
      };

      snapshots.push_back(CarouselSnapshot(
         XXO("Explore free plugins for scuplting your audio"),
         RoundedImage(Rescale(LoadEmbeddedPNG(MuseHubPromo_png, MuseHubPromo_png_len), width, height), 12),
         displayMuseHub,
         XXO("View free plugins"),
         XXO("")
      ));
   }

   snapshots.push_back(CarouselSnapshot(
      XXO("25th Anniversary Merchandise!"),
      RoundedImage(Rescale(LoadEmbeddedPNG(AudacityMerchStore_png, AudacityMerchStore_png_len), width, height), 12),
      AudacityMerchStoreURL,
      XXO("Visit now"),
      XXO("Visit our new Audacity merch store")
   ));


   S.StartVerticalLay(wxEXPAND);
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         const auto carousel = safenew ImageCarousel(S.GetParent(), snapshots);
         S
            .Prop(1)
            .Position(wxEXPAND)
            .AddWindow(carousel);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   const auto line = safenew NotFocusableWindow(S.GetParent(), wxID_ANY);
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

   const auto bottomLine = safenew NotFocusableWindow(S.GetParent(), wxID_ANY);
   bottomLine->SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNSHADOW));
   bottomLine->SetSize(-1, 1);
   S
      .Prop(0)
      .Position(wxEXPAND)
      .AddWindow(bottomLine);

   S.Position(wxEXPAND).StartPanel(2);
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetBorder(4);
         mDontShowAgain = S
            .Position(wxALL | wxALIGN_CENTER)
            .AddCheckBox( XXO("&Don't show this again at start up"), !showSplashScreen);

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

wxImage WhatsNewDialog::Rescale(const wxImage& image, int width, int height)
{
   wxImage img = image;
   img.Rescale(width, height, wxIMAGE_QUALITY_HIGH);

   return img;
}

wxImage WhatsNewDialog::LoadEmbeddedPNG(const unsigned char* data, size_t len)
{
    wxMemoryInputStream stream(data, len);
    wxImage image;
    if (!image.LoadFile(stream, wxBITMAP_TYPE_PNG))
    {
        wxLogError("Failed to load embedded PNG image.");
        return wxImage();
    }
    return image;
}
