/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  GetEffectsDialog.h

  Dmitry Makarenko

**********************************************************************/
#include "GetEffectsDialog.h"

#include <wx/font.h>
#include <wx/image.h>
#include <wx/mstream.h>
#include <wx/sizer.h>
#include <wx/string.h>
#include <wx/weakref.h>
#include <wx/treectrl.h>

#include "MuseHubService.h"

#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"

#include "RoundedStaticBitmap.h"
#include "BasicUI.h"
#include "Theme.h"
#include "AllThemeResources.h"
#include "GradientButton.h"
#include "WindowAccessible.h"

namespace audacity::musehub
{

static constexpr int iconRadius = 12;

static constexpr int effectIconWidth = 118;
static constexpr int effectIconHeight = 118;

static constexpr int effectPanelWidth = 318;
static constexpr int effectPanelHeight = effectIconHeight;

static constexpr int getEffectButtonWidth = 187;
static constexpr int getEffectButtonHeight = 30;

static const wxColor musehubButtonNormal = wxColor(0x63, 0x63, 0xd5);
static const wxColor musehubButtonPressed = wxColor(0x53, 0x53, 0xc5);

static const auto loadingPluginsTabText = XO("Loading...");
static const auto loadingPluginsText = XO("Please wait...");

static const auto connectionErrorTitleText = XO("Connection error");
static const auto connectionErrorDescriptionText = XO("Audacity is unable to connect to MuseHub.com. Please check your connection and try again.");
static const auto tryAgainText = XO("Try again");

static const auto getItOnMusehubButtonText = XO("Get it on MuseHub");

static const auto becomeAPartnerButtonText = XO("Become a partner");
static const auto becomeAPartnerTitle = XO("Become a MuseHub partner");
static const auto becomeAPartnerDescription =
   XO("You can reach millions of music creators by publishing your apps, "
      "effects and sounds on MuseHub. "
      "If you're a developer and interested in publishing your products "
      "on MuseHub, you can apply today.");

GetEffectsDialog::GetEffectsDialog(wxWindow *parent) :
   wxDialogWrapper(parent, wxID_ANY, XO("Get Effects"), wxDefaultPosition, {500, 500}, wxDEFAULT_DIALOG_STYLE)
{
   m_treebook = new wxTreebook(this, wxID_ANY);
   m_treebook->GetTreeCtrl()->SetIndent(0);
   wxBoxSizer* vSizer = new wxBoxSizer(wxVERTICAL);
   vSizer->Add(m_treebook, 1, wxEXPAND);

   // Needed so that when the tree is the focus, pressing the Enter key
   // does close the dialog, in accordance with the OK button being
   // the default button.
   m_treebook->Bind(wxEVT_TREE_KEY_DOWN, [this](wxTreeEvent& event) {
      if (event.GetKeyCode() == WXK_RETURN)
         EndModal(wxID_OK);
      else
         event.Skip();
      });

   wxPanel* bottomPanel = new wxPanel(this, wxID_ANY);
   wxBoxSizer* hSizer = new wxBoxSizer(wxHORIZONTAL);

   hSizer->AddStretchSpacer(1);

   auto* okButton = new wxButton(bottomPanel, wxID_OK, XO("OK").Translation());
   hSizer->Add(okButton, 0, wxALL, 10);
   okButton->SetDefault();

   bottomPanel->SetSizer(hSizer);
   vSizer->Add(bottomPanel, 0, wxEXPAND | wxBOTTOM | wxLEFT | wxRIGHT, 0);

   SetSizer(vSizer);

   SetSize(920, 650);
   CenterOnParent();

   ReloadEffectList();
}

void GetEffectsDialog::ReloadEffectList()
{
   m_treebook->DeleteAllPages();
   AddLoadingPage();
   GetEffects([this, self = wxWeakRef(this)] (std::vector<EffectsGroup> groups) {
      if (!self)
         return;
      BasicUI::CallAfter([this, groups]() {
         // Freeze the window to avoid flickering
         Freeze();

         m_treebook->DeleteAllPages();

         for (const auto group : groups) {
            AddEffectsPage(group.title, group.effects);
         }

         if (groups.empty()) {
            AddLoadingErrorPage();
         }

         AddBecomeAPartnerPage();

         Thaw();
         Layout();
      });
   });
}

void GetEffectsDialog::FetchImage(RoundedStaticBitmap* bitmap, const std::string& url)
{
   using namespace audacity::network_manager;
   auto response = NetworkManager::GetInstance().doGet(Request(url));

   response->setRequestFinishedCallback([response, bitmap, self=wxWeakRef(this)] (auto){
      if (!self) {
         return;
      }

      const auto httpCode = response->getHTTPCode();
      if (httpCode != HttpCode::OK) {
         return;
      }

      BasicUI::CallAfter([response, bitmap, self]() {
         if(!self) {
            return;
         }

         auto data = response->readAll<std::vector<uint8_t>>();
         wxMemoryInputStream memStream(data.data(), data.size());
         wxImage image;

         if (!image.LoadFile(memStream)) {
            return;
         }

         image.Rescale(effectIconWidth, effectPanelHeight, wxIMAGE_QUALITY_HIGH);
         bitmap->SetImage(image);
      });
   });
}

void GetEffectsDialog::AddLoadingPage() {
   wxPanel* page = safenew wxPanel();
   page->Hide();
   page->Create(m_treebook, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE);

   wxBoxSizer* sizer = safenew wxBoxSizer(wxVERTICAL);
   page->SetSizer(sizer);
   sizer->AddSpacer(20);

   wxStaticText* title = safenew wxStaticText(page, wxID_ANY, loadingPluginsText.Translation());
   wxFont font = title->GetFont().MakeLarger().MakeBold();
   title->SetFont(font);

   sizer->Add(title, 0, wxALIGN_CENTER_HORIZONTAL);

   m_treebook->AddPage(page, loadingPluginsTabText.Translation());
}

void GetEffectsDialog::AddBecomeAPartnerPage() {
   wxPanel* page = safenew wxPanel();
   page->Hide();
   page->Create(m_treebook, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE);

   wxBoxSizer* sizer = safenew wxBoxSizer(wxVERTICAL);

   page->SetSizer(sizer);

   wxStaticText* title = safenew wxStaticText(page, wxID_ANY, becomeAPartnerTitle.Translation());
   wxFont font = title->GetFont().MakeLarger().MakeBold();
   title->SetFont(font);

   wxStaticText* description = safenew wxStaticText(page, wxID_ANY, becomeAPartnerDescription.Translation());
   description->Wrap(550);

   GradientButton* button = safenew GradientButton(page, wxID_ANY, becomeAPartnerButtonText.Translation(), wxDefaultPosition, wxSize(getEffectButtonWidth, getEffectButtonHeight));
   button->SetNormalColor(musehubButtonNormal);
   button->SetPressedColor(musehubButtonPressed);

   button->Bind(wxEVT_BUTTON, [](auto) {
      BasicUI::OpenInDefaultBrowser(GetBecomeAPartnerUrl());
   });
#if wxUSE_ACCESSIBILITY
   safenew WindowAccessible(button);
   button->SetName(becomeAPartnerTitle.Translation() + wxT(", ")
      + becomeAPartnerDescription.Translation() + wxT(", ")
      + becomeAPartnerButtonText.Translation());
#endif

   sizer->AddSpacer(35);
   sizer->Add(title, 0, wxLEFT, 25);
   sizer->AddSpacer(20);
   sizer->Add(description, 0, wxLEFT, 25);
   sizer->AddSpacer(20);
   sizer->Add(button, 0, wxLEFT, 25);

   m_treebook->AddPage(page, becomeAPartnerButtonText.Translation());
}

void GetEffectsDialog::AddLoadingErrorPage() {
   wxPanel* page = safenew wxPanel();
   page->Hide();
   page->Create(m_treebook, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE);

   wxBoxSizer* sizer = safenew wxBoxSizer(wxVERTICAL);

   page->SetSizer(sizer);
   sizer->AddSpacer(20);

   wxStaticText* title = safenew wxStaticText(page, wxID_ANY, connectionErrorTitleText.Translation());
   wxFont font = title->GetFont().MakeLarger().MakeBold();
   title->SetFont(font);

   wxStaticText* description = safenew wxStaticText(page, wxID_ANY, connectionErrorDescriptionText.Translation());
   description->Wrap(550);

   GradientButton* button = safenew GradientButton(page, wxID_ANY, tryAgainText.Translation(), wxDefaultPosition, wxSize(getEffectButtonWidth, getEffectButtonHeight));
   button->SetNormalColor(musehubButtonNormal);
   button->SetPressedColor(musehubButtonPressed);
   button->Bind(wxEVT_BUTTON, [this](auto) {
      ReloadEffectList();
   });

   sizer->Add(title, 0, wxALL, 10);
   sizer->Add(description, 0, wxALL, 10);
   sizer->Add(button, 0, wxALL, 10);

   m_treebook->AddPage(page, loadingPluginsTabText.Translation());
}

void GetEffectsDialog::AddEffectsPage(const std::string& group, const std::vector<EffectInfo>& effects) {
   wxScrolledWindow* page = safenew wxScrolledWindow();
   page->Hide();
   page->Create(m_treebook, wxID_ANY);

   page->SetScrollRate(0, 20);
   page->SetBackgroundColour(theTheme.Colour(clrMedium));

   wxGridSizer* grid = safenew wxGridSizer(2, 16, 40);

   for (const auto& elem : effects) {
      wxPanel* itemPanel = safenew wxPanel(page);
      itemPanel->SetMinSize({effectPanelWidth, effectPanelHeight});
      itemPanel->SetMaxSize({effectPanelWidth, effectPanelHeight});
      itemPanel->SetBackgroundColour(theTheme.Colour(clrMedium));

      wxImage img(effectIconWidth, effectIconHeight);
      auto fillCol = theTheme.Colour(clrDark);
      img.SetRGB(wxRect(0, 0, effectIconWidth, effectIconHeight),
               fillCol.Red(), fillCol.Green(), fillCol.Blue());
      auto bitmap = safenew RoundedStaticBitmap(
            itemPanel, wxID_ANY, wxBitmap(img), iconRadius,
            wxDefaultPosition, wxSize(effectIconWidth, effectIconHeight)
      );

      FetchImage(bitmap, elem.iconUrl);

      wxPanel* textPanel = safenew wxPanel(itemPanel);
      textPanel->SetBackgroundColour(theTheme.Colour(clrMedium));

      wxStaticText* title = safenew wxStaticText(textPanel, wxID_ANY, elem.title);
      wxFont titleFont = title->GetFont().MakeBold().MakeLarger();
      title->SetFont(titleFont);

      wxStaticText* description = safenew wxStaticText(textPanel, wxID_ANY, elem.subtitle);
      description->Wrap(getEffectButtonWidth - 20);

      GradientButton* button = safenew GradientButton(
            textPanel, wxID_ANY, getItOnMusehubButtonText.Translation(),
            wxDefaultPosition, wxSize(getEffectButtonWidth, getEffectButtonHeight)
      );

      button->Bind(wxEVT_BUTTON, [code = elem.code](auto) {
         BasicUI::OpenInDefaultBrowser(GetEffectUrl(code));
      });
      button->SetNormalColor(musehubButtonNormal);
      button->SetPressedColor(musehubButtonPressed);
#if wxUSE_ACCESSIBILITY
      safenew WindowAccessible(button);
      button->SetName(elem.title + wxT(", ") + elem.subtitle + wxT(", ")
         + getItOnMusehubButtonText.Translation());
#endif

      wxBoxSizer* vSizer = safenew wxBoxSizer(wxVERTICAL);

      vSizer->Add(title);
      vSizer->Add(description, 0, wxTOP, 5);
      vSizer->AddStretchSpacer();
      vSizer->Add(button);

      textPanel->SetSizer(vSizer);

      wxBoxSizer* hSizer = safenew wxBoxSizer(wxHORIZONTAL);

      hSizer->Add(bitmap, 0, wxALIGN_TOP | wxRIGHT);
      hSizer->AddSpacer(16);
      hSizer->Add(textPanel, 1, wxEXPAND);

      itemPanel->SetSizer(hSizer);
      grid->Add(itemPanel, 1, wxALL);
   }

   constexpr int pagePadding = 16;
   wxBoxSizer* vSizer = safenew wxBoxSizer(wxVERTICAL);
   vSizer->Add(grid, 0, wxALL, pagePadding);

   page->SetSizer(vSizer);
   vSizer->Fit(page);
   page->SetAutoLayout(true);
   m_treebook->AddPage(page, group.data());
}

}
