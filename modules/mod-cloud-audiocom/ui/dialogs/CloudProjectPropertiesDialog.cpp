/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectPropertiesDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudProjectPropertiesDialog.h"

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/sizer.h>
#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "../UserPanel.h"

#include "AccessibilityUtils.h"
#include "AuthorizationHandler.h"
#include "CodeConversions.h"

namespace audacity::cloud::audiocom::sync
{
CloudProjectPropertiesDialog::CloudProjectPropertiesDialog(
   const ServiceConfig& serviceConfig, OAuthService& authService,
   UserService& userService, const wxString& projectName, wxWindow* parent)
    : wxDialogWrapper { parent, wxID_ANY, XO("Save to audio.com") }
{
   GetAuthorizationHandler().PushSuppressDialogs();

   mUserPanel =
      new UserPanel(serviceConfig, authService, userService, true, this);

   mUserStateChangedSubscription =
      mUserPanel->Subscribe([this](auto) { OnUpdateCloudSaveState(); });

   mProjectName = new wxTextCtrl { this,          wxID_ANY,
                                   projectName,   wxDefaultPosition,
                                   wxDefaultSize, wxTE_PROCESS_ENTER };

   mAnonStateText = safenew wxStaticText {
      this, wxID_ANY,
      XO("Cloud saving requires a free audio.com account linked to Audacity. Press \"Link account\" above to proceed.")
         .Translation()
   };

   mAnonStateText->Wrap(450 - 32);

   if (!projectName.empty())
   {
      mProjectName->SetValue(projectName);
      // mProjectName->SetInsertionPoint(-1);
   }

   const wxString choices[] = { XO("Private").Translation(),
                                XO("Unlisted").Translation(),
                                XO("Public").Translation() };

   mSaveToCloud = new wxButton { this, wxID_ANY, XO("Save").Translation() };

   mSaveLocally =
      new wxButton { this, wxID_ANY, XO("Save to computer...").Translation() };
   mCancel = new wxButton { this, wxID_ANY, XO("Cancel").Translation() };

   mProjectName->SetFocus();

   LayoutControls();
   OnUpdateCloudSaveState();

   SetupEvents();

   SetupAccessibility(this);
}

CloudProjectPropertiesDialog::~CloudProjectPropertiesDialog()
{
   GetAuthorizationHandler().PopSuppressDialogs();
}

std::pair<CloudProjectPropertiesDialog::Action, std::string>
CloudProjectPropertiesDialog::Show(
   const ServiceConfig& serviceConfig, OAuthService& authService,
   UserService& userService, const wxString& projectName, wxWindow* parent,
   bool allowLocalSave)
{
   CloudProjectPropertiesDialog dialog { serviceConfig, authService,
                                         userService, projectName, parent };

   dialog.mSaveLocally->Show(allowLocalSave);

   const auto resultCode = dialog.ShowModal();

   const auto action =
      resultCode == wxID_OK ?
         Action::SaveToCloud :
         (resultCode == wxID_CANCEL ? Action::Cancel : Action::SaveLocally);

   return { action, dialog.GetProjectName() };
}

bool CloudProjectPropertiesDialog::OnSubmit()
{
   const bool canSubmit =
      mUserPanel->IsAuthorized() && !GetProjectName().empty();

   if (!canSubmit)
      return false;

   EndModal(wxID_OK);
   return true;
}

void CloudProjectPropertiesDialog::LayoutControls()
{
   auto* topSizer = new wxBoxSizer { wxVERTICAL };

   constexpr auto spacerHeight = 8;

   topSizer->AddSpacer(spacerHeight);

   topSizer->Add(mUserPanel, 0, wxEXPAND | wxLEFT | wxRIGHT, 16);
   topSizer->AddSpacer(spacerHeight);

   topSizer->Add(new wxStaticLine { this }, 0, wxEXPAND | wxLEFT | wxRIGHT, 16);
   topSizer->AddSpacer(spacerHeight);

   topSizer->Add(
      new wxStaticText { this, wxID_ANY, XO("Project Name").Translation() }, 0,
      wxEXPAND | wxLEFT | wxRIGHT, 16);
   topSizer->AddSpacer(spacerHeight);

   topSizer->Add(mProjectName, 0, wxEXPAND | wxLEFT | wxRIGHT, 16);

   topSizer->AddSpacer(spacerHeight);

   topSizer->Add(mAnonStateText, 0, wxEXPAND | wxLEFT | wxRIGHT, 16);

   topSizer->AddSpacer(2 * spacerHeight);

   auto buttonSizer = new wxBoxSizer { wxHORIZONTAL };

   buttonSizer->Add(mSaveLocally, 0, wxLEFT, 16);
   buttonSizer->AddStretchSpacer();
   buttonSizer->Add(mCancel, 0, wxRIGHT, 4);
   buttonSizer->Add(mSaveToCloud, 0, wxRIGHT, 16);

   topSizer->Add(buttonSizer, 0, wxEXPAND | wxALL, 0);
   topSizer->AddSpacer(spacerHeight);

   SetMinSize({ 450, -1 });
   SetSizer(topSizer);
   Fit();
   Centre(wxBOTH);

   mAnonStateText->Show(!mUserPanel->IsAuthorized());
}

void CloudProjectPropertiesDialog::SetupEvents()
{
   mSaveToCloud->Bind(wxEVT_BUTTON, [this](auto&) { EndModal(wxID_OK); });
   mSaveLocally->Bind(wxEVT_BUTTON, [this](auto&) { EndModal(wxID_SAVE); });
   mCancel->Bind(wxEVT_BUTTON, [this](auto&) { EndModal(wxID_CANCEL); });

   Bind(
      wxEVT_CHAR_HOOK,
      [this](auto& evt)
      {
         if (!IsEscapeKey(evt))
         {
            evt.Skip();
            return;
         }

         EndModal(wxID_CANCEL);
      });

   Bind(
      wxEVT_KEY_UP,
      [this](auto& evt)
      {
         const auto keyCode = evt.GetKeyCode();
         if (keyCode != WXK_RETURN && keyCode != WXK_NUMPAD_ENTER)
         {
            evt.Skip();
            return;
         }

         if (!OnSubmit())
            evt.Skip();
      });

   mProjectName->Bind(wxEVT_TEXT, [this](auto&) { OnUpdateCloudSaveState(); });
   mProjectName->Bind(wxEVT_TEXT_ENTER, [this](auto&) { OnSubmit(); });
}

std::string CloudProjectPropertiesDialog::GetProjectName() const
{
   wxString result { mProjectName->GetValue() };
   result.Trim(true).Trim(false);
   return audacity::ToUTF8(result);
}

void CloudProjectPropertiesDialog::OnUpdateCloudSaveState()
{
   mSaveToCloud->Enable(
      mUserPanel->IsAuthorized() && !GetProjectName().empty());

   mAnonStateText->Show(!mUserPanel->IsAuthorized());
   Fit();
   Layout();
}

} // namespace audacity::cloud::audiocom::sync
