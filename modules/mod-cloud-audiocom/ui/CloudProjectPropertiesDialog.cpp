/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectPropertiesDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudProjectPropertiesDialog.h"

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>

#include "AuthorizationHandler.h"
#include "CodeConversions.h"
#include "CloudModuleSettings.h"
#include "UserPanel.h"

namespace cloud::audiocom::sync
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

   mProjectName = new wxTextCtrl { this, wxID_ANY, projectName };

   if (!projectName.empty())
   {
      mProjectName->SetValue(projectName);
      mProjectName->SetInsertionPoint(projectName.length());
   }

   const wxString choices[] = { XO("Private").Translation(),
                                XO("Unlisted").Translation(),
                                XO("Public").Translation() };

   mProjectVisibility =
      new wxChoice { this,          wxID_ANY,          wxDefaultPosition,
                     wxDefaultSize, WXSIZEOF(choices), choices };

   mProjectVisibility->SetSelection(0);

   mSaveToCloud = new wxButton { this, wxID_ANY, XO("Save").Translation() };

   mSaveLocally =
      new wxButton { this, wxID_ANY, XO("Save to computer...").Translation() };
   mCancel = new wxButton { this, wxID_ANY, XO("Cancel").Translation() };

   LayoutControls();
   OnUpdateCloudSaveState();

   SetupEvents();
}

CloudProjectPropertiesDialog::~CloudProjectPropertiesDialog()
{
   GetAuthorizationHandler().PopSuppressDialogs();
}

SaveResult CloudProjectPropertiesDialog::Show(
   const ServiceConfig& serviceConfig, OAuthService& authService,
   UserService& userService, const wxString& projectName, wxWindow* parent, bool allowLocalSave)
{
   CloudProjectPropertiesDialog dialog { serviceConfig, authService,
                                         userService, projectName, parent };

   dialog.mSaveLocally->Show(allowLocalSave);

   const auto resultCode = dialog.ShowModal();

   return { audacity::ToUTF8(dialog.GetProjectName()),
            static_cast<CloudProjectVisibility>(
               dialog.mProjectVisibility->GetSelection()),
            resultCode == wxID_OK, resultCode == wxID_CANCEL };
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
   topSizer->AddSpacer(2 * spacerHeight);

   topSizer->Add(
      new wxStaticText { this, wxID_ANY,
                         XO("Project Visibility").Translation() },
      0, wxEXPAND | wxLEFT | wxRIGHT, 16);
   topSizer->AddSpacer(spacerHeight);

   topSizer->Add(mProjectVisibility, 0, wxLEFT | wxRIGHT, 16);
   topSizer->AddSpacer(4 * spacerHeight);

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

   mProjectName->Bind(wxEVT_TEXT, [this](auto&) { OnUpdateCloudSaveState(); });
}

wxString CloudProjectPropertiesDialog::GetProjectName() const
{
   wxString result { mProjectName->GetValue() };
   result.Trim(true).Trim(false);
   return result;
}

void CloudProjectPropertiesDialog::OnUpdateCloudSaveState()
{
   mSaveToCloud->Enable(mUserPanel->IsAuthorized() && !GetProjectName().empty());
}

} // namespace cloud::audiocom::sync
