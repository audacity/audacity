/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UserPanel.cpp

  Dmitry Vedenko

**********************************************************************/

#include "UserPanel.h"

#include <cassert>

#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/sizer.h>

#ifdef HAS_CUSTOM_URL_HANDLING
#   include "URLSchemesRegistry.h"
#endif

#include "LinkAccountDialog.h"

#include "ServiceConfig.h"
#include "OAuthService.h"
#include "UserService.h"

#include "CodeConversions.h"

#include "ShuttleGui.h"
#include "Theme.h"
#include "AllThemeResources.h"

#include "UserImage.h"

#include "MemoryX.h"

#include "HelpSystem.h"

namespace cloud::audiocom
{

namespace
{
const wxSize avatarSize = { 32, 32 };
} // namespace

UserPanel::UserPanel(
   const ServiceConfig& serviceConfig, OAuthService& authService,
   UserService& userService, bool hasLinkButton, wxWindow* parent,
   const wxPoint& pos, const wxSize& size)
    : wxPanelWrapper { parent, wxID_ANY, pos, size }
    , mServiceConfig { serviceConfig }
    , mAuthService { authService }
    , mUserService { userService }
    , mUserDataChangedSubscription {
       userService.Subscribe([this](const auto&) { UpdateUserData(); })
    }
{

   mUserImage = safenew UserImage(this, avatarSize);
   mUserName = safenew wxStaticText(this, wxID_ANY, XO("Anonymous").Translation());
   mLinkButton = safenew wxButton(this, wxID_ANY, XXO("&Link Account").Translation());
   mLinkButton->Bind(wxEVT_BUTTON, [this](auto) { OnLinkButtonPressed(); });
   mLinkButton->Show(hasLinkButton);

   auto sizer = safenew wxBoxSizer { wxHORIZONTAL };

   sizer->Add(mUserImage, 0, wxALIGN_CENTER_VERTICAL);
   sizer->AddSpacer(8);
   sizer->Add(mUserName, 0, wxALIGN_CENTER_VERTICAL);
   sizer->AddStretchSpacer();
   sizer->Add(mLinkButton, 0, wxALIGN_CENTER_VERTICAL);

   SetSizerAndFit(sizer);
   UpdateUserData();
}

UserPanel::~UserPanel() = default;

void UserPanel::UpdateUserData()
{
   Freeze();

   auto layoutUpdater = finally(
      [this]()
      {
         mLinkButton->Fit();

         Layout();
         Thaw();

         Refresh();
      });

   auto& oauthService = GetOAuthService();

   if (!oauthService.HasRefreshToken())
   {
      SetAnonymousState();
      return;
   }

   if (!oauthService.HasAccessToken())
      oauthService.ValidateAuth({});

   auto& userService = GetUserService();

   if (userService.GetUserSlug().empty())
   {
      SetAnonymousState();
      return;
   }

   const auto displayName = userService.GetDisplayName();

   if (!displayName.empty())
      mUserName->SetLabel(displayName);

   const auto avatarPath = userService.GetAvatarPath();

   if (!avatarPath.empty())
      mUserImage->SetBitmap(avatarPath);
   else
      mUserImage->SetBitmap(theTheme.Bitmap(bmpAnonymousUser));

   mLinkButton->SetLabel(XXO("&Unlink Account").Translation());

   Publish(UserPanelStateChangedMessage { true });
}

void UserPanel::OnLinkButtonPressed()
{
   auto& oauthService = GetOAuthService();

   if (oauthService.HasAccessToken())
      oauthService.UnlinkAccount();
   else
   {
      OpenInDefaultBrowser(
         { audacity::ToWXString(mServiceConfig.GetOAuthLoginPage()) });

#ifdef HAS_CUSTOM_URL_HANDLING
      if (!URLSchemesRegistry::Get().IsURLHandlingSupported())
#endif
      {
         LinkAccountDialog dlg(this);
         dlg.ShowModal();
      }
   }
}

void UserPanel::SetAnonymousState()
{
   mUserName->SetLabel(XO("Anonymous").Translation());
   mUserImage->SetBitmap(theTheme.Bitmap(bmpAnonymousUser));
   mLinkButton->SetLabel(XXO("&Link Account").Translation());

   Publish(UserPanelStateChangedMessage { false });
}

} // namespace cloud::audiocom
