/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UserPanel.cpp

  Dmitry Vedenko

**********************************************************************/

#include "UserPanel.h"

#include <cassert>

#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#ifdef HAS_CUSTOM_URL_HANDLING
#   include "URLSchemesRegistry.h"
#endif

#include "dialogs/LinkWithTokenDialog.h"

#include "AuthorizationHandler.h"
#include "OAuthService.h"
#include "ServiceConfig.h"
#include "UserService.h"
#include "ui/dialogs/LoginDialog.h"

#include "CodeConversions.h"
#include "ExportUtils.h"

#include "AllThemeResources.h"
#include "Theme.h"

#include "UserImage.h"

#include "MemoryX.h"

#include "HelpSystem.h"

namespace audacity::cloud::audiocom
{

namespace
{
const wxSize avatarSize  = { 32, 32 };
const auto anonymousText = XO("You are not signed in");
} // namespace

UserPanel::UserPanel(
   const ServiceConfig& serviceConfig, OAuthService& authService,
   UserService& userService, bool linkButtonVisible, AudiocomTrace trace,
   wxWindow* parent, const wxPoint& pos, const wxSize& size)
    : wxPanelWrapper { parent, wxID_ANY, pos, size }
    , mServiceConfig { serviceConfig }
    , mAuthService { authService }
    , mUserService { userService }
    , mAudiocomTrace { trace }
    , mUserDataChangedSubscription { userService.Subscribe(
         [this](const auto&) { UpdateUserData(); }) }
    , mLinkButtonVisible { linkButtonVisible }
{
   GetAuthorizationHandler().PushSuppressDialogs();
   mUserImage = safenew UserImage(this, avatarSize);
   mUserImage->SetLabel(anonymousText);      // for screen readers
   mUserName = safenew wxStaticText(
      this,
      wxID_ANY,
      anonymousText.Translation() );

   mLinkButton =
      safenew wxButton(this, wxID_ANY, XXO("Sign in").Translation());
   mLinkButton->Bind(wxEVT_BUTTON, [this](auto) { OnLinkButtonPressed(); });

   mLinkButton->Show(mLinkButtonVisible);

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

bool UserPanel::IsAuthorized() const
{
   return mIsAuthorized;
}

void UserPanel::OnStateChanged(bool isAuthorized)
{
   if (mIsAuthorized == isAuthorized)
      return;

   mIsAuthorized = isAuthorized;
   Publish(UserPanelStateChangedMessage { isAuthorized });
}

void UserPanel::UpdateUserData()
{
   Freeze();

   auto layoutUpdater = finally(
      [this]()
      {
         mLinkButton->Fit();

         Layout();
         Thaw();

         auto parent = GetParent();

         if (parent != nullptr)
            parent->Refresh();
         else
            Refresh();
      });

   auto& oauthService = GetOAuthService();

   if (!oauthService.HasRefreshToken())
   {
      SetAnonymousState();
      return;
   }

   if (!oauthService.HasAccessToken())
      oauthService.ValidateAuth({}, mAudiocomTrace, true);

   auto& userService = GetUserService();

   if (userService.GetUserSlug().empty())
   {
      SetAnonymousState();
      return;
   }

   wxString displayName = userService.GetDisplayName();

   if (displayName.empty())
   {
      displayName = userService.GetUserSlug();
   }

   mUserName->SetLabel(displayName);
   mUserImage->wxPanel::SetLabel(displayName);  // for screen readers

   const auto avatarPath = userService.GetAvatarPath();

   if (!avatarPath.empty())
      mUserImage->SetBitmap(avatarPath);
   else
      mUserImage->SetBitmap(theTheme.Bitmap(bmpAnonymousUser));

   mLinkButton->SetLabel(XXO("&Sign Out").Translation());
   mLinkButton->Show();

   OnStateChanged(true);
}

void UserPanel::OnLinkButtonPressed()
{
   auto& oauthService = GetOAuthService();

   if (oauthService.HasAccessToken())
   {
      oauthService.UnlinkAccount(mAudiocomTrace);
   }
   else
   {
      LoginDialog::SignIn(this, LoginDialog::Mode::Create);
   }
}

void UserPanel::SetAnonymousState()
{
   mUserName->SetLabel(anonymousText.Translation());
   mUserImage->SetBitmap(theTheme.Bitmap(bmpAnonymousUser));
   mUserImage->SetLabel(anonymousText);   // for screen readers
   mLinkButton->SetLabel(XXO("Sign in").Translation());
   mLinkButton->Show(mLinkButtonVisible);

   OnStateChanged(false);
}

} // namespace audacity::cloud::audiocom
