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

#include "OAuthService.h"
#include "ServiceConfig.h"
#include "UserService.h"

#include "CodeConversions.h"
#include "ExportUtils.h"

#include "AllThemeResources.h"
#include "Theme.h"

#include "UserImage.h"

#include "MemoryX.h"

#include "HelpSystem.h"

namespace audacity::cloud::audiocom {
namespace {
const wxSize avatarSize  = { 32, 32 };
const auto notLinkedText = XO("Account not linked");
const auto anonymousText = XO("Anonymous");
} // namespace

UserPanel::UserPanel(
    const ServiceConfig& serviceConfig, OAuthService& authService, UserService& userService, LinkMode linkMode, AudiocomTrace trace,
    wxWindow* parent, const wxPoint& pos, const wxSize& size)
    : wxPanelWrapper{parent, wxID_ANY, pos, size}
    , mServiceConfig{serviceConfig}
    , mAuthService{authService}
    , mUserService{userService}
    , mAudiocomTrace{trace}
    , mUserDataChangedSubscription{userService.Subscribe(
                                       [this](const auto&) { UpdateUserData(); })},
    mLinkMode { linkMode }
{
    mUserImage = safenew UserImage(this, avatarSize);
    mUserImage->SetLabel(mLinkMode == LinkMode::Link
                         ? notLinkedText : anonymousText);     // for screen readers
    mUserName = safenew wxStaticText(
        this,
        wxID_ANY,
        (mLinkMode == LinkMode::Link
         ? notLinkedText : anonymousText).Translation());

    mLinkButton
        =safenew wxButton(this, wxID_ANY, XXO("&Link Account").Translation());
    mLinkButton->Bind(wxEVT_BUTTON, [this](auto) { OnLinkButtonPressed(); });

    mLinkButton->Show(mLinkMode == LinkMode::Link);

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
    if (mIsAuthorized == isAuthorized) {
        return;
    }

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

        if (parent != nullptr) {
            parent->Refresh();
        } else {
            Refresh();
        }
    });

    auto& oauthService = GetOAuthService();

    if (!oauthService.HasRefreshToken()) {
        SetAnonymousState();
        return;
    }

    if (!oauthService.HasAccessToken()) {
        oauthService.ValidateAuth({}, mAudiocomTrace, true);
    }

    auto& userService = GetUserService();

    if (userService.GetUserSlug().empty()) {
        SetAnonymousState();
        return;
    }

    const auto displayName = userService.GetDisplayName();

    if (!displayName.empty()) {
        mUserName->SetLabel(displayName);
        mUserImage->wxPanel::SetLabel(displayName); // for screen readers
    }

    const auto avatarPath = userService.GetAvatarPath();

    if (!avatarPath.empty()) {
        mUserImage->SetBitmap(avatarPath);
    } else {
        mUserImage->SetBitmap(theTheme.Bitmap(bmpAnonymousUser));
    }

    mLinkButton->SetLabel((mLinkMode == LinkMode::Link
                           ? XXO("&Unlink Account")
                           : XXO("&Sign Out")).Translation());
    mLinkButton->Show();

    OnStateChanged(true);
}

void UserPanel::OnLinkButtonPressed()
{
    auto& oauthService = GetOAuthService();

    if (oauthService.HasAccessToken()) {
        oauthService.UnlinkAccount(mAudiocomTrace);
    } else {
        OpenInDefaultBrowser({ audacity::ToWXString(
                                   mServiceConfig.GetOAuthLoginPage(mAudiocomTrace)) });

#ifdef HAS_CUSTOM_URL_HANDLING
        if (!URLSchemesRegistry::Get().IsURLHandlingSupported())
#endif
        {
            LinkWithTokenDialog dlg(mAudiocomTrace, this);
            dlg.ShowModal();
        }
    }
}

void UserPanel::SetAnonymousState()
{
    mUserName->SetLabel(anonymousText.Translation());
    mUserImage->SetBitmap(theTheme.Bitmap(bmpAnonymousUser));
    mUserImage->SetLabel(anonymousText);  // for screen readers
    mLinkButton->SetLabel(XXO("&Link Account").Translation());
    mLinkButton->Show(mLinkMode == LinkMode::Link);

    OnStateChanged(false);
}
} // namespace audacity::cloud::audiocom
