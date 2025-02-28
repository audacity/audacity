/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AuthorizationHandler.cpp

  Dmitry Vedenko

**********************************************************************/
#include "AuthorizationHandler.h"

#include <chrono>
#include <future>
#include <optional>

#include "OAuthService.h"
#include "ServiceConfig.h"

#include "ui/dialogs/LoginDialog.h"
#include "ui/dialogs/LinkAccountDialog.h"
#include "ui/dialogs/LinkFailedDialog.h"
#include "ui/dialogs/LinkSucceededDialog.h"
#include "ui/dialogs/WaitForActionDialog.h"

#include "CodeConversions.h"
#include "HelpSystem.h"
#include "MemoryX.h"

namespace audacity::cloud::audiocom {
namespace {
AuthorizationHandler handler;

std::optional<AuthResult> WaitForAuth(
    std::future<std::optional<AuthResult> > future,
    const AudacityProject* project)
{
    using namespace sync;

    if (
        future.wait_for(std::chrono::milliseconds { 100 })
        != std::future_status::ready) {
        auto waitResult
            =WaitForActionDialog {
            project, XO("Waiting for audio.com"),
            XO("An action on audio.com is required before you can continue. You can cancel this operation."),
            false
            }
        .ShowDialog(
            [&future]() -> DialogButtonIdentifier
        {
            if (
                future.wait_for(std::chrono::milliseconds { 50 })
                != std::future_status::ready) {
                return {}
            }

            return { L"done" };
        });

        if (waitResult == WaitForActionDialog::CancelButtonIdentifier()) {
            return AuthResult { AuthResult::Status::Cancelled, {} }
        }
    }

    if (GetOAuthService().HasAccessToken()) {
        return AuthResult { AuthResult::Status::Authorised, {} }
    }

    return future.get();
}
} // namespace

AuthorizationHandler& GetAuthorizationHandler()
{
    return handler;
}

AuthResult PerformBlockingAuth(
    AudacityProject* project, AudiocomTrace trace,
    const TranslatableString& alternativeActionLabel)
{
    using namespace sync;
    auto& oauthService = GetOAuthService();

    // Assume, that the token is valid
    // Services will need to handle 403 errors and refresh the token
    if (GetOAuthService().HasAccessToken()) {
        return { AuthResult::Status::Authorised, {} }
    }

    GetAuthorizationHandler().PushSuppressDialogs();
    auto popSuppress
        =finally([] { GetAuthorizationHandler().PopSuppressDialogs(); });

    if (oauthService.HasRefreshToken()) {
        std::promise<std::optional<AuthResult> > promise;

        oauthService.ValidateAuth(
            [&promise](auto...) { promise.set_value({}); }, trace, true);

        if (auto waitResult = WaitForAuth(promise.get_future(), project)) {
            return *waitResult;
        }
    }

    auto linkResult
        =sync::LinkAccountDialog { project, alternativeActionLabel }.ShowDialog();

    if (linkResult == LinkAccountDialog::CancelButtonIdentifier()) {
        return { AuthResult::Status::Cancelled, {} }
    }

    if (linkResult == LinkAccountDialog::AlternativeButtonIdentifier()) {
        return { AuthResult::Status::UseAlternative, {} }
    }

    std::promise<std::optional<AuthResult> > promise;

    auto authSubscription = oauthService.Subscribe(
        [&promise](auto& result)
    {
        promise.set_value(
            result.authorised
            ? AuthResult { AuthResult::Status::Authorised, {} }
            : AuthResult { AuthResult::Status::Failure,
                           std::string(result.errorMessage) });
    });

    LoginDialog::SignIn(nullptr, LoginDialog::Mode::SignIn);

    auto waitResult = WaitForAuth(promise.get_future(), project);

    if (waitResult) {
        return *waitResult;
    }

    return AuthResult { AuthResult::Status::Failure, {} };
}

AuthorizationHandler::AuthorizationHandler()
    : mAuthStateChangedSubscription(GetOAuthService().Subscribe(
                                        [this](const auto& message) { OnAuthStateChanged(message); }))
{
}

void AuthorizationHandler::PushSuppressDialogs()
{
    ++mSuppressed;
}

void AuthorizationHandler::PopSuppressDialogs()
{
    assert(mSuppressed > 0);

    if (mSuppressed > 0) {
        --mSuppressed;
    }
}

void AuthorizationHandler::OnAuthStateChanged(
    const AuthStateChangedMessage& message)
{
    if (mSuppressed > 0 || message.silent) {
        return;
    }

    if (!message.errorMessage.empty()) {
        LinkFailedDialog dialog { nullptr, message.trace };
        dialog.ShowModal();
    } else if (message.authorised) {
        LinkSucceededDialog dialog { nullptr };
        dialog.ShowModal();
    }
}
} // namespace audacity::cloud::audiocom
