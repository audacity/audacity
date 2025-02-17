/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SyncFailedDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "SyncFailedDialog.h"

#include "CodeConversions.h"

namespace audacity::cloud::audiocom::sync {
namespace {
const auto MessageAuthorizationFailed
    =XO("You are not authorized to access this project.");
const auto MessageExpired
    =XO("You tried to access a project that has expired.");
const auto MessageConnectionFailed
    =XO("Audacity had trouble connecting to the server.");
const auto MessageTooLarge = XO(
    "The project is too large to upload. Please save it to your computer instead.");
const auto MessageForbidden = XO("You don't have access to this project.");
const auto MessageNotFound  = XO("The project could not be found.");
const auto MessageUnexpectedResponse
    =XO("The server responded with something Audacity could not understand.");
const auto MessageInternalClientError
    =XO("Audacity encountered an internal error.");
const auto MessageInternalServerError
    =XO("Audio.com encountered an internal error.");
const auto MessageUnknownError = XO("Audacity encountered an unknown error.");

TranslatableString GetMessage(const CloudSyncError& error)
{
    switch (error.Type) {
    case CloudSyncError::Authorization:
        return MessageAuthorizationFailed;
    case CloudSyncError::Network:
        return MessageConnectionFailed;
    case CloudSyncError::Server:
        return MessageInternalServerError;
    case CloudSyncError::ClientFailure:
        return MessageInternalClientError;
    default:
        return MessageUnknownError;
    }
}

TranslatableString GetMessage(const ResponseResult& error)
{
    switch (error.Code) {
    case SyncResultCode::Unauthorized:
        return MessageAuthorizationFailed;
    case SyncResultCode::Expired:
        return MessageExpired;
    case SyncResultCode::ConnectionFailed:
        return MessageConnectionFailed;
    case SyncResultCode::TooLarge:
        return MessageTooLarge;
    case SyncResultCode::Forbidden:
        return MessageForbidden;
    case SyncResultCode::NotFound:
        return MessageNotFound;
    case SyncResultCode::UnexpectedResponse:
        return MessageUnexpectedResponse;
    case SyncResultCode::InternalServerError:
        return MessageInternalServerError;
    default:
        return MessageUnknownError;
    }
}
} // namespace

void SyncFailedDialog::OnOpen(const CloudSyncError& error)
{
    if (error.Type == CloudSyncError::None) {
        return;
    }

    auto message = GetMessage(error);

    SyncFailedDialog dialog { nullptr, message, error.ErrorMessage,
                              DialogMode::Opening };
    dialog.ShowDialog();
}

void SyncFailedDialog::OnSave(const CloudSyncError& error)
{
    if (error.Type == CloudSyncError::None) {
        return;
    }

    auto message = GetMessage(error);

    SyncFailedDialog dialog { nullptr, message, error.ErrorMessage,
                              DialogMode::Saving };
    dialog.ShowDialog();
}

void SyncFailedDialog::OnOpen(const ResponseResult& error)
{
    if (error.Code == SyncResultCode::Success) {
        return;
    }

    auto message = GetMessage(error);

    SyncFailedDialog dialog { nullptr, message, error.Content,
                              DialogMode::Opening };
    dialog.ShowDialog();
}

void SyncFailedDialog::OnSave(const ResponseResult& error)
{
    if (error.Code == SyncResultCode::Success) {
        return;
    }

    auto message = GetMessage(error);

    SyncFailedDialog dialog { nullptr, message, error.Content,
                              DialogMode::Saving };
    dialog.ShowDialog();
}

SyncFailedDialog::SyncFailedDialog(
    const AudacityProject* project, const TranslatableString& message,
    const std::string& log, DialogMode dialogMode)
    : AudioComDialogBase{project, {}, dialogMode}
{
    AddTitle(XO("Sync failed"));

    AddParagraph(message);

    if (!log.empty()) {
        AddParagraph(XO("Error details:\n%s").Format(ToWXString(log)));
    }

    AddButton(CancelButtonIdentifier(), XO("OK"), DefaultButton | EscButton);
}
} // namespace audacity::cloud::audiocom::sync
