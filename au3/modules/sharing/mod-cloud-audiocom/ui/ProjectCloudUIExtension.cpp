/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectCloudUIExtension.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ProjectCloudUIExtension.h"

#include <wx/log.h>

#include "sync/ProjectCloudExtension.h"
#include "sync/ResumedSnaphotUploadOperation.h"

#include "dialogs/ConnectionIssuesDialog.h"
#include "dialogs/NotCloudProjectDialog.h"
#include "dialogs/ProjectLimitDialog.h"
#include "dialogs/ProjectVersionConflictDialog.h"
#include "dialogs/SyncFailedDialog.h"
#include "dialogs/WaitForActionDialog.h"

#include "CloudProjectFileIOExtensions.h"
#include "CloudProjectOpenUtils.h"

#include "OAuthService.h"
#include "ServiceConfig.h"
#include "UserService.h"

#include "BasicUI.h"
#include "CodeConversions.h"
#include "Project.h"

namespace audacity::cloud::audiocom::sync {
namespace {
const AttachedProjectObjects::RegisteredFactory key {
    [](AudacityProject& project)
    { return std::make_shared<ProjectCloudUIExtension>(project); }
};

OnCloseHook::Scope onCloseHookScope { [](AudacityProject& project) {
        return ProjectCloudUIExtension::Get(project).AllowClosing();
    } };
} // namespace

ProjectCloudUIExtension::ProjectCloudUIExtension(AudacityProject& project)
    : mProject{project}
    , mCloudStatusChangedSubscription{
                                      ProjectCloudExtension::Get(project).SubscribeStatusChanged(
                                          [this](const auto& status) { OnCloudStatusChanged(status); }, true)
                                      }
{
}

ProjectCloudUIExtension::~ProjectCloudUIExtension() = default;

ProjectCloudUIExtension& ProjectCloudUIExtension::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<ProjectCloudUIExtension&>(key);
}

const ProjectCloudUIExtension&
ProjectCloudUIExtension::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

void ProjectCloudUIExtension::SetUploadProgress(double progress)
{
    mProgress = progress;
}

bool ProjectCloudUIExtension::AllowClosing()
{
    while (mInSync.load(std::memory_order_acquire) && !mClosingCancelled)
    {
        if (mProgressDialog == nullptr) {
            mProgressDialog = BasicUI::MakeProgress(
                XO("Save to audio.com"),
                XO("Project is syncing with audio.com. Do you want to stop the sync process?"),
                BasicUI::ProgressShowCancel | BasicUI::ProgressShowStop);
        }

        const auto result = mProgressDialog->Poll(mProgress * 10000, 10000);

        if (result == BasicUI::ProgressResult::Cancelled) {
            mClosingCancelled = true;
            mProgressDialog.reset();
        }

        if (result == BasicUI::ProgressResult::Stopped) {
            ProjectCloudExtension::Get(mProject).CancelSync();
        }

        BasicUI::Yield();
    }

    bool closingCancelled = mClosingCancelled;
    mClosingCancelled     = false;

    mProgressDialog.reset();

    return !mInSync.load(std::memory_order_acquire) || !closingCancelled;
}

void ProjectCloudUIExtension::OnCloudStatusChanged(
    const CloudStatusChangedMessage& message)
{
    mInSync = message.IsSyncing();

    if (!mInSync) {
        mProgressDialog.reset();
    } else {
        SetUploadProgress(message.Progress);
    }

    // It the sync was successful - check for unsuccessful operations before
    if (message.Status == ProjectSyncStatus::Synced) {
        ResumeProjectUpload(ProjectCloudExtension::Get(mProject), {});
    }

    if (message.Status != ProjectSyncStatus::Failed || !message.Error) {
        return;
    }

    const auto error = *message.Error;

    switch (error.Type) {
    case CloudSyncError::Authorization:
        // How do we got here? Probable auth_token is invalid?
        GetOAuthService().UnlinkAccount(message.audiocomTrace);
        SaveToCloud(mProject, UploadMode::Normal);
        break;
    case CloudSyncError::ProjectLimitReached:
        [[fallthrough]];
    case CloudSyncError::ProjectStorageLimitReached:
    {
        auto result = ProjectLimitDialog { &mProject }.ShowDialog();

        if (result == ProjectLimitDialog::VisitAudioComIdentifier()) {
            auto& userService = GetUserService();
            auto& oauthService = GetOAuthService();
            auto& serviceConfig = GetServiceConfig();

            const auto slug = audacity::ToUTF8(userService.GetUserSlug());
            const auto projectsPath = serviceConfig.GetProjectsPagePath(slug, message.audiocomTrace);
            const auto url = oauthService.MakeAudioComAuthorizeURL(slug, projectsPath);

            BasicUI::OpenInDefaultBrowser(url);

            WaitForActionDialog {
                &mProject,
                XO("Waiting for space to free up"),
                XO("Once you have made storage space available on audio.com, click Retry."),
            }
            .ShowDialog();
            SaveToCloud(mProject, UploadMode::Normal);
        } else if (result == ProjectLimitDialog::SaveLocallyButtonIdentifier()) {
            if (!ResaveLocally(mProject)) {
                SaveToCloud(mProject, UploadMode::Normal);
            }
        }
    }
    break;
    case CloudSyncError::ProjectVersionConflict:
    {
        if (
            ProjectVersionConflictDialog { &mProject,
                                           ProjectVersionConflictDialogMode::Save }
            .ShowDialog() == ProjectVersionConflictDialog::UseLocalIdentifier()) {
            SaveToCloud(mProject, UploadMode::ForceOverwrite);
        } else {
            ReopenProject(mProject);
        }
    }
    break;
    case CloudSyncError::ProjectNotFound:
    {
        if (
            NotCloudProjectDialog { &mProject }.ShowDialog()
            == NotCloudProjectDialog::SaveLocallyIdentifier()) {
            if (!ResaveLocally(mProject)) {
                SaveToCloud(mProject, UploadMode::CreateNew);
            }
        } else {
            SaveToCloud(mProject, UploadMode::CreateNew);
        }
    }
    break;
    case CloudSyncError::Network:
    {
        ConnectionIssuesDialog { &mProject }.ShowDialog();
    }
    break;
    case CloudSyncError::DataUploadFailed:
        [[fallthrough]];
    case CloudSyncError::Server:
        [[fallthrough]];
    case CloudSyncError::ClientFailure:
        SyncFailedDialog::OnSave(error);
        break;
    case CloudSyncError::Cancelled:
        [[fallthrough]];
    default:
        break;
    }

    wxLogError(
        "Cloud sync has failed: %s", audacity::ToWXString(error.ErrorMessage));
}
} // namespace audacity::cloud::audiocom::sync
