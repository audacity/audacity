/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectOpenUtils.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudProjectOpenUtils.h"

#include <wx/log.h>

#include "AuthorizationHandler.h"
#include "BasicUI.h"
#include "CloudSyncService.h"
#include "CodeConversions.h"
#include "ExportUtils.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "ProjectManager.h"
#include "ProjectWindow.h"
#include "UriParser.h"

#include "ui/dialogs/LinkFailedDialog.h"
#include "ui/dialogs/ProjectVersionConflictDialog.h"
#include "ui/dialogs/SyncFailedDialog.h"
#include "ui/dialogs/UnsyncedProjectDialog.h"

#include "sync/ProjectCloudExtension.h"

namespace audacity::cloud::audiocom::sync {
namespace {
auto MakeProgress()
{
    return BasicUI::MakeProgress(
        XO("Open trace audio.com"), XO("Synchronizing project"),
        BasicUI::ProgressShowCancel);
}

auto MakePoller(BasicUI::ProgressDialog& dialog)
{
    return [&dialog](double progress)
    {
        return dialog.Poll(static_cast<unsigned>(progress * 10000), 10000ULL)
               == BasicUI::ProgressResult::Success;
    };
}

template<typename T>
T GetResult(std::future<T>& future)
{
    while (future.wait_for(std::chrono::milliseconds(50))
           != std::future_status::ready) {
        BasicUI::Yield();
    }

    return future.get();
}

bool HandleFailure(const ProjectSyncResult& result)
{
    if (
        result.Status == ProjectSyncResult::StatusCode::Succeeded
        || result.Result.Code == SyncResultCode::Conflict) {
        return false;
    }

    if (result.Result.Code == SyncResultCode::Cancelled) {
        wxLogError(
            "Opening of the cloud project was canceled", result.Result.Content);
        return true;
    }

    if (result.Result.Code == SyncResultCode::SyncImpossible) {
        UnsyncedProjectDialog { nullptr, false }.ShowDialog();
        return true;
    }

    SyncFailedDialog::OnOpen(result.Result);

    wxLogError("Failed to open cloud project: %s", result.Result.Content);

    return true;
}

enum class ConflictResolution
{
    None,
    Remote,
    Local,
    Stop
};

ConflictResolution
GetConfilctResolution(AudacityProject* project, const ProjectSyncResult& result)
{
    if (result.Result.Code != SyncResultCode::Conflict) {
        return ConflictResolution::None;
    }

    ProjectVersionConflictDialog dialog {
        project, ProjectVersionConflictDialogMode::OpenDirty
    };
    const auto resolution = dialog.ShowDialog();

    if (resolution == ProjectVersionConflictDialog::CancelButtonIdentifier()) {
        return ConflictResolution::Stop;
    }

    if (resolution == ProjectVersionConflictDialog::UseLocalIdentifier()) {
        return ConflictResolution::Local;
    }

    if (resolution == ProjectVersionConflictDialog::UseRemoteIdentifier()) {
        return ConflictResolution::Remote;
    }

    assert(false);
    return ConflictResolution::Stop;
}

void LogTransferStats(TransferStats stats)
{
    wxLogMessage(
        "Transfer stats: %f Kb transferred, %f secs",
        stats.BytesTransferred / 1024.0,
        std::chrono::duration<float>(stats.TransferDuration).count());
}
} // namespace

AudacityProject* GetPotentialTarget()
{
    return AllProjects {}.empty() ? nullptr : AllProjects {}.rbegin()->get();
}

AudacityProject* GetOpenedProject(std::string_view projectId)
{
    const auto begin = AllProjects {}.begin(), end = AllProjects {}.end();
    auto iter = std::find_if(
        begin, end,
        [projectId](const AllProjects::value_type& ptr) {
        return projectId
               == ProjectCloudExtension::Get(*ptr).GetCloudProjectId();
    });

    if (iter == end) {
        return nullptr;
    }

    return iter->get();
}

AudacityProject* OpenProjectFromCloud(
    AudacityProject* potentialTarget, std::string_view projectId,
    std::string_view snapshotId, CloudSyncService::SyncMode mode)
{
    ASSERT_MAIN_THREAD();

    auto authResult
        =PerformBlockingAuth(potentialTarget, AudiocomTrace::OpenFromCloudMenu);

    if (authResult.Result != AuthResult::Status::Authorised) {
        LinkFailedDialog dialog { potentialTarget != nullptr
                                  ? &ProjectWindow::Get(*potentialTarget)
                                  : nullptr,
                                  AudiocomTrace::OpenFromCloudMenu };
        dialog.ShowModal();
        return nullptr;
    }

    auto openedProject = GetOpenedProject(projectId);

    if (openedProject != nullptr && mode != CloudSyncService::SyncMode::ForceNew) {
        if (mode == CloudSyncService::SyncMode::ForceOverwrite) {
            ProjectWindow::Get(*openedProject).Close(true);
        } else {
            auto snapshotIdFuture
                =CloudSyncService::Get().GetHeadSnapshotID(std::string(projectId));

            auto snapshotIdResult = GetResult(snapshotIdFuture);

            if (std::holds_alternative<std::string>(snapshotIdResult)) {
                const auto headSnapshotId
                    =*std::get_if<std::string>(&snapshotIdResult);

                if (
                    headSnapshotId
                    != ProjectCloudExtension::Get(*openedProject).GetSnapshotId()) {
                    auto dialog = std::make_unique<ProjectVersionConflictDialog>(
                        openedProject, ProjectVersionConflictDialogMode::OpenActive);

                    const auto result = dialog->ShowDialog();
                    dialog.reset();

                    if (
                        result == ProjectVersionConflictDialog::UseRemoteIdentifier()) {
                        auto newProject = ProjectManager::New();
                        return OpenProjectFromCloud(
                            newProject, projectId, headSnapshotId,
                            CloudSyncService::SyncMode::ForceOverwrite);
                    }
                }
            }

            ProjectWindow::Get(*openedProject).Raise();
            return openedProject;
        }
    }

    auto progressDialog = MakeProgress();

    auto future = CloudSyncService::Get().OpenFromCloud(
        std::string(projectId), std::string(snapshotId), mode,
        MakePoller(*progressDialog));

    auto result = GetResult(future);
    LogTransferStats(result.Stats);

    progressDialog.reset();

    const auto conflictResolution
        =GetConfilctResolution(potentialTarget, result);

    if (conflictResolution == ConflictResolution::Stop) {
        return nullptr;
    }

    if (conflictResolution == ConflictResolution::Remote) {
        return OpenProjectFromCloud(
            potentialTarget, projectId, snapshotId,
            CloudSyncService::SyncMode::ForceOverwrite);
    }

    if (HandleFailure(result)) {
        return nullptr;
    }

    auto project = ProjectManager::OpenProject(
        GetPotentialTarget(), audacity::ToWXString(result.ProjectPath), true,
        false);

    if (project != nullptr && mode == CloudSyncService::SyncMode::ForceNew) {
        ProjectFileIO::Get(*project).MarkTemporary();
    }

    return project;
}

AudacityProject* OpenProjectFromCloud(
    AudacityProject* potentialTarget, std::string_view projectId,
    std::string_view snapshotId, bool forceNew)
{
    return OpenProjectFromCloud(
        potentialTarget, projectId, snapshotId,
        forceNew ? CloudSyncService::SyncMode::ForceNew
        : CloudSyncService::SyncMode::Normal);
}

bool SyncCloudProject(
    AudacityProject& project, std::string_view path, AudiocomTrace trace,
    bool force)
{
    ASSERT_MAIN_THREAD();

    if (!CloudSyncService::Get().IsCloudProject(std::string(path))) {
        return true;
    }

    auto authResult = PerformBlockingAuth(&project, trace);

    if (authResult.Result != AuthResult::Status::Authorised) {
        LinkFailedDialog dialog { &ProjectWindow::Get(project), trace };
        dialog.ShowModal();
        return false;
    }

    auto progressDialog = MakeProgress();

    auto future = CloudSyncService::Get().SyncProject(
        project, std::string(path), force, MakePoller(*progressDialog));

    auto result = GetResult(future);
    LogTransferStats(result.Stats);

    progressDialog.reset();

    const auto conflictResolution = GetConfilctResolution(&project, result);

    if (conflictResolution == ConflictResolution::Stop) {
        return false;
    }

    if (conflictResolution == ConflictResolution::Remote) {
        return SyncCloudProject(project, path, trace, true);
    }

    if (HandleFailure(result)) {
        return false;
    }

    return true;
}

bool HandleProjectLink(std::string_view uri)
{
    ASSERT_MAIN_THREAD();

    const auto parsedUri = ParseUri(uri);

    if (parsedUri.Scheme != "audacity" || parsedUri.Host != "open") {
        return false;
    }

    const auto queryParameters = ParseUriQuery(parsedUri.Query);

    if (queryParameters.empty()) {
        return false;
    }

    const auto projectId = queryParameters.find("projectId");

    if (projectId == queryParameters.end()) {
        return false;
    }

    const auto snapshotId = queryParameters.find("snapshotId");

    const auto forceNew = queryParameters.find("new") != queryParameters.end();

    OpenProjectFromCloud(
        GetPotentialTarget(), projectId->second,
        snapshotId != queryParameters.end() ? snapshotId->second
        : std::string_view {},
        forceNew);

    return true;
}

void ReopenProject(AudacityProject& project)
{
    auto& projectCloudExtension = ProjectCloudExtension::Get(project);

    if (!projectCloudExtension.IsCloudProject()) {
        return;
    }

    BasicUI::CallAfter(
        [&project,
         projectId = std::string(projectCloudExtension.GetCloudProjectId())]
    {
        auto newProject = ProjectManager::New();
        ProjectWindow::Get(project).Close(true);
        OpenProjectFromCloud(
            newProject, projectId, {},
            CloudSyncService::SyncMode::ForceOverwrite);
    });
}
} // namespace audacity::cloud::audiocom::sync
