/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectFileIOExtensions.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudProjectFileIOExtensions.h"

#include "AuthorizationHandler.h"
#include "CloudLibrarySettings.h"
#include "CloudProjectOpenUtils.h"

#include "ExportUtils.h"
#include "OAuthService.h"
#include "ServiceConfig.h"
#include "UserService.h"

#include "ui/dialogs/CloudLocationDialog.h"
#include "ui/dialogs/CloudProjectPropertiesDialog.h"
#include "ui/dialogs/LinkFailedDialog.h"
#include "ui/dialogs/SyncInBackroundDialog.h"
#include "ui/dialogs/SyncSucceededDialog.h"

#include "sync/CloudSyncDTO.h"
#include "sync/LocalProjectSnapshot.h"
#include "sync/ProjectCloudExtension.h"
#include "sync/ResumedSnaphotUploadOperation.h"

#include "BasicUI.h"
#include "CodeConversions.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "ProjectFileIOExtension.h"
#include "ProjectFileManager.h"
#include "ProjectWindow.h"

namespace {
using namespace audacity::cloud::audiocom;
using namespace audacity::cloud::audiocom::sync;

class IOExtension final : public ProjectFileIOExtension
{
    OnOpenAction
    OnOpen(AudacityProject& project, const std::string& path) override
    {
        return SyncCloudProject(
            project, path, AudiocomTrace::OpenFromCloudMenu)
               ? OnOpenAction::Continue
               : OnOpenAction::Cancel;
    }

    void OnLoad(AudacityProject& project) override
    {
        auto& projectCloudExtenstion = ProjectCloudExtension::Get(project);
        projectCloudExtenstion.OnLoad();

        if (projectCloudExtenstion.IsCloudProject()) {
            ResumeProjectUpload(
                projectCloudExtenstion, [&project](AudiocomTrace trace) {
                PerformBlockingAuth(&project, trace);
            });
        }
    }

    OnSaveAction PerformCloudSave(
        AudacityProject& project, std::string name, std::string filePath,
        const ProjectSaveCallback& projectSaveCallback, bool fileRenamed,
        AudiocomTrace trace)
    {
        auto& projectCloudExtension = ProjectCloudExtension::Get(project);

        projectCloudExtension.OnSyncStarted();

        auto future = LocalProjectSnapshot::Create(
            GetServiceConfig(), GetOAuthService(), projectCloudExtension, name,
            mUploadMode, trace);

        mUploadMode = UploadMode::Normal;

        // Do we need UI here?
        // while (future.wait_for(std::chrono::milliseconds(50)) !=
        //       std::future_status::ready)
        //   BasicUI::Yield();

        auto result = future.get();

        if (!result.Response) {
            // Prevent any updates to the file to preserve the correct state.
            // Errors would be handled by the UI extension
            return OnSaveAction::Cancelled;
        }

        if (!projectSaveCallback(audacity::ToUTF8(filePath), fileRenamed)) {
            if (result.Operation) {
                result.Operation->Abort();
            } else {
                projectCloudExtension.OnSyncCompleted(
                    nullptr, CloudSyncError { CloudSyncError::Aborted }, trace);
            }
            // Something has failed horrible during the save
            return OnSaveAction::Cancelled;
        }

        if (mSnapshotCallback) {
            mSnapshotCallback(*result.Response);
            mSnapshotCallback = {};
        }

        return OnSaveAction::Handled;
    }

    OnSaveAction SaveCloudProject(
        AudacityProject& project, const ProjectSaveCallback& projectSaveCallback,
        AudiocomTrace trace)
    {
        auto authResult = PerformBlockingAuth(&project, trace);

        if (authResult.Result == AuthResult::Status::Failure) {
            LinkFailedDialog dialog { &ProjectWindow::Get(project), trace };
            dialog.ShowModal();
            // Pretend we have canceled the save
            return OnSaveAction::Cancelled;
        } else if (authResult.Result == AuthResult::Status::Cancelled) {
            return OnSaveAction::Cancelled;
        } else if (authResult.Result == AuthResult::Status::UseAlternative) {
            ProjectFileIO::Get(project).MarkTemporary();
            return OnSaveAction::Continue;
        }

        return PerformCloudSave(
            project, audacity::ToUTF8(project.GetProjectName()),
            audacity::ToUTF8(ProjectFileIO::Get(project).GetFileName()),
            projectSaveCallback, false, trace);
    }

    OnSaveAction OnSave(
        AudacityProject& project,
        const ProjectSaveCallback& projectSaveCallback) override
    {
        auto& projectCloudExtension = ProjectCloudExtension::Get(project);
        auto& projectFileIO         = ProjectFileIO::Get(project);

        const bool isTemporary    = projectFileIO.IsTemporary();
        const bool isCloudProject = projectCloudExtension.IsCloudProject();

        const bool forceSaveToCloud
            =mAudiocomTrace == AudiocomTrace::SaveToCloudMenu;

        Finally finally { [&] {
                mAudiocomTrace = AudiocomTrace::SaveProjectSaveToCloudMenu;
            } };

        auto parent = &ProjectWindow::Get(project);

        // Check location first
        if (isTemporary && !forceSaveToCloud) {
            CloudLocationDialog cloudLocationDialog { parent,
                                                      LocationDialogType::Save };
            const auto saveAction = cloudLocationDialog.ShowDialog();

            // Not doing a cloud save
            if (saveAction == LocationDialogResult::Local) {
                return OnSaveAction::Continue;
            } else if (saveAction == LocationDialogResult::Cancel) {
                return OnSaveAction::Cancelled;
            }
        }

        // For regular projects - do nothing
        if (!isTemporary && !forceSaveToCloud && !isCloudProject) {
            return OnSaveAction::Continue;
        }

        const auto trace = forceSaveToCloud
                           ? AudiocomTrace::SaveToCloudMenu
                           : AudiocomTrace::SaveProjectSaveToCloudMenu;

        if (!isTemporary) {
            return SaveCloudProject(project, projectSaveCallback, trace);
        }

        auto result = CloudProjectPropertiesDialog::Show(
            GetServiceConfig(), GetOAuthService(), GetUserService(),
            project.GetProjectName(), parent, false, trace);

        if (result.first == CloudProjectPropertiesDialog::Action::Cancel) {
            // Suppress the Save function completely
            return OnSaveAction::Cancelled;
        } else if (
            result.first == CloudProjectPropertiesDialog::Action::SaveLocally) {
            // Just let the things flow as usual
            return OnSaveAction::Continue;
        }

        const auto dir = CloudProjectsSavePath.Read();
        FileNames::MkDir(dir);

        const auto filePath = sync::MakeSafeProjectPath(dir, result.second);

        return PerformCloudSave(
            project, result.second, audacity::ToUTF8(filePath),
            projectSaveCallback, true, mAudiocomTrace);
    }

    OnCloseAction OnClose(AudacityProject& project) override
    {
        auto& projectCloudExtension = ProjectCloudExtension::Get(project);

        if (!projectCloudExtension.IsCloudProject()) {
            return OnCloseAction::Continue;
        }

        return OnCloseHook::Call(project) ? OnCloseAction::Continue
               : OnCloseAction::Veto;
    }

    void OnUpdateSaved(
        AudacityProject& project, const ProjectSerializer& serializer) override
    {
        auto& projectCloudExtension = ProjectCloudExtension::Get(project);

        if (!projectCloudExtension.IsCloudProject()) {
            return;
        }

        projectCloudExtension.OnUpdateSaved(serializer);

        const int savesCount = projectCloudExtension.GetSavesCount();

        if (savesCount < 2) {
            SyncInBackroundDialog { &project }.ShowDialog();
        }

        if (projectCloudExtension.IsFirstSyncDialogShown()) {
            return;
        }

        ShowDialogOn(
            [weakProject = project.weak_from_this()] {
            auto project = weakProject.lock();

            if (!project) {
                return true;
            }

            return ProjectCloudExtension::Get(*project)
                   .GetCurrentSyncStatus() == ProjectSyncStatus::Synced;
        },
            [weakProject = project.weak_from_this(), trace = mAudiocomTrace] {
            auto project = weakProject.lock();

            if (
                !project
                || ProjectCloudExtension::Get(*project).IsFirstSyncDialogShown()) {
                return;
            }

            ProjectCloudExtension::Get(*project).SetFirstSyncDialogShown(true);

            const auto result
                =SyncSucceededDialog { project.get() }.ShowDialog();

            if (result == SyncSucceededDialog::ViewOnlineIdentifier()) {
                BasicUI::OpenInDefaultBrowser(audacity::ToWXString(
                                                  ProjectCloudExtension::Get(*project).GetCloudProjectPage(
                                                      trace)));
            }
        });
    }

    bool
    IsBlockLocked(const AudacityProject& project, int64_t blockId) const override
    {
        return ProjectCloudExtension::Get(project).IsBlockLocked(blockId);
    }

public:
    void ForceCloudSave()
    {
        mAudiocomTrace = AudiocomTrace::SaveToCloudMenu;
    }

    void SetUploadModeForNextSave(UploadMode mode)
    {
        mUploadMode = mode;
    }

    void SetSnapshotCallbackForNextSave(CreateSnapshotCallback callback)
    {
        mSnapshotCallback = std::move(callback);
    }

private:
    // Snapshot callback for the next save
    CreateSnapshotCallback mSnapshotCallback;
    // Upload mode for the next save
    UploadMode mUploadMode { UploadMode::Normal };
    // Forces the next save to be a cloud save
    AudiocomTrace mAudiocomTrace { AudiocomTrace::SaveProjectSaveToCloudMenu };
};

IOExtension& GetExtension()
{
    static IOExtension extension;
    return extension;
}

ProjectFileIOExtensionRegistry::Extension extension { GetExtension() };
} // namespace

namespace audacity::cloud::audiocom::sync {
void SaveToCloud(
    AudacityProject& project, UploadMode mode,
    CreateSnapshotCallback snapshotCallback)
{
    ASSERT_MAIN_THREAD();

    auto& projectCloudExtension = ProjectCloudExtension::Get(project);

    auto& ioExtension = GetExtension();

    ioExtension.ForceCloudSave();
    ioExtension.SetUploadModeForNextSave(mode);
    ioExtension.SetSnapshotCallbackForNextSave(std::move(snapshotCallback));

    ProjectFileManager::Get(project).Save();
}

bool ResaveLocally(AudacityProject& project)
{
    // TODO: Delete the old file immediately?
    return ProjectFileManager::Get(project).SaveAs();
}
} // namespace audacity::cloud::audiocom::sync
