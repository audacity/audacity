/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectUtils.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudProjectUtils.h"

#include <wx/log.h>

#include "AuthorizationHandler.h"
#include "BasicUI.h"
#include "CloudSettings.h"
#include "CloudSyncService.h"
#include "CodeConversions.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "ProjectFileManager.h"
#include "ProjectManager.h"
#include "ProjectWindow.h"
#include "ServiceConfig.h"
#include "UriParser.h"

#include "ui/dialogs/LinkFailedDialog.h"
#include "ui/dialogs/ProjectVersionConflictDialog.h"
#include "ui/dialogs/UpdateCloudPreviewDialog.h"

#include "sync/CloudSyncUtils.h"
#include "sync/MixdownUploader.h"
#include "sync/ProjectCloudExtension.h"

namespace cloud::audiocom::sync
{
namespace
{
AudacityProject* GetPotentialTarget()
{
   return AllProjects {}.empty() ? nullptr : AllProjects {}.rbegin()->get();
}

auto MakeProgress()
{
   return BasicUI::MakeProgress(
      XO("Open from audio.com"), XO("Synchronizing project"),
      BasicUI::ProgressShowCancel);
}

auto MakePoller(BasicUI::ProgressDialog& dialog)
{
   return [&dialog](double progress)
   {
      return dialog.Poll(static_cast<unsigned>(progress * 10000), 10000ULL) ==
             BasicUI::ProgressResult::Success;
   };
}

template<typename T>
T GetResult(std::future<T>& future)
{
   while (future.wait_for(std::chrono::milliseconds(50)) !=
          std::future_status::ready)
      BasicUI::Yield();

   return future.get();
}

bool HandleFailure(const ProjectSyncResult& result)
{
   if (
      result.Status == ProjectSyncResult::StatusCode::Succeeded ||
      result.Result.Code == ResponseResultCode::Conflict)
      return false;

   BasicUI::ShowErrorDialog(
      {}, XO("Error"), XO("Failed to open cloud project"), {},
      BasicUI::ErrorDialogOptions {}.Log(
         audacity::ToWString(result.Result.Content)));

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
   if (result.Result.Code != ResponseResultCode::Conflict)
      return ConflictResolution::None;

   ProjectVersionConflictDialog dialog { project, false };
   const auto resolution = dialog.ShowDialog();

   if (resolution == ProjectVersionConflictDialog::CancellButtonIdentifier())
      return ConflictResolution::Stop;

   if (resolution == ProjectVersionConflictDialog::UseLocalIdentifier())
      return ConflictResolution::Local;

   if (resolution == ProjectVersionConflictDialog::UseRemoteIdentifier())
      return ConflictResolution::Remote;

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

void OpenProjectFromCloud(
   AudacityProject* potentialTarget, std::string_view projectId,
   std::string_view snapshotId, CloudSyncService::SyncMode mode)
{
   ASSERT_MAIN_THREAD();

   auto authResult = PerformBlockingAuth(potentialTarget);

   if (authResult.Result != AuthResult::Status::Authorised)
   {
      LinkFailedDialog dialog { potentialTarget != nullptr ?
                                   &ProjectWindow::Get(*potentialTarget) :
                                   nullptr };
      dialog.ShowModal();
      return;
   }

   auto progressDialog = MakeProgress();

   auto future = CloudSyncService::Get().OpenFromCloud(
      std::string(projectId), std::string(snapshotId), mode,
      MakePoller(*progressDialog));

   auto result = GetResult(future);
   LogTransferStats(result.Stats);

   progressDialog.reset();

   const auto conflictResolution =
      GetConfilctResolution(potentialTarget, result);

   if (conflictResolution == ConflictResolution::Stop)
      return;

   if (conflictResolution == ConflictResolution::Remote)
   {
      OpenProjectFromCloud(
         potentialTarget, projectId, snapshotId,
         CloudSyncService::SyncMode::ForceOverwrite);
      return;
   }

   if (HandleFailure(result))
      return;

   auto project = ProjectManager::OpenProject(
      GetPotentialTarget(), audacity::ToWXString(result.ProjectPath), true,
      false);

   if (project != nullptr && mode == CloudSyncService::SyncMode::ForceNew)
      ProjectFileIO::Get(*project).MarkTemporary();
}

void OpenProjectFromCloud(
   AudacityProject* potentialTarget, std::string_view projectId,
   std::string_view snapshotId, bool forceNew)
{
   OpenProjectFromCloud(
      potentialTarget, projectId, snapshotId,
      forceNew ? CloudSyncService::SyncMode::ForceNew :
                 CloudSyncService::SyncMode::Normal);
}

bool SyncCloudProject(
   AudacityProject& project, std::string_view path, bool force)
{
   ASSERT_MAIN_THREAD();

   if (!CloudSyncService::Get().IsCloudProject(std::string(path)))
      return true;

   auto authResult = PerformBlockingAuth(&project);

   if (authResult.Result != AuthResult::Status::Authorised)
   {
      LinkFailedDialog dialog { &ProjectWindow::Get(project) };
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

   if (conflictResolution == ConflictResolution::Stop)
      return false;

   if (conflictResolution == ConflictResolution::Remote)
      return SyncCloudProject(project, path, true);

   if (HandleFailure(result))
      return false;

   return true;
}

void SaveToCloud(AudacityProject& project, UploadMode mode)
{
   ASSERT_MAIN_THREAD();

   auto& projectCloudExtension = ProjectCloudExtension::Get(project);

   if (!projectCloudExtension.IsCloudProject())
      projectCloudExtension.MarkPendingCloudSave();

   projectCloudExtension.SetUploadModeForNextSave(mode);
   ProjectFileManager::Get(project).Save();
}

bool HandleProjectLink(std::string_view uri)
{
   ASSERT_MAIN_THREAD();

   const auto parsedUri = ParseUri(uri);

   if (parsedUri.Scheme != "audacity" || parsedUri.Host != "open")
      return false;

   const auto queryParameters = ParseUriQuery(parsedUri.Query);

   if (queryParameters.empty())
      return false;

   const auto projectId = queryParameters.find("projectId");

   if (projectId == queryParameters.end())
      return false;

   const auto snapshotId = queryParameters.find("snapshotId");

   const auto forceNew = queryParameters.find("new") != queryParameters.end();

   OpenProjectFromCloud(
      GetPotentialTarget(), projectId->second,
      snapshotId != queryParameters.end() ? snapshotId->second :
                                            std::string_view {},
      forceNew);

   return true;
}

void UploadMixdown(AudacityProject& project, const UploadUrls& urls)
{
   auto& projectCloudExtension = ProjectCloudExtension::Get(project);

   const auto frequency = MixdownGenerationFrequency.Read();

   const auto forceMixdown =
      frequency == 0 &&
      projectCloudExtension.GetSavesCountSinceMixdown() % 5 == 0 &&
      UpdateCloudPreviewDialog { &project }.ShowDialog() ==
         UpdateCloudPreviewDialog::RenderPreviewIdentifier();

   if (!projectCloudExtension.NeedsMixdownSync() && !forceMixdown)
      return;

   auto progressDialog = BasicUI::MakeProgress(
      XO("Save to audio.com"), XO("Generating mixdown..."),
      BasicUI::ProgressShowCancel);

   auto mixdownUploader = MixdownUploader::Upload(
      GetServiceConfig(), project,
      [progressDialog = progressDialog.get()](auto progress)
      {
         return progressDialog->Poll(
                   static_cast<unsigned>(progress * 10000), 10000) ==
                BasicUI::ProgressResult::Success;
      });

   mixdownUploader->SetUrls(urls);

   auto subscription = projectCloudExtension.SubscribeStatusChanged(
      [progressDialog = progressDialog.get(),
       mixdownUploader](const CloudStatusChangedMessage& message)
      {
         if (message.Status != ProjectSyncStatus::Failed)
            return;

         mixdownUploader->Cancel();
      },
      true);

   auto future = mixdownUploader->GetResultFuture();

   while (future.wait_for(std::chrono::milliseconds(50)) !=
          std::future_status::ready)
      BasicUI::Yield();

   auto result = future.get();

   if (result.State == MixdownState::Succeeded)
      projectCloudExtension.MixdownSynced();
}

bool ResaveLocally(AudacityProject& project)
{
   // TODO: Delete the old file immediately?
   return ProjectFileManager::Get(project).SaveAs();
}

void ReopenProject(AudacityProject& project)
{
   auto& projectCloudExtension = ProjectCloudExtension::Get(project);

   if (!projectCloudExtension.IsCloudProject())
      return;

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
} // namespace cloud::audiocom::sync
