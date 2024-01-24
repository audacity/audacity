/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncService.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudSyncService.h"

#include <algorithm>
#include <cassert>
#include <chrono>
#include <mutex>
#include <string>

#include "CloudSettings.h"

#include "sync/CloudProjectsDatabase.h"
#include "sync/LocalProjectSnapshot.h"
#include "sync/ProjectCloudExtension.h"
#include "sync/CloudSyncUI.h"
#include "sync/CloudSyncUtils.h"
#include "sync/RemoteProjectSnapshot.h"

#include "CodeConversions.h"

#include "ServiceConfig.h"
#include "OAuthService.h"

#include "MemoryX.h"

#include "Project.h"
#include "ProjectFileIO.h"

#include "BasicUI.h"
#include "FileNames.h"

#include "wxFileNameWrapper.h"

#include "NetworkManager.h"
#include "Request.h"
#include "IResponse.h"


namespace cloud::audiocom
{
namespace
{
class DefaultCloudSyncUI : public sync::CloudSyncUI
{
public:
   sync::SaveResult OnHandleFirstSave(
      const AudacityProject&, const BasicUI::WindowPlacement&) override
   {
      return {};
   }

   sync::SaveResult OnHandleSave(
      const AudacityProject&, const BasicUI::WindowPlacement&) override
   {
      return {};
   }

   bool OnAuthorizationRequired(const BasicUI::WindowPlacement&) override
   {
      return {};
   }

   bool OnUploadProgress(AudacityProject*, double) override
   {
      return true;
   }

   void OnUploadFailed(AudacityProject*, std::string) override
   {
   }

   void OnUploadSucceeded(AudacityProject*) override
   {
   }

   void OnDownloadStarted() override
   {
   }

   bool OnDownloadProgress(double) override
   {
      return true;
   }

   void OnDownloadFinished() override
   {
   }

   void ShowDownloadError(std::string) override
   {
   }

   sync::DownloadConflictResolution
   OnDownloadConflict(const BasicUI::WindowPlacement&) override
   {
      return sync::DownloadConflictResolution::Remote;
   }

   void OnMixdownStarted() override
   {
   }

   void SetMixdownProgressMessage(const TranslatableString& message) override
   {
   }

   bool OnMixdownProgress(double progress) override
   {
      return true;
   }

   void OnMixdownFinished() override
   {
   }
}; // class DefaultCloudSyncUI

std::mutex& GetResponsesMutex()
{
   static std::mutex mutex;
   return mutex;
}

std::vector<std::shared_ptr<audacity::network_manager::IResponse>>&
GetPendingRequests()
{
   static std::vector<std::shared_ptr<audacity::network_manager::IResponse>>
      requests;

   return requests;
}

void RemovePendingRequest(audacity::network_manager::IResponse* request)
{
   std::lock_guard lock { GetResponsesMutex() };

   auto& requests = GetPendingRequests();

   requests.erase(
      std::remove_if(
         requests.begin(), requests.end(),
         [request](const auto& r) { return r.get() == request; }),
      requests.end());
}

void PerformProjectGetRequest(
   OAuthService& oAuthService, std::string url,
   std::function<void(std::string, bool)> dataCallback)
{
   assert(oAuthService.HasAccessToken());

   using namespace audacity::network_manager;

   auto request = Request(std::move(url));

   request.setHeader(
      common_headers::ContentType, common_content_types::ApplicationJson);

   request.setHeader(
      common_headers::Accept, common_content_types::ApplicationJson);

   request.setHeader(
      common_headers::Authorization, oAuthService.GetAccessToken());

   auto response = NetworkManager::GetInstance().doGet(request);

   response->setRequestFinishedCallback(
      [dataCallback = std::move(dataCallback)](auto response)
      {
         BasicUI::CallAfter(
            [dataCallback = std::move(dataCallback), response]
            {
               auto removeRequest =
                  finally([response] { RemovePendingRequest(response); });

               auto body = response->template readAll<std::string>();

               if (response->getError() != NetworkError::NoError)
               {
                  dataCallback(
                     response->getError() != NetworkError::HTTPError ?
                        response->getErrorString() :
                        std::move(body),
                     false);
                  return;
               }

               dataCallback(std::move(body), true);
            });
      });

   std::lock_guard lock { GetResponsesMutex() };
   GetPendingRequests().emplace_back(std::move(response));
}

void GetProjectInfo(
   OAuthService& oAuthService, const ServiceConfig& serviceConfig,
   std::string projectId,
   std::function<void(sync::ProjectInfo, std::string, bool)> callback)
{
   assert(callback);

   PerformProjectGetRequest(
      oAuthService, serviceConfig.GetProjectInfoUrl(projectId),
      [callback = std::move(callback)](std::string data, bool success)
      {
         if (!success)
         {
            callback(sync::ProjectInfo {}, std::move(data), false);
            return;
         }

         auto projectInfo = sync::DeserializeProjectInfo(data);

         if (!projectInfo)
         {
            callback(
               {},
               audacity::ToUTF8(
                  XO("Failed to deserialize project info").Translation()),
               false);
            return;
         }

         callback(std::move(*projectInfo), {}, true);
      });
}

void GetSnapshotInfo(
   OAuthService& oAuthService, const ServiceConfig& serviceConfig,
   std::string projectId, std::string snapshotId,
   std::function<void(sync::SnapshotInfo, std::string, bool)> callback)
{
   assert(callback);

   PerformProjectGetRequest(
      oAuthService, serviceConfig.GetSnapshotInfoUrl(projectId, snapshotId),
      [callback = std::move(callback)](std::string data, bool success)
      {
         if (!success)
         {
            callback(sync::SnapshotInfo {}, std::move(data), false);
            return;
         }

         auto snapshotInfo = sync::DeserializeSnapshotInfo(data);

         if (!snapshotInfo)
         {
            callback(
               {},
               audacity::ToUTF8(
                  XO("Failed to deserialize snapshot info").Translation()),
               false);
            return;
         }

         callback(std::move(*snapshotInfo), {}, true);
      });
}

void GetSnapshotInfo(
   OAuthService& oAuthService, const ServiceConfig& serviceConfig,
   std::string projectId, std::string snapshotId,
   std::function<void(sync::ProjectInfo, sync::SnapshotInfo, std::string, bool)>
      callback)
{
   assert(callback);

   GetProjectInfo(
      oAuthService, serviceConfig, projectId,
      [callback = std::move(callback), projectId,
       snapshotId = std::move(snapshotId), &oAuthService, &serviceConfig](
         sync::ProjectInfo projectInfo, std::string errorMessage, bool success)
      {
         if (!success)
         {
            callback({}, {}, std::move(errorMessage), false);
            return;
         }

         const auto id =
            !snapshotId.empty() ? snapshotId : projectInfo.HeadSnapshot.Id;

         GetSnapshotInfo(
            oAuthService, serviceConfig, projectId, id,
            [callback = std::move(callback),
             projectInfo = std::move(projectInfo)](
               sync::SnapshotInfo snapshotInfo, std::string errorMessage,
               bool success)
            {
               callback(
                  std::move(projectInfo), std::move(snapshotInfo),
                  std::move(errorMessage), success);
            });
      });
}

template<typename AuthorizedCallback, typename UnauthorizedCallback>
void PerformAuthorised(
   OAuthService& oAuthService, sync::CloudSyncUI& ui,
   AudacityProject* targetProject, AuthorizedCallback authorizedCallback,
   UnauthorizedCallback unauthorizedCallback)
{
   oAuthService.ValidateAuth(
      [&oAuthService, &ui, targetProject,
       authorizedCallback = std::move(authorizedCallback),
       unauthorizedCallback = std::move(unauthorizedCallback)](auto token)
      {
         BasicUI::CallAfter(
            [&oAuthService, &ui, targetProject,
             authorizedCallback = std::move(authorizedCallback),
             unauthorizedCallback = std::move(unauthorizedCallback),
             authorized = !token.empty()]()
            {
               if (authorized)
               {
                  authorizedCallback();
                  return;
               }

               if (!ui.OnAuthorizationRequired(
                      *ProjectFramePlacement(targetProject)))
               {
                  unauthorizedCallback();
                  return;
               }

               PerformAuthorised(
                  oAuthService, ui, targetProject,
                  std::move(authorizedCallback),
                  std::move(unauthorizedCallback));
            });
      },
      true);
}

bool HasAutosave(const std::string& path)
{
   auto connection = sqlite::Connection::Open(path, sqlite::OpenMode::ReadOnly);

   if (!connection)
      return false;

   if (!connection->CheckTableExists("autosave"))
      return false;

   auto statement = connection->CreateStatement("SELECT COUNT(1) FROM autosave");

   if (!statement)
      return false;

   auto result = statement->Prepare().Run();

   if (!result.IsOk())
      return false;

   for (const auto& row : result)
   {
      if (row.GetOr(0, 0) > 0)
         return true;
   }

   return false;
}

} // namespace

CloudSyncService& CloudSyncService::Get()
{
   static CloudSyncService service;
   return service;
}

void CloudSyncService::GetProjects(
   int page, int pageSize, sync::GetProjectsCallback callback)
{
   using namespace audacity::network_manager;

   if (!callback)
      callback = [](auto...) {};

   PerformAuthorised(
      GetOAuthService(), GetUI(), {},
      [page, pageSize, callback]()
      {
         auto& serviceConfig = GetServiceConfig();
         auto& oAuthService = GetOAuthService();

         auto request = Request(serviceConfig.GetProjectsUrl(page, pageSize));

         request.setHeader(
            common_headers::ContentType, common_content_types::ApplicationJson);
         request.setHeader(
            common_headers::Accept, common_content_types::ApplicationJson);

         const auto language = serviceConfig.GetAcceptLanguageValue();

         if (!language.empty())
            request.setHeader(
               audacity::network_manager::common_headers::AcceptLanguage,
               language);

         request.setHeader(
            common_headers::Authorization, oAuthService.GetAccessToken());

         auto response = NetworkManager::GetInstance().doGet(request);

         response->setRequestFinishedCallback(
            [callback = std::move(callback), response](auto)
            {
               const auto& body = response->readAll<std::string>();

               if (response->getError() != NetworkError::NoError)
               {
                  callback(
                     sync::PaginatedProjectsResponse {},
                     response->getError() != NetworkError::HTTPError ?
                        response->getErrorString() :
                        body,
                     false);
                  return;
               }

               auto projects = sync::DeserializePaginatedProjectsResponse(body);

               if (!projects)
               {
                  callback(
                     {},
                     audacity::ToUTF8(
                        XO("Failed to parse projects response")
                           .Translation()),
                     false);
                  return;
               }

               callback(std::move(*projects), {}, true);
            });
      },
      [callback]()
      {
         callback(
            {}, audacity::ToUTF8(XO("Authorization required").Translation()),
            false);
      });
}

void CloudSyncService::SaveToCloud(AudacityProject& project)
{
   auto& cloudExtension = sync::ProjectCloudExtension::Get(project);

   if (cloudExtension.IsCloudProject())
   {
      ProjectFileIO::Get(project).UpdateSaved(nullptr);
      return;
   }

   const auto result =
      GetUI().OnHandleSave(project, *ProjectFramePlacement(&project));

   if (!result.SaveToCloud)
      return;

   DoCloudSave(project, result.Title);
}

void CloudSyncService::OpenFromCloud(
   AudacityProject* targetProject, std::string projectId,
   std::string snapshotId, sync::ProjectDownloadedCallback callback)
{
   if (!callback)
      callback = [](auto...) {};

   GetUI().OnDownloadStarted();

   PerformAuthorised(
      GetOAuthService(), GetUI(), targetProject,
      [this, projectId = std::move(projectId),
       snapshotId = std::move(snapshotId), targetProject, callback]()
      {
         GetSnapshotInfo(
            GetOAuthService(), GetServiceConfig(), projectId, snapshotId,
            [this, targetProject, callback = std::move(callback)](
               sync::ProjectInfo projectInfo, sync::SnapshotInfo snapshotInfo,
               std::string errorMessage, bool success)
            {
               if (!success)
               {
                  GetUI().OnDownloadFinished();

                  callback({ targetProject,
                             {},
                             std::move(errorMessage),
                             sync::ProjectDownloadState::Failed });

                  return;
               }

               SyncCloudSnapshot(
                  targetProject, projectInfo, snapshotInfo,
                  std::move(callback));
            });
      },
      [this, targetProject, callback]()
      {
         GetUI().OnDownloadFinished();
         callback(
            { targetProject,
              {},
              audacity::ToUTF8(XO("Authorization required").Translation()),
              sync::ProjectDownloadState::NotAuthorized });
      });
}

bool CloudSyncService::DoCloudSave(
   AudacityProject& project, const std::string& title)
{
   auto& cloudExtension = sync::ProjectCloudExtension::Get(project);
   cloudExtension.MarkPendingCloudSave();

   const auto dir = CloudProjectsSavePath.Read();
   FileNames::MkDir(dir);

   project.SetProjectName(audacity::ToWXString(title));

   const wxString filePath =
      sync::MakeSafeProjectPath(dir, audacity::ToWXString(title));

   return ProjectFileIO::Get(project).SaveProject(filePath, nullptr);
}

void CloudSyncService::OnOpen(AudacityProject& project, const std::string& path)
{
   auto& cloudExtension = sync::ProjectCloudExtension::Get(project);

   if (cloudExtension.GetAutoDownloadSuppressed())
      return;

   auto& cloudDatabase = sync::CloudProjectsDatabase::Get();

   auto projectInfo = cloudDatabase.GetProjectDataForPath(path);

   if (!projectInfo)
      return;

   GetUI().OnDownloadStarted();

   bool syncFinished = false;

   PerformAuthorised(
      GetOAuthService(), GetUI(), &project,
      [this, projectInfo = std::move(*projectInfo), &project, &syncFinished,
       path]
      {
         GetProjectInfo(
            GetOAuthService(), GetServiceConfig(), projectInfo.ProjectId,
            [&syncFinished, projectInfo, &project, path, this](
               sync::ProjectInfo remoteInfo, std::string errorMessage,
               bool successful)
            {
               syncFinished = true;

               if (!successful)
               {
                  GetUI().ShowDownloadError(errorMessage);
                  return;
               }

               auto& projectFileIO = ProjectFileIO::Get(project);

               // if (remoteInfo.HeadSnapshot.Id == projectInfo.SnapshotId)
               //    return;

               if (HasAutosave(path))
               {
                  if (
                     GetUI().OnDownloadConflict(*ProjectFramePlacement(&project)) ==
                     sync::DownloadConflictResolution::Local)
                     return;
               }

               syncFinished = false;

               GetSnapshotInfo(
                  GetOAuthService(), GetServiceConfig(), remoteInfo.Id,
                  remoteInfo.HeadSnapshot.Id,
                  [this, &project, remoteInfo, &syncFinished](sync::SnapshotInfo snapshotInfo,
                     std::string errorMessage, bool successful)
                  {
                     if (!successful)
                     {
                        syncFinished = true;

                        GetUI().ShowDownloadError(errorMessage);
                        return;
                     }

                     auto& cloudExtension =
                        sync::ProjectCloudExtension::Get(project);

                     cloudExtension.SuppressAutoDownload();

                     SyncCloudSnapshot(
                        &project, remoteInfo, snapshotInfo,
                        [this, &project, &cloudExtension,
                         &syncFinished](sync::ProjectDownloadResult result)
                        {
                           syncFinished = true;

                           if (
                              result.SyncState !=
                              sync::ProjectDownloadState::Succeeded)
                           {
                              GetUI().ShowDownloadError(result.ErrorMessage);
                              return;
                           }
                        });
                  });
            });
      },
      [&syncFinished, this]
      {
         syncFinished = true;
         GetUI().ShowDownloadError({});
      });

   while (!syncFinished)
      BasicUI::Yield();

   GetUI().OnDownloadFinished();
}

void CloudSyncService::OnLoad(AudacityProject& project)
{
   sync::ProjectCloudExtension::Get(project).OnLoad();
}

bool CloudSyncService::OnSave(AudacityProject& project, bool fromTempProject)
{
   if (!fromTempProject)
      return false;

   if (!UI::Get())
      return false;

   auto& ui = UI::Get()();

   auto placement = ProjectFramePlacement(&project);
   const auto result = ui.OnHandleFirstSave(project, *placement);

   if (!result.SaveToCloud)
      return false;

   return DoCloudSave(project, result.Title);
}

bool CloudSyncService::OnClose(AudacityProject& project)
{
   return true;
}

bool CloudSyncService::IsBlockLocked(
   const AudacityProject& project, int64_t blockId) const
{
   return false;
}

void CloudSyncService::OnUpdateSaved(
   AudacityProject& project, const ProjectSerializer& serializer)
{
   auto& cloudExtension = sync::ProjectCloudExtension::Get(project);

   if (!cloudExtension.OnUpdateSaved(serializer))
      return;

   GetOAuthService().ValidateAuth(
      [this, weakProject = cloudExtension.GetProject()](auto token)
      {
         if (token.empty())
         {
            BasicUI::CallAfter(
               [this, weakProject]
               {
                  auto project = weakProject.lock();
                  if (!project)
                     return;

                  if (GetUI().OnAuthorizationRequired(
                         *ProjectFramePlacement(project.get())))
                  {
                     CreateSnapshot(*project);
                  }
               });
            return;
         }

         auto project = weakProject.lock();
         if (!project)
            return;

         CreateSnapshot(*project);
      },
      true);
}

void CloudSyncService::CreateSnapshot(AudacityProject& project)
{
   auto& cloudExtension = sync::ProjectCloudExtension::Get(project);
   mLocalSnapshots.emplace_back(sync::LocalProjectSnapshot::Create(
      GetUI(), GetServiceConfig(), GetOAuthService(), cloudExtension,
      [this](const auto& update)
      {
         UpdateProgress();

         if (update.Completed)
         {
            BasicUI::CallAfter(
               [this, update]
               {
                  if (update.Successful)
                     GetUI().OnUploadSucceeded(
                        update.Snapshot->GetProject().get());
                  else
                     GetUI().OnUploadFailed(
                        update.Snapshot->GetProject().get(),
                        update.ErrorMessage);

                  mLocalSnapshots.erase(
                     std::remove_if(
                        mLocalSnapshots.begin(), mLocalSnapshots.end(),
                        [this,
                         currentSnapshot = update.Snapshot](auto& snapshot)
                        { return snapshot.get() == currentSnapshot; }),
                     mLocalSnapshots.end());
               });
         }
      }));
}

void CloudSyncService::SyncCloudSnapshot(
   AudacityProject* targetProject, const sync::ProjectInfo& projectInfo,
   const sync::SnapshotInfo& snapshotInfo,
   sync::ProjectDownloadedCallback callback)
{
   // Get the project location
   auto localProjectInfo =
      sync::CloudProjectsDatabase::Get().GetProjectData(projectInfo.Id);

   const auto createNew = !localProjectInfo;

   const auto path = createNew ? sync::MakeSafeProjectPath(
                                    CloudProjectsSavePath.Read(),
                                    audacity::ToWXString(projectInfo.Name)) :
                                 localProjectInfo->LocalPath;

   const auto fileExists = wxFileExists(path);

   if (!fileExists)
   {
      const auto dir = CloudProjectsSavePath.Read();
      FileNames::MkDir(dir);

      InvisibleTemporaryProject project;
      ProjectFileIO::Get(project.Project()).LoadProject(path, true);
   }

   mRemoteSnapshot = sync::RemoteProjectSnapshot::Sync(
      projectInfo, snapshotInfo, audacity::ToUTF8(path),
      [this, createNew, path = audacity::ToUTF8(path), targetProject,
       projectId = projectInfo.Id, snapshotId = snapshotInfo.Id,
       callback = std::move(callback)](sync::RemoteProjectSnapshotState state)
      {
         UpdateProgress(
            (state.ProjectDownloaded + state.BlocksDownloaded) /
            (state.BlocksTotal + 1.0));

         if (state.Complete)
         {
            BasicUI::CallAfter(
               [this, createNew, path, state, targetProject,
                projectId = std::move(projectId),
                snapshotId = std::move(snapshotId),
                callback = std::move(callback)]
               {
                  GetUI().OnDownloadFinished();

                  callback(
                     { targetProject, path, state.Error,
                       state.Success   ? sync::ProjectDownloadState::Succeeded :
                       state.Cancelled ? sync::ProjectDownloadState::Cancelled :
                                         sync::ProjectDownloadState::Failed });

                  mRemoteSnapshot.reset();
               });
         }
      });
}

sync::CloudSyncUI& CloudSyncService::GetUI() const
{
   auto& factory = UI::Get();

   if (!factory)
   {
      static DefaultCloudSyncUI ui;
      return ui;
   }

   return factory();
}

void CloudSyncService::UpdateProgress(double downloadProgress)
{
   auto lock = std::lock_guard { mProgressMutex };

   if (downloadProgress >= 0.0)
      mDownloadProgress = downloadProgress;

   if (mProgressUpdateQueued)
      return;

   mProgressUpdateQueued = true;

   BasicUI::CallAfter(
      [this]
      {
         auto lock = std::lock_guard { mProgressMutex };

         for (auto& snapshot : mLocalSnapshots)
         {
            if (snapshot->IsCompleted())
               continue;

            auto project = snapshot->GetProject();

            if (!project)
               continue;

            if (!GetUI().OnUploadProgress(
                   project.get(), snapshot->GetSyncProgress()))
               snapshot->Cancel();
         }

         if (mRemoteSnapshot && !GetUI().OnDownloadProgress(mDownloadProgress))
            mRemoteSnapshot->Cancel();

         mProgressUpdateQueued = false;
      });
}

namespace
{
ProjectFileIOExtensionRegistry::Extension extension { CloudSyncService::Get() };
}
} // namespace cloud::audiocom
