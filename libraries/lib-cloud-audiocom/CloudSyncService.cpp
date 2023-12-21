/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncService.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudSyncService.h"

#include <algorithm>
#include <chrono>

#include "CloudSettings.h"

#include "sync/CloudProjectSnapshot.h"
#include "sync/ProjectCloudExtension.h"
#include "sync/CloudSyncUI.h"
#include "sync/CloudSyncUtils.h"

#include "CodeConversions.h"

#include "ServiceConfig.h"
#include "OAuthService.h"

#include "Project.h"
#include "ProjectFileIO.h"

#include "BasicUI.h"
#include "FileNames.h"

#include <wx/log.h>
#include "wxFileNameWrapper.h"

#include "NetworkManager.h"
#include "Request.h"
#include "IResponse.h"


namespace cloud::audiocom
{
CloudSyncService& CloudSyncService::Get()
{
   static CloudSyncService service;
   return service;
}

void CloudSyncService::GetProjects(
   int page, int pageSize, GetProjectsCallback callback)
{
   using namespace audacity::network_manager;

   if (!callback)
      callback = [](auto...) {};

   GetOAuthService().ValidateAuth(
      [this, page, pageSize, callback = std::move(callback)](auto token)
      {
         if (token.empty())
         {
            BasicUI::CallAfter(
               [this, page, pageSize, callback = std::move(callback)]
               {
                  if (UI::Get() && UI::Get()().OnAuthorizationRequired({}))
                     GetProjects(page, pageSize, std::move(callback));
                  else
                  {
                     callback(
                        {},
                        audacity::ToUTF8(
                           XO("Authorization required").Translation()),
                        false);
                  }
               });
            return;
         }

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
                  callback (
                        {},
                     audacity::ToUTF8 (
                           XO("Failed to deserialize projects response")
                              .Translation()),
                        false);
                  return;
               }

               callback(std::move(*projects), {}, true);
            });
      },
      true);
}

void CloudSyncService::SaveToCloud(AudacityProject& project)
{
   auto& cloudExtension = sync::ProjectCloudExtension::Get(project);

   if (cloudExtension.IsCloudProject())
   {
      ProjectFileIO::Get(project).UpdateSaved(nullptr);
      return;
   }

   if (!UI::Get())
      return;

   auto& ui = UI::Get()();

   auto placement = ProjectFramePlacement(&project);
   const auto result = ui.OnHandleSave(project, *placement);

   if (!result.SaveToCloud)
      return;

   DoCloudSave(project, result.Title);
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

                  auto placement = ProjectFramePlacement(project.get());

                  if (
                     UI::Get() && UI::Get()().OnAuthorizationRequired(*placement))
                     CreateSnapshot(*project);
               });
            return;
         }

         auto project = weakProject.lock();
         if (!project)
            return;

         CreateSnapshot(*project);
      }, true);
}

void CloudSyncService::CreateSnapshot(AudacityProject& project)
{
   auto& cloudExtension = sync::ProjectCloudExtension::Get(project);
   mSnapshots.emplace_back(sync::CloudProjectSnapshot::Create(
      GetServiceConfig(), GetOAuthService(), cloudExtension,
      [this](const auto& update)
      {
         wxLogDebug(
            "Update: %lld/%lld\n\tproject uploaded: %d\n\tcompleted: %d\n\tsuccess: %d\n\t%s",
            update.SampleBlocksProcessed, update.SampleBlocksCount,
            update.ProjectBlobProcessed, update.Completed, update.Successful,
            update.ErrorMessage);

         if (update.Completed)
         {
            BasicUI::CallAfter(
               [this]
               {
                  mSnapshots.erase(
                     std::remove_if(
                        mSnapshots.begin(), mSnapshots.end(),
                        [this](auto& snapshot)
                        { return snapshot->IsCompleted(); }),
                     mSnapshots.end());
               });
         }
      }));
}

namespace
{
ProjectFileIOExtensionRegistry::Extension extension { CloudSyncService::Get() };
}
} // namespace cloud::audiocom
