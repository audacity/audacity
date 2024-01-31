/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectUtils.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudProjectUtils.h"

#include "BasicUI.h"
#include "CodeConversions.h"
#include "CloudSyncService.h"
#include "Project.h"
#include "ProjectManager.h"
#include "UriParser.h"

#include "sync/ProjectCloudExtension.h"

namespace cloud::audiocom::sync
{
void OpenProjectFromCloud(
   AudacityProject* potentialTarget, std::string_view projectId,
   std::string_view snapshotId)
{
   CloudSyncService::Get().OpenFromCloud(
      potentialTarget, std::string(projectId), std::string(snapshotId),
      [](auto result)
      {
         switch (result.SyncState)
         {
         case ProjectDownloadState::Failed:
            BasicUI::ShowErrorDialog(
               {}, XO("Error"), XO("Failed to open cloud project"), {},
               BasicUI::ErrorDialogOptions {}.Log(
                  audacity::ToWString(result.ErrorMessage)));
            break;
         case ProjectDownloadState::Succeeded:
            if (result.TargetProject != nullptr)
            {
               ProjectCloudExtension::Get(*result.TargetProject)
                  .SuppressAutoDownload();
            }

            ProjectManager::OpenProject(
               result.TargetProject, audacity::ToWXString(result.ProjectPath),
               true, false);
            break;
         default:
            break;
         }
      });
}

bool HandleProjectLink(std::string_view uri)
{
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

   AudacityProject* targetProject =
      AllProjects {}.empty() ? nullptr : AllProjects {}.begin()->get();

   OpenProjectFromCloud(
      targetProject, projectId->second,
      snapshotId != queryParameters.end() ? snapshotId->second :
                                            std::string_view {});

   return true;
}
} // namespace cloud::audiocom::sync
