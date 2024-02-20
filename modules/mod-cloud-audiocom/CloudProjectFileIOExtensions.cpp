/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectFileIOExtensions.h

  Dmitry Vedenko

**********************************************************************/

#include "AuthorizationHandler.h"
#include "CloudSettings.h"
#include "CloudProjectUtils.h"

#include "OAuthService.h"
#include "ServiceConfig.h"
#include "UserService.h"

#include "ui/ProjectCloudUIExtension.h"
#include "ui/dialogs/CloudProjectPropertiesDialog.h"
#include "ui/dialogs/LinkFailedDialog.h"
#include "ui/dialogs/MixdownPropertiesDialog.h"
#include "ui/dialogs/SelectSaveLocationDialog.h"

#include "sync/CloudSyncUtils.h"
#include "sync/LocalProjectSnapshot.h"
#include "sync/ProjectCloudExtension.h"

#include "BasicUI.h"
#include "CodeConversions.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "ProjectFileIOExtension.h"
#include "ProjectWindow.h"

namespace
{
using namespace cloud::audiocom;
using namespace cloud::audiocom::sync;

class IOExtension final : public ProjectFileIOExtension
{
   bool OnOpen(AudacityProject& project, const std::string& path) override
   {
      return SyncCloudProject(project, path);
   }

   void OnLoad(AudacityProject& project) override
   {
      ProjectCloudExtension::Get(project).OnLoad();
   }

   bool OnSave(
      AudacityProject& project,
      const ProjectSaveCallback& projectSaveCallback) override
   {
      auto& projectCloudExtension = ProjectCloudExtension::Get(project);
      auto& projectFileIO         = ProjectFileIO::Get(project);

      if (
         !projectFileIO.IsTemporary() &&
         !projectCloudExtension.IsPendingCloudSave())
      {
         if (projectCloudExtension.IsCloudProject())
         {
            auto authResult = PerformBlockingAuth(&project);

            if (authResult.Result == AuthResult::Status::Failure)
            {
               LinkFailedDialog dialog { &ProjectWindow::Get(project) };
               dialog.ShowModal();
               // Pretend we have canceled the save
               return true;
            }
            else if (authResult.Result == AuthResult::Status::Cancelled)
            {
               return true;
            }
            else if (authResult.Result == AuthResult::Status::UseAlternative)
            {
               return false;
            }

            projectCloudExtension.OnSyncStarted();
         }

         return false;
      }

      auto parent = &ProjectWindow::Get(project);

      if (
         projectFileIO.IsTemporary() &&
         !projectCloudExtension.IsPendingCloudSave())
      {
         SelectSaveLocationDialog selectSaveLocationDialog { parent };
         const auto saveLocation = selectSaveLocationDialog.ShowModal();

         // Not doing a cloud save
         if (saveLocation != wxID_SAVE)
            return false;
      }

      auto result = CloudProjectPropertiesDialog::Show(
         GetServiceConfig(), GetOAuthService(), GetUserService(),
         project.GetProjectName(), parent, false);

      if (result.first == CloudProjectPropertiesDialog::Action::Cancel)
         // Suppress the Save function completely
         return true;
      else if (
         result.first == CloudProjectPropertiesDialog::Action::SaveLocally)
         // Just let the things flow as usual
         return false;

      // Adjust the mix down generation rate (if needed)
      MixdownPropertiesDialog::ShowIfNeeded(parent);

      const auto dir = CloudProjectsSavePath.Read();
      FileNames::MkDir(dir);

      const auto filePath = sync::MakeSafeProjectPath(dir, result.second);

      // Notify the extension that Sync has started
      projectCloudExtension.OnSyncStarted();

      if (!projectSaveCallback(audacity::ToUTF8(filePath), true))
         projectCloudExtension.OnSyncCompleted(
            nullptr, MakeClientFailure(XO("Failed to save the project")));

      return true;
   }

   bool OnClose(AudacityProject& project) override
   {
      auto& projectCloudExtension = ProjectCloudExtension::Get(project);

      if (!projectCloudExtension.IsCloudProject())
         return true;

      auto& projectCloudUIExtension = ProjectCloudUIExtension::Get(project);

      return projectCloudUIExtension.AllowClosing();
   }

   void OnUpdateSaved(
      AudacityProject& project, const ProjectSerializer& serializer) override
   {
      auto& projectCloudExtension = ProjectCloudExtension::Get(project);

      if (!projectCloudExtension.OnUpdateSaved(serializer))
         return;

      auto snapshot = LocalProjectSnapshot::Create(
         GetServiceConfig(), GetOAuthService(), projectCloudExtension);

      if (snapshot != nullptr)
         // This operation is blocking, but that is expected
         // and it Yields appropriately 
         UploadMixdownForSnapshot(project, snapshot);
   }

   bool
   IsBlockLocked(const AudacityProject& project, int64_t blockId) const override
   {
      return false;
   }
};

IOExtension& GetExtension()
{
   static IOExtension extension;
   return extension;
}

ProjectFileIOExtensionRegistry::Extension extension { GetExtension() };
} // namespace
