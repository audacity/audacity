/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectFileIOExtensions.h

  Dmitry Vedenko

**********************************************************************/

#include "AuthorizationHandler.h"
#include "CloudProjectUtils.h"
#include "CloudSettings.h"

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
using namespace audacity::cloud::audiocom;
using namespace audacity::cloud::audiocom::sync;

class IOExtension final : public ProjectFileIOExtension
{
   std::optional<UploadUrls> LastMixdownUrls;

   OnOpenAction
   OnOpen(AudacityProject& project, const std::string& path) override
   {
      return SyncCloudProject(project, path) ? OnOpenAction::Continue :
                                               OnOpenAction::Cancel;
   }

   void OnLoad(AudacityProject& project) override
   {
      ProjectCloudExtension::Get(project).OnLoad();
   }

   OnSaveAction CreateSnapshot(AudacityProject& project, std::string name)
   {
      LastMixdownUrls = std::nullopt;

      auto& projectCloudExtension = ProjectCloudExtension::Get(project);

      projectCloudExtension.OnSyncStarted();

      auto future = LocalProjectSnapshot::Create(
         GetServiceConfig(), GetOAuthService(), projectCloudExtension, name);

      // Do we need UI here?
      //while (future.wait_for(std::chrono::milliseconds(50)) !=
      //       std::future_status::ready)
      //   BasicUI::Yield();

      auto result = future.get();

      if (!result)
         // Prevent any updates to the file to preserve the correct state.
         // Errors would be handled by the UI extension
         return OnSaveAction::Cancelled;

      LastMixdownUrls = result->SyncState.MixdownUrls;

      return OnSaveAction::Continue;
   }

   OnSaveAction SaveCloudProject (AudacityProject& project)
   {
      auto authResult = PerformBlockingAuth(&project);

      if (authResult.Result == AuthResult::Status::Failure)
      {
         LinkFailedDialog dialog { &ProjectWindow::Get(project) };
         dialog.ShowModal();
         // Pretend we have canceled the save
         return OnSaveAction::Cancelled;
      }
      else if (authResult.Result == AuthResult::Status::Cancelled)
      {
         return OnSaveAction::Cancelled;
      }
      else if (authResult.Result == AuthResult::Status::UseAlternative)
      {
         ProjectFileIO::Get(project).MarkTemporary();
         return OnSaveAction::Continue;
      }

      return CreateSnapshot(
         project, audacity::ToUTF8(project.GetProjectName()));
   }

   OnSaveAction OnSave(
      AudacityProject& project,
      const ProjectSaveCallback& projectSaveCallback) override
   {
      auto& projectCloudExtension = ProjectCloudExtension::Get(project);
      auto& projectFileIO         = ProjectFileIO::Get(project);

      const bool isTemporary      = projectFileIO.IsTemporary();
      const bool pendingCloudSave = projectCloudExtension.IsPendingCloudSave();
      const bool isCloudProject   = projectCloudExtension.IsCloudProject();

      auto parent = &ProjectWindow::Get(project);

      // Check location first
      if (isTemporary && !pendingCloudSave)
      {
         SelectSaveLocationDialog selectSaveLocationDialog { parent };
         const auto saveLocation = selectSaveLocationDialog.ShowModal();

         // Not doing a cloud save
         if (saveLocation != wxID_SAVE)
            return OnSaveAction::Continue;
      }

      // For regular projects - do nothing
      if (!isTemporary && !pendingCloudSave && !isCloudProject)
         return OnSaveAction::Continue;

      if (!isTemporary)
         return SaveCloudProject(project);

      auto result = CloudProjectPropertiesDialog::Show(
         GetServiceConfig(), GetOAuthService(), GetUserService(),
         project.GetProjectName(), parent, false);

      if (result.first == CloudProjectPropertiesDialog::Action::Cancel)
         // Suppress the Save function completely
         return OnSaveAction::Cancelled;
      else if (
         result.first == CloudProjectPropertiesDialog::Action::SaveLocally)
         // Just let the things flow as usual
         return OnSaveAction::Continue;

      // Adjust the mix down generation rate (if needed)
      MixdownPropertiesDialog::ShowIfNeeded(parent);

      if (CreateSnapshot(project, result.second) == OnSaveAction::Cancelled)
         return OnSaveAction::Cancelled;

      const auto dir = CloudProjectsSavePath.Read();
      FileNames::MkDir(dir);

      const auto filePath = sync::MakeSafeProjectPath(dir, result.second);

      if (!projectSaveCallback(audacity::ToUTF8(filePath), true))
         projectCloudExtension.OnSyncCompleted(
            nullptr, MakeClientFailure(XO("Failed to save the project")));

      return OnSaveAction::Handled;
   }

   OnCloseAction OnClose(AudacityProject& project) override
   {
      auto& projectCloudExtension = ProjectCloudExtension::Get(project);

      if (!projectCloudExtension.IsCloudProject())
         return OnCloseAction::Continue;

      auto& projectCloudUIExtension = ProjectCloudUIExtension::Get(project);

      return projectCloudUIExtension.AllowClosing() ? OnCloseAction::Continue :
                                                      OnCloseAction::Veto;
   }

   void OnUpdateSaved(
      AudacityProject& project, const ProjectSerializer& serializer) override
   {
      auto& projectCloudExtension = ProjectCloudExtension::Get(project);
      projectCloudExtension.OnUpdateSaved(serializer);

      if (LastMixdownUrls)
         UploadMixdown(project, *LastMixdownUrls);
   }

   bool
   IsBlockLocked(const AudacityProject& project, int64_t blockId) const override
   {
      return ProjectCloudExtension::Get(project).IsBlockLocked(blockId);
   }
};

IOExtension& GetExtension()
{
   static IOExtension extension;
   return extension;
}

ProjectFileIOExtensionRegistry::Extension extension { GetExtension() };
} // namespace
