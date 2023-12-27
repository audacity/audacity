/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComShareUI.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudSyncService.h"
#include "sync/CloudSyncUI.h"

#include "CodeConversions.h"

#include "ServiceConfig.h"
#include "OAuthService.h"
#include "UserService.h"

#include "Project.h"
#include "ProjectManager.h"

#include "SelectSaveLocationDialog.h"
#include "CloudProjectPropertiesDialog.h"
#include "MixdownPropertiesDialog.h"

#include "CloudModuleSettings.h"

#include "wxWidgetsWindowPlacement.h"

namespace cloud::audiocom::sync
{
class AudioComShareUI : public CloudSyncUI
{
public:
   ~AudioComShareUI() override = default;

   SaveResult SaveProject(
      const AudacityProject& project, const BasicUI::WindowPlacement& placement,
      bool allowLocalSave)
   {
      auto parent = wxWidgetsWindowPlacement::GetParent(placement);

      auto result = CloudProjectPropertiesDialog::Show(
         GetServiceConfig(), GetOAuthService(), GetUserService(),
         project.GetProjectName(), parent, allowLocalSave);

      if (!MixdownDialogShown.Read())
      {
         result.PreviewSaveFrequency = MixdownPropertiesDialog::Show(parent);
         MixdownDialogShown.Write(true);
      }

      return result;
   }

   SaveResult OnHandleFirstSave(
      const AudacityProject& project,
      const BasicUI::WindowPlacement& placement) override
   {
      const auto suppressSelectDialog = DoNotShowCloudSyncDialog.Read();

      if (suppressSelectDialog)
      {
         SaveResult result;
         result.SaveToCloud = SaveToCloudByDefault.Read();
         result.PreviewSaveFrequency = MixdownGenerationFrequency.Read();
         return result;
      }

      auto parent = wxWidgetsWindowPlacement::GetParent(placement);

      SelectSaveLocationDialog selectSaveLocationDialog { parent };
      const auto saveLocation = selectSaveLocationDialog.ShowModal();

      if (saveLocation == wxID_CANCEL)
      {
         SaveResult result;
         result.Cancelled = true;
         return result;
      }
      else if (saveLocation == wxID_OK)
      {
         SaveResult result;
         result.SaveToCloud = false;
         return result;
      }

      return SaveProject(project, placement, false);
   }

   SaveResult OnHandleSave(
      const AudacityProject& project,
      const BasicUI::WindowPlacement& placement) override
   {
      return SaveProject(project, placement, false);
   }

   bool
   OnAuthorizationRequired(const BasicUI::WindowPlacement& placement) override
   {
      return false;
   }

   bool OnUploadProgress(AudacityProject* project, double progress) override
   {
      return true;
   }

   void
   OnUploadFailed(AudacityProject* project, std::string errorMessage) override
   {
   }

   void OnUploadSucceeded(AudacityProject* project) override
   {
   }

   void OnDownloadStarted() override
   {
      assert(!mDownloadProgressDialog);

      mDownloadProgressDialog = BasicUI::MakeProgress(
         XO("Opening cloud project"), XO("Loading..."),
         BasicUI::ProgressShowCancel);

      mDownloadProgressDialog->Poll(0, 10000);
   }

   bool OnDownloadProgress(double progress) override
   {
      assert(mDownloadProgressDialog);
      if (!mDownloadProgressDialog)
         return true;

      if (progress < 0.0)
         progress = 0.0;

      return mDownloadProgressDialog->Poll(
                static_cast<unsigned>(progress * 10000), 10000) ==
             BasicUI::ProgressResult::Success;
   }

   void OnDownloadFailed(std::string errorMessage, bool verbose) override
   {
      mDownloadProgressDialog.reset();

      if (verbose)
      {
         BasicUI::ShowErrorDialog(
            {}, XO("Error"), XO("Failed to open cloud project"), {},
            BasicUI::ErrorDialogOptions {}.Log(audacity::ToWString(errorMessage)));
      }
   }

   AudacityProject* OnDownloadSucceeded(
      AudacityProject* targetProject, const std::string& path) override
   {
      mDownloadProgressDialog.reset();

      return ProjectManager::OpenProject(
         targetProject, audacity::ToWXString(path), true, false);
   }

private:
   std::unique_ptr<BasicUI::ProgressDialog> mDownloadProgressDialog;
};

CloudSyncService::UI::Scope scope { []() -> CloudSyncUI&
                                    {
                                       static AudioComShareUI ui;
                                       return ui;
                                    } };
} // namespace cloud::audiocom::sync
