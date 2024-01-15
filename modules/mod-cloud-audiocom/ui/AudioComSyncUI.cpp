/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComShareUI.cpp

  Dmitry Vedenko

**********************************************************************/

#include <wx/log.h>

#include "CloudSyncService.h"
#include "sync/CloudSyncUI.h"

#include "CodeConversions.h"

#include "ServiceConfig.h"
#include "OAuthService.h"
#include "UserService.h"

#include "Project.h"

#include "AuthorizationHandler.h"

#include "CloudSyncStatusField.h"
#include "SelectSaveLocationDialog.h"
#include "CloudProjectPropertiesDialog.h"
#include "MixdownPropertiesDialog.h"

#include "CloudModuleSettings.h"

#include "LinkAccountDialog.h"
#include "LinkWithTokenDialog.h"

#include "wxWidgetsWindowPlacement.h"

#ifdef HAS_CUSTOM_URL_HANDLING
#   include "URLSchemesRegistry.h"
#endif

#include "HelpSystem.h"

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
         MixdownPropertiesDialog::Show(parent);
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
      LinkAccountDialog linkDialog { wxWidgetsWindowPlacement::GetParent(
         placement) };
      if (wxID_OK != linkDialog.ShowModal())
         return false;

      OpenInDefaultBrowser(
         { audacity::ToWXString(GetServiceConfig().GetOAuthLoginPage()) });

#ifdef HAS_CUSTOM_URL_HANDLING
      if (URLSchemesRegistry::Get().IsURLHandlingSupported())
      {
         GetAuthorizationHandler().PushSuppressDialogs();

         auto progress = BasicUI::MakeGenericProgress(placement, XO("Link account"), XO("Waiting for authorization..."));

         while (!GetOAuthService ().HasAccessToken ())
         {
            if (progress->Pulse() != BasicUI::ProgressResult::Success)
               return false;
         }

         progress.reset();

         GetAuthorizationHandler().PopSuppressDialogs();

         return true;
      }
      else
#endif
      {
         LinkWithTokenDialog dlg {
            wxWidgetsWindowPlacement::GetParent(placement)
         };
         dlg.ShowModal();
      }

      return false;
   }

   bool OnUploadProgress(AudacityProject* project, double progress) override
   {
      if (project == nullptr)
         return false;

      auto& statusField = CloudSyncStatusField::Get(*project);
      statusField.SetUploadProgress(progress);

      return true;
   }

   void
   OnUploadFailed(AudacityProject* project, std::string errorMessage) override
   {
      if (project == nullptr)
         return;

      auto& statusField = CloudSyncStatusField::Get(*project);
      statusField.UploadCompleted(false);

      wxLogError(
         "Project upload has failed: %s", audacity::ToWXString(errorMessage));
   }

   void OnUploadSucceeded(AudacityProject* project) override
   {
      if (project == nullptr)
         return;

      auto& statusField = CloudSyncStatusField::Get(*project);
      statusField.UploadCompleted(true);
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

   void OnDownloadFinished() override
   {
      mDownloadProgressDialog.reset();
   }

   void ShowDownloadError (std::string errorMessage) override
   {
      const auto platformMessage = audacity::ToWString(errorMessage);

      wxLogError("Project download has failed: %s", platformMessage);

      BasicUI::ShowErrorDialog(
         {}, XO("Error"), XO("Failed to sync cloud project"), {},
         BasicUI::ErrorDialogOptions {}.Log(platformMessage));
   }

   DownloadConflictResolution
   OnDownloadConflict(const BasicUI::WindowPlacement& placement) override
   {
      return DownloadConflictResolution::Remote;
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
