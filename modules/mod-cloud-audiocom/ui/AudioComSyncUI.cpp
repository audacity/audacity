/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComShareUI.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudSyncService.h"
#include "sync/CloudSyncUI.h"

#include "ServiceConfig.h"
#include "OAuthService.h"
#include "UserService.h"

#include "Project.h"

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

   SaveResult SaveProject (
      const AudacityProject& project, const BasicUI::WindowPlacement& placement, bool allowLocalSave)
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
      const AudacityProject& project, const BasicUI::WindowPlacement& placement) override
   {
      return SaveProject(project, placement, false);
   }

   bool OnAuthorizationRequired(const BasicUI::WindowPlacement& placement) override
   {
      return false;
   }
};

CloudSyncService::UI::Scope scope { []() -> CloudSyncUI&
                                    {
                                       static AudioComShareUI ui;
                                       return ui;
                                    } };
} // namespace cloud::audiocom::sync
