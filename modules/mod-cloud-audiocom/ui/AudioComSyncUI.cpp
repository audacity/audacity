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

   FirstSaveResult OnHandleFirstSave(
      const AudacityProject& project,
      const BasicUI::WindowPlacement& placement) override
   {
      const auto suppressSelectDialog = DoNotShowCloudSyncDialog.Read();

      if (suppressSelectDialog)
      {
         FirstSaveResult result;
         result.SaveToCloud = SaveToCloudByDefault.Read();
         result.PreviewSaveFrequency = MixdownGenerationFrequency.Read();
         return result;
      }

      auto parent = wxWidgetsWindowPlacement::GetParent(placement);

      SelectSaveLocationDialog selectSaveLocationDialog { parent };
      const auto saveLocation = selectSaveLocationDialog.ShowModal();

      if (saveLocation == wxID_CANCEL)
      {
         FirstSaveResult result;
         result.Cancelled = true;
         return result;
      }
      else if (saveLocation == wxID_OK)
      {
         FirstSaveResult result;
         result.SaveToCloud = false;
         return result;
      }

      auto result = CloudProjectPropertiesDialog::Show(
         GetServiceConfig(), GetOAuthService(), GetUserService(),
         project.GetProjectName(), parent);

      if (!MixdownDialogShown.Read())
      {
         result.PreviewSaveFrequency = MixdownPropertiesDialog::Show(parent);
         MixdownDialogShown.Write(true);
      }

      return result;
   }

   bool OnUnauthorizedSave(const BasicUI::WindowPlacement& placement) override
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
