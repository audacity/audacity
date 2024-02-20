/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudProjectUtils.h"
#include "CommandContext.h"
#include "MenuRegistry.h"
#include "sync/ProjectCloudExtension.h"

#include "ProjectWindow.h"

#include "ui/dialogs/ProjectsListDialog.h"

namespace
{
using namespace cloud::audiocom::sync;

void OnSaveToCloud(const CommandContext& context)
{
   SaveToCloud(context.project, UploadMode::Normal);
}

void OnOpenFromCloud(const CommandContext& context)
{
   ProjectsListDialog dialog { ProjectWindow::Find(&context.project),
                               &context.project };

   dialog.ShowModal();
}

void OnUpdateMixdown(const CommandContext& context)
{
   ProjectCloudExtension::Get(context.project).MarkNeedsMixdownSync();
   SaveToCloud(context.project, UploadMode::Normal);
}
const ReservedCommandFlag& IsCloudProjectFlag()
{
   static ReservedCommandFlag flag {
      [](const AudacityProject& project)
      { return ProjectCloudExtension::Get(project).IsCloudProject(); },
      CommandFlagOptions { [](const TranslatableString&) {
         return XO("Previews can be updated only for cloud projects");
      } }.QuickTest()
         .Priority(1)
   };
   return flag;
}

using namespace MenuRegistry;

AttachedItem sSaveAttachment { Command(
                                  wxT("SaveToCloud"), XXO("Save to cloud..."),
                                  OnSaveToCloud, AlwaysEnabledFlag),
                               wxT("File/Save") };

AttachedItem sMixdownAttachment { Command(
                                     wxT("UpdateMixdown"),
                                     XXO("Update Cloud Audio Preview"),
                                     OnUpdateMixdown, IsCloudProjectFlag()),
                                  wxT("File/Save") };

AttachedItem sOpenAttachment { Command(
                                  wxT("OpenFromCloud"),
                                  XXO("Open From Cloud..."), OnOpenFromCloud,
                                  AlwaysEnabledFlag),
                               wxT("File/Basic") };

} // namespace
