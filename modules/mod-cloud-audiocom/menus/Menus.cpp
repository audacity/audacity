/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dmitry Vedenko

**********************************************************************/

#include "MenuRegistry.h"
#include "CommandContext.h"
#include "CloudProjectUtils.h"

#include "ProjectWindow.h"

#include "ui/dialogs/ProjectsListDialog.h"

namespace
{

void OnSaveToCloud(const CommandContext& context)
{
   cloud::audiocom::sync::SaveToCloud(
      context.project, cloud::audiocom::sync::SaveMode::Normal);
}

void OnOpenFromCloud (const CommandContext& context)
{     
   cloud::audiocom::sync::ProjectsListDialog dialog {
      ProjectWindow::Find(&context.project), &context.project
   };

   dialog.ShowModal();
}

using namespace MenuRegistry;

AttachedItem sSaveAttachment { Command(
                              wxT("SaveToCloud"), XXO("Save to cloud..."),
                              OnSaveToCloud, AlwaysEnabledFlag),
                           wxT("File/Save") };

AttachedItem sOpenAttachment { Command(
                              wxT("OpenFromCloud"), XXO("Open From Cloud"),
                              OnOpenFromCloud, AlwaysEnabledFlag),
                           wxT("File") };
} // namespace
