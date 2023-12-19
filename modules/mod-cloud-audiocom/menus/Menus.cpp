/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dmitry Vedenko

**********************************************************************/

#include "MenuRegistry.h"
#include "CloudSyncService.h"
#include "CommandContext.h"

#include "ui/ProjectsListDialog.h"

namespace
{

void OnSaveToCloud(const CommandContext& context)
{
   cloud::audiocom::CloudSyncService::Get().SaveToCloud(context.project);
}

void OnOpenFromCloud (const CommandContext& context)
{
   cloud::audiocom::sync::ProjectsListDialog dialog { nullptr };
   dialog.ShowModal();
}

using namespace MenuRegistry;

AttachedItem sSaveAttachment { Command(
                              wxT("SaveToCloud"), XXO("Save to Cloud"),
                              OnSaveToCloud, AlwaysEnabledFlag),
                           wxT("File/Save") };

AttachedItem sOpenAttachment { Command(
                              wxT("OpenFromCloud"), XXO("Open From Cloud"),
                              OnOpenFromCloud, AlwaysEnabledFlag),
                           wxT("File") };
} // namespace
