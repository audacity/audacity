/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dmitry Vedenko

**********************************************************************/

#include "MenuRegistry.h"
#include "CloudSyncService.h"
#include "CommandContext.h"

namespace
{

void OnSaveToCloud(const CommandContext& context)
{
   cloud::audiocom::CloudSyncService::Get().SaveToCloud(context.project);
}

using namespace MenuRegistry;

AttachedItem sAttachment { Command(
                              wxT("SaveToCloud"), XXO("Save to Cloud"),
                              OnSaveToCloud, AlwaysEnabledFlag),
                           wxT("File/Save") };
}

