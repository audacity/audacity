/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncService.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "ProjectFileIOExtension.h"

#include <memory>
#include <vector>

namespace cloud::audiocom
{
namespace sync
{
class CloudProjectSnapshot;
}

class CloudSyncService final : public ProjectFileIOExtension
{
public:
   static CloudSyncService& Get();

private:
   void OnLoad(AudacityProject& project) override;
   void OnSave(AudacityProject& project) override;
   bool OnClose(AudacityProject& project) override;

   std::vector<std::unique_ptr<sync::CloudProjectSnapshot>> mSnapshots;
};
} // namespace cloud::audiocom
