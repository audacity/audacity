/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncUtils.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <optional>
#include <vector>

struct sqlite3;
class AudacityProject;

namespace cloud::audiocom::sync
{
using BlockID = int64_t;
using SampleBlockIDs = std::vector<BlockID>;


SampleBlockIDs GetProjectBlocks(const AudacityProject& project);

std::string SerializeBlockIDs(const SampleBlockIDs& ids);

struct MissingBlock final
{
   BlockID Id;
   std::string UploadUrl;
   std::string ConfirmUrl;
};

struct ProjectResponse final
{
   int64_t ProjectId;
   int64_t Version;
   std::vector<MissingBlock> MissingBlocks;
};

std::optional<ProjectResponse> DeserializeProjectResponse(const std::string& data);

sqlite3* GetProjectDatabaseHandle(AudacityProject& project);

bool CheckTableExists(AudacityProject& project, std::string_view tableName);

std::vector<MissingBlock> GetMissingBlocks(AudacityProject& project);
bool AppendMissingBlocks(AudacityProject& project, const std::vector<MissingBlock>& blocks);
void RemoveMissingBlock(AudacityProject& project, BlockID blockId);

} // namespace cloud::audiocom::sync
