/*
* Audacity: A Digital Audio Editor
*/
#include "cloudprojectsprovider.h"

#include "au3-cloud-audiocom/sync/CloudProjectsDatabase.h"
#include "au3-cloud-audiocom/sync/CloudSyncDTO.h"
#include "au3-string-utils/CodeConversions.h"

using namespace au::au3cloud;

namespace {
std::optional<CloudProjectRecord> toCloudProjectRecord(
    const std::optional<audacity::cloud::audiocom::sync::DBProjectData>& data)
{
    if (!data.has_value() || data->ProjectId.empty()) {
        return std::nullopt;
    }

    CloudProjectRecord record;
    record.projectId = data->ProjectId;
    record.snapshotId = data->SnapshotId;
    record.localPath = muse::io::path_t(data->LocalPath);
    return record;
}
}

std::optional<CloudProjectRecord> CloudProjectsProvider::projectRecordForPath(const muse::io::path_t& projectPath) const
{
    namespace sync = audacity::cloud::audiocom::sync;
    return toCloudProjectRecord(sync::CloudProjectsDatabase::Get().GetProjectDataForPath(projectPath.toStdString()));
}

std::optional<CloudProjectRecord> CloudProjectsProvider::projectRecordForId(const std::string& projectId) const
{
    namespace sync = audacity::cloud::audiocom::sync;
    return toCloudProjectRecord(sync::CloudProjectsDatabase::Get().GetProjectData(projectId));
}

muse::io::path_t CloudProjectsProvider::makeSafeFilePath(const muse::io::path_t& rootDir, const std::string& fileName,
                                                         const std::string& fileExtension) const
{
    namespace sync = audacity::cloud::audiocom::sync;
    const wxString path = sync::MakeSafeFilePath(audacity::ToWXString(rootDir.toStdString()), audacity::ToWXString(fileName),
                                                 audacity::ToWXString(fileExtension));
    return muse::io::path_t(audacity::ToUTF8(path));
}
