/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectSnapshot.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>
#include <functional>
#include <future>
#include <memory>
#include <mutex>
#include <string_view>

#include "ProjectUploadOperation.h"
#include "CloudSyncError.h"
#include "CloudSyncDTO.h"
#include "NetworkUtils.h"

#include "concurrency/CancellationContext.h"

class AudacityProject;
enum class AudiocomTrace;

namespace audacity::cloud::audiocom {
class OAuthService;
class ServiceConfig;
} // namespace audacity::cloud::audiocom

namespace audacity::cloud::audiocom::sync {
constexpr auto UNASSIGNED_PROJECT_ID = -1;

class ProjectCloudExtension;
class LocalProjectSnapshot;
class MixdownUploader;
class CloudSyncUI;
struct MissingBlocksUploadProgress;

class MissingBlocksUploader;

class CLOUD_AUDIOCOM_API LocalProjectSnapshot final : public ProjectUploadOperation,
    public std::enable_shared_from_this<LocalProjectSnapshot>
{
    struct Tag final
    {
    };

public:
    struct CLOUD_AUDIOCOM_API SnapshotData final
    {
        std::optional<CreateSnapshotResponse> Response;
        std::shared_ptr<LocalProjectSnapshot> Operation;
    };

    using Promise = std::promise<SnapshotData>;
    using Future = std::future<SnapshotData>;

    LocalProjectSnapshot(
        Tag, const ServiceConfig& config, const OAuthService& oauthService, ProjectCloudExtension& extension, std::string name,
        UploadMode mode, AudiocomTrace trace);
    ~LocalProjectSnapshot() override;

    static Future Create(
        const ServiceConfig& config, const OAuthService& oauthService, ProjectCloudExtension& extension, std::string name, UploadMode mode,
        AudiocomTrace trace);

    bool IsCompleted() const override;

    std::shared_ptr<AudacityProject> GetProject();

    void Start() override;
    void SetUploadData(const ProjectUploadData& data) override;
    void Cancel() override;

    void Abort();

private:
    void UploadFailed(CloudSyncError error);
    void DataUploadFailed(const ResponseResult& uploadResult);
    void DataUploadFailed(const MissingBlocksUploadProgress& uploadResult);

    void UpdateProjectSnapshot();

    void
    OnSnapshotCreated(const CreateSnapshotResponse& response, bool newProject);
    void StorePendingSnapshot(
        const CreateSnapshotResponse& response, const ProjectUploadData& data);
    void MarkSnapshotSynced();
    void DeleteSnapshot();

    ProjectCloudExtension& mProjectCloudExtension;
    std::weak_ptr<AudacityProject> mWeakProject;

    std::promise<ProjectUploadData> mProjectDataPromise;

    const ServiceConfig& mServiceConfig;
    const OAuthService& mOAuthService;
    const AudiocomTrace mAudiocomTrace;

    std::string mProjectName;

    struct ProjectBlocksLock;
    std::unique_ptr<ProjectBlocksLock> mProjectBlocksLock;

    std::shared_ptr<MissingBlocksUploader> mMissingBlockUploader;

    std::mutex mCreateSnapshotResponseMutex;
    std::optional<CreateSnapshotResponse> mCreateSnapshotResponse;

    UploadMode mUploadMode { UploadMode::Normal };

    Promise mCreateSnapshotPromise;

    concurrency::CancellationContextPtr mCancellationContext;

    std::atomic<bool> mCompleted { false };
    std::atomic<bool> mCancelled { false };
    std::atomic<bool> mProjectDataReady { false };
};
} // namespace audacity::cloud::audiocom::sync
