/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CheckoutRebaseOperation.cpp

**********************************************************************/
#include "CheckoutRebaseOperation.h"

#include <chrono>
#include <cstdio>
#include <functional>
#include <future>
#include <map>
#include <set>
#include <thread>

#include "../ServiceConfig.h"
#include "CheckoutRebase.h"
#include "CloudSyncDTO.h"
#include "DataUploader.h"
#include "NetworkUtils.h"
#include "ProjectCloudExtension.h"
#include "ProjectDocument.h"
#include "WavPackCompressor.h"

#include "au3-basic-ui/BasicUI.h"
#include "au3-crypto/crypto/SHA256.h"
#include "au3-network-manager/IResponse.h"
#include "au3-network-manager/NetworkManager.h"
#include "au3-network-manager/Request.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-project-file-io/ProjectSerializer.h"
#include "au3-project/Project.h"
#include "au3-string-utils/StringUtils.h"
#include "au3-track/Track.h"
#include "au3-wave-track/SampleBlock.h"
#include "au3-wave-track/Sequence.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"

namespace audacity::cloud::audiocom::sync {
namespace {
using namespace audacity::network_manager;

struct Failure final
{
    std::string reason;
};

struct Response final
{
    int httpCode {};
    std::string body;
    bool networkError {};
};

//! Blocking request, for use off the main thread: callbacks run on the network thread
Response Send(const std::string& method, const std::string& url, const std::string& body = {}, bool authorize = true)
{
    Request request(url);
    if (authorize) {
        SetCommonHeaders(request);
    }
    if (!body.empty()) {
        request.setHeader(common_headers::ContentType, common_content_types::ApplicationJson);
    }

    auto& manager = NetworkManager::GetInstance();
    const auto response = method == "GET" ? manager.doGet(request)
                          : manager.doPost(request, body.data(), body.size());

    std::promise<Response> promise;
    response->setRequestFinishedCallback([&promise, response](auto) {
        promise.set_value({ static_cast<int>(response->getHTTPCode()), response->readAll<std::string>(),
                            response->getError() != NetworkError::NoError });
    });
    return promise.get_future().get();
}

//! Any 2xx: the server answers some requests with 201 or 204, not 200
bool Succeeded(const Response& response)
{
    return !response.networkError && response.httpCode >= 200 && response.httpCode < 300;
}

std::string Describe(const Response& response)
{
    return "HTTP " + std::to_string(response.httpCode) + (response.body.empty() ? std::string() : ": " + response.body);
}

std::vector<uint8_t> Download(const std::string& url)
{
    const auto response = Send("GET", url, {}, false);
    if (!Succeeded(response)) {
        throw Failure { "download failed (" + Describe(response) + "): " + url };
    }
    return { response.body.begin(), response.body.end() };
}

SnapshotInfo GetSnapshot(const ServiceConfig& config, const std::string& projectId, const std::string& snapshotId)
{
    const auto response = Send("GET", config.GetSnapshotInfoUrl(projectId, snapshotId));
    auto info = Succeeded(response) ? DeserializeSnapshotInfo(response.body) : std::nullopt;
    if (!info) {
        throw Failure { "can't get snapshot " + snapshotId };
    }
    return *info;
}

//! The head once it's complete: a head still being uploaded can't be built upon
SnapshotInfo WaitForSyncedHead(const ServiceConfig& config, const std::string& projectId)
{
    for (int attempt = 0; attempt < 120; ++attempt) {
        const auto response = Send("GET", config.GetProjectInfoUrl(projectId));
        const auto info = Succeeded(response) ? DeserializeProjectInfo(response.body) : std::nullopt;
        if (!info) {
            throw Failure { "can't get the project's state" };
        }
        if (info->HeadSnapshot.Synced > 0) {
            return info->HeadSnapshot;
        }
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }
    throw Failure { "the project's latest version didn't finish uploading" };
}

void Upload(const ServiceConfig& config, const UploadUrls& urls, std::vector<uint8_t> data)
{
    std::promise<ResponseResult> promise;
    DataUploader::Get().Upload(concurrency::CancellationContext::Create(), config, urls, std::move(data),
                               [&promise](ResponseResult result) { promise.set_value(std::move(result)); });
    const auto result = promise.get_future().get();
    if (result.Code != SyncResultCode::Success) {
        throw Failure { "upload failed: " + result.Content };
    }
}

//! Hash as computed by BlockHasher: SHA-256 of the samples, prefixed with the block id
std::string HashOf(SampleBlock& block, long long blockId)
{
    const auto format = block.GetSampleFormat();
    const auto count = block.GetSampleCount();
    std::vector<uint8_t> samples(count * SAMPLE_SIZE(format));
    if (block.GetSamples(reinterpret_cast<samplePtr>(samples.data()), format, 0, count, false) != count) {
        throw Failure { "can't read the samples of block " + std::to_string(blockId) };
    }
    auto hash = crypto::sha256(samples);
    char prefix[17];
    std::snprintf(prefix, sizeof(prefix), "%08llX", blockId);
    hash.replace(0, 8, prefix);
    return hash;
}

void CollectBlockIds(const DocumentElement& element, std::set<long long>& ids)
{
    if (element.name == "waveblock") {
        if (const auto id = element.IntAttribute("blockid"); id && *id > 0) {
            ids.insert(*id);
        }
    }
    for (const auto& child : element.children) {
        CollectBlockIds(child, ids);
    }
}

std::set<long long> EditLockedBlockIds(AudacityProject& project)
{
    std::set<long long> ids;
    for (const auto* track : TrackList::Get(project).Any<const WaveTrack>()) {
        for (const auto& clip : track->Intervals()) {
            for (size_t ch = 0; ch < clip->NChannels(); ++ch) {
                for (const auto& block : clip->GetSequence(ch)->GetBlockArray()) {
                    if (block.sb->IsEditLocked()) {
                        ids.insert(block.sb->GetBlockID());
                    }
                }
            }
        }
    }
    return ids;
}

//! Throws if a block replaced from `before` to `after` isn't allowed
void CheckReplacements(const DocumentElement& before, const DocumentElement& after,
                       const std::function<bool(long long)>& isAllowed, const std::string& reason)
{
    for (const auto& replacement : ComputeReplacements(before, after)) {
        for (const auto id : replacement.removedBlockIds) {
            if (id > 0 && !isAllowed(id)) {
                throw Failure { reason };
            }
        }
    }
}

void Rebase(const std::string& projectId, const std::string& baseSnapshotId, const DocumentElement& local,
            const std::map<long long, std::shared_ptr<SampleBlock> >& localBlocks, const std::set<long long>& lockedIds)
{
    const auto& config = GetServiceConfig();

    const auto head = WaitForSyncedHead(config, projectId);
    if (head.Id == baseSnapshotId) {
        throw Failure { "nothing to rebase onto: the checkout is up to date" };
    }

    const auto headInfo = GetSnapshot(config, projectId, head.Id);
    const auto baseInfo = GetSnapshot(config, projectId, baseSnapshotId);
    const auto headDocument = DecodeProjectBlob(Download(headInfo.FileUrl));
    const auto baseDocument = DecodeProjectBlob(Download(baseInfo.FileUrl));
    if (!headDocument || !baseDocument) {
        throw Failure { "can't read the project's versions" };
    }

    // Checked here rather than left to the main instance, which would reject everything
    CheckReplacements(*baseDocument, local, [&](long long id) { return !lockedIds.count(id); },
                      "this checkout changed audio outside the selection it was opened with");

    std::variant<RebaseResult, RebaseConflict> rebased = RebaseCheckout(*baseDocument, local, *headDocument);
    if (const auto conflict = std::get_if<RebaseConflict>(&rebased)) {
        throw Failure { "conflict: " + conflict->reason };
    }
    const auto& result = std::get<RebaseResult>(rebased);

    // Hashes start with the block id, so the head's blocks are found by id
    std::map<long long, std::string> hashes;
    for (const auto& block : headInfo.Blocks) {
        hashes[std::stoll(block.Hash.substr(0, 8), nullptr, 16)] = block.Hash;
    }
    struct NewBlock {
        std::shared_ptr<SampleBlock> block;
        long long id;
    };
    std::map<std::string, NewBlock> newBlocks;
    for (const auto& [localId, newId] : result.newBlocks) {
        const auto it = localBlocks.find(localId);
        if (it == localBlocks.end()) {
            throw Failure { "block " + std::to_string(localId) + " isn't in the checkout" };
        }
        const auto hash = HashOf(*it->second, newId);
        hashes[newId] = hash;
        // Hashes are compared ignoring case, as the server may change it
        newBlocks[ToUpper(hash)] = { it->second, newId };
    }

    std::set<long long> ids;
    CollectBlockIds(result.document, ids);
    ProjectForm form;
    form.HeadSnapshotId = head.Id;
    for (const auto id : ids) {
        const auto it = hashes.find(id);
        if (it == hashes.end()) {
            throw Failure { "no hash for block " + std::to_string(id) };
        }
        form.Hashes.push_back(it->second);
    }

    const auto created = Send("POST", config.GetCreateSnapshotUrl(projectId), Serialize(form));
    if (created.httpCode == 409 || created.httpCode == 422) {
        throw Failure { "the project changed again on the server, rebase again" };
    }
    const auto snapshot = Succeeded(created) ? DeserializeCreateSnapshotResponse(created.body) : std::nullopt;
    if (!snapshot) {
        throw Failure { "can't create the snapshot (" + Describe(created) + ")" };
    }

    Upload(config, snapshot->SyncState.FileUrls, EncodeProjectBlob(result.document));
    for (const auto& urls : snapshot->SyncState.MissingBlocks) {
        const auto it = newBlocks.find(ToUpper(urls.Id));
        if (it == newBlocks.end()) {
            throw Failure { "the server asks for block " + urls.Id + ", which isn't one of this checkout's new blocks" };
        }
        const auto& [block, id] = it->second;
        Upload(config, urls, CompressBlock({ id, block->GetSampleFormat(), block, urls.Id }));
    }

    const auto synced = Send("POST", config.GetSnapshotSyncUrl(projectId, snapshot->Snapshot.Id));
    if (!Succeeded(synced)) {
        throw Failure { "can't finalize the snapshot (" + Describe(synced) + ")" };
    }
}
} // namespace

void RebaseOntoHead(AudacityProject& project, std::function<void(std::string error)> onDone)
{
    auto& cloudExtension = ProjectCloudExtension::Get(project);
    const auto projectId = cloudExtension.GetCloudProjectId();
    const auto baseSnapshotId = cloudExtension.GetSnapshotId();
    if (projectId.empty() || baseSnapshotId.empty()) {
        onDone("not a cloud project");
        return;
    }

    // What needs the project is captured here, on the main thread
    ProjectSerializer serializer;
    ProjectFileIO::Get(project).WriteXML(serializer);
    std::optional<DocumentElement> local = DecodeProjectBlob(PackProjectBlob(serializer));
    if (!local) {
        onDone("can't read the checkout's project");
        return;
    }
    std::map<long long, std::shared_ptr<SampleBlock> > localBlocks;
    for (const auto* track : TrackList::Get(project).Any<const WaveTrack>()) {
        for (const auto& clip : track->Intervals()) {
            for (size_t ch = 0; ch < clip->NChannels(); ++ch) {
                for (const auto& block : clip->GetSequence(ch)->GetBlockArray()) {
                    localBlocks.emplace(block.sb->GetBlockID(), block.sb);
                }
            }
        }
    }

    std::thread([projectId, baseSnapshotId, local = std::move(*local), localBlocks = std::move(localBlocks),
                 lockedIds = EditLockedBlockIds(project), onDone = std::move(onDone)]() {
        std::string error;
        try {
            Rebase(projectId, baseSnapshotId, local, localBlocks, lockedIds);
        } catch (const Failure& failure) {
            error = failure.reason;
        } catch (const std::exception& e) {
            error = e.what();
        }
        BasicUI::CallAfter([onDone, error] { onDone(error); });
    }).detach();
}
} // namespace audacity::cloud::audiocom::sync
