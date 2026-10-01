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
#include "CloudProjectsDatabase.h"

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
#include "au3-math/SampleFormat.h"

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

struct HeadChanges final
{
    std::string headSnapshotId;
    ProjectPatch patch;
    std::map<long long, DecompressedBlock> blocks;
};


namespace {
long long UidAttribute(const DocumentElement& element)
{
    if (const auto uid = element.IntAttribute("uid")) {
        return *uid;
    }
    return 0;
}

double DoubleAttribute(const DocumentElement& element, std::string_view name, double fallback)
{
    const auto value = element.Attribute(name);
    if (!value) {
        return fallback;
    }
    if (const auto v = std::get_if<double>(value)) {
        return *v;
    }
    if (const auto v = std::get_if<float>(value)) {
        return *v;
    }
    if (const auto v = element.IntAttribute(name)) {
        return static_cast<double>(*v);
    }
    if (const auto v = std::get_if<std::string>(value)) {
        try {
            return std::stod(*v);
        } catch (...) {
        }
    }
    return fallback;
}

//! The samples of a saved channel of a clip, as floats
std::vector<float> ClipChannelSamples(const DocumentElement& clip, const std::map<long long, DecompressedBlock>& blocks)
{
    std::vector<float> samples;
    for (const auto& sequence : clip.children) {
        if (sequence.name != "sequence") {
            continue;
        }
        for (const auto& waveblock : sequence.children) {
            if (waveblock.name != "waveblock") {
                continue;
            }
            const auto id = waveblock.IntAttribute("blockid").value_or(0);
            const auto length = waveblock.IntAttribute("length").value_or(0);
            const auto offset = samples.size();
            samples.resize(offset + length, 0.f);
            const auto it = id > 0 ? blocks.find(id) : blocks.end();
            if (it == blocks.end()) {
                continue; // silent
            }
            const auto& block = it->second;
            const auto count = std::min<size_t>(length, block.Data.size() / SAMPLE_SIZE(block.Format));
            CopySamples(reinterpret_cast<constSamplePtr>(block.Data.data()), block.Format,
                        reinterpret_cast<samplePtr>(samples.data() + offset), floatSample, count);
        }
    }
    return samples;
}

//! Builds a clip from its saved channels (one element per channel) and adds it to the track
void AddClip(WaveTrack& track, const std::vector<const DocumentElement*>& channels,
             const std::map<long long, DecompressedBlock>& blocks)
{
    const auto& first = *channels.front();
    const double offset = DoubleAttribute(first, "offset", 0.0);
    auto clip = track.CreateClip(offset, wxString::FromUTF8(first.StringAttribute("name").value_or("")));

    std::vector<std::vector<float> > samples;
    for (const auto* channel : channels) {
        samples.push_back(ClipChannelSamples(*channel, blocks));
    }
    // A mono clip saved in a stereo track can't be; repeat the channel if needed
    while (samples.size() < clip->NChannels()) {
        samples.push_back(samples.back());
    }
    const size_t length = samples.front().size();
    std::vector<constSamplePtr> buffers;
    for (auto& channel : samples) {
        channel.resize(length, 0.f);
        buffers.push_back(reinterpret_cast<constSamplePtr>(channel.data()));
    }
    clip->Append(buffers.data(), floatSample, length, 1, floatSample);
    clip->Flush();

    clip->TrimLeftTo(offset + DoubleAttribute(first, "trimLeft", 0.0));
    clip->TrimRightTo(clip->GetSequenceEndTime() - DoubleAttribute(first, "trimRight", 0.0));
    clip->SetPersistentId(UidAttribute(first));
    track.InsertInterval(clip, true);
}

//! Saved clips of a track (channel elements), grouped by clip uid, in order
std::vector<std::vector<const DocumentElement*> > ClipsByUid(const std::vector<const DocumentElement*>& trackChannels)
{
    std::vector<std::vector<const DocumentElement*> > clips;
    std::map<long long, size_t> index;
    for (const auto* channel : trackChannels) {
        for (const auto& clip : channel->children) {
            if (clip.name != "waveclip") {
                continue;
            }
            const auto uid = UidAttribute(clip);
            const auto [it, inserted] = index.emplace(uid, clips.size());
            if (inserted) {
                clips.emplace_back();
            }
            clips[it->second].push_back(&clip);
        }
    }
    return clips;
}
}

void FetchHeadChanges(AudacityProject& project, std::function<void(HeadChangesPtr changes, std::string error)> onDone)
{
    auto& cloudExtension = ProjectCloudExtension::Get(project);
    const auto projectId = cloudExtension.GetCloudProjectId();
    const auto baseSnapshotId = cloudExtension.GetSnapshotId();
    if (projectId.empty() || baseSnapshotId.empty()) {
        onDone(nullptr, "not a cloud project");
        return;
    }

    std::thread([projectId, baseSnapshotId, lockedIds = EditLockedBlockIds(project), onDone = std::move(onDone)]() {
        auto changes = std::make_shared<HeadChanges>();
        std::string error;
        try {
            const auto& config = GetServiceConfig();
            const auto head = WaitForSyncedHead(config, projectId);
            if (head.Id == baseSnapshotId) {
                throw Failure { "nothing new on the server" };
            }
            const auto headInfo = GetSnapshot(config, projectId, head.Id);
            const auto headDocument = DecodeProjectBlob(Download(headInfo.FileUrl));
            const auto baseDocument = DecodeProjectBlob(Download(GetSnapshot(config, projectId, baseSnapshotId).FileUrl));
            if (!headDocument || !baseDocument) {
                throw Failure { "can't read the project's versions" };
            }

            changes->headSnapshotId = head.Id;
            changes->patch = ComputePatch(*baseDocument, *headDocument);

            std::map<long long, std::string> urls;
            for (const auto& block : headInfo.Blocks) {
                urls[std::stoll(block.Hash.substr(0, 8), nullptr, 16)] = block.Url;
            }
            CheckReplacements(*baseDocument, *headDocument, [&](long long id) { return lockedIds.count(id) > 0; },
                              "the changes on the server touch audio that isn't locked");
            // The new blocks: replacing locked runs, and in new tracks and clips
            std::set<long long> neededIds;
            for (const auto& replacement : changes->patch.replacements) {
                neededIds.insert(replacement.addedBlockIds.begin(), replacement.addedBlockIds.end());
            }
            for (const auto& track : changes->patch.newTracks) {
                CollectBlockIds(track, neededIds);
            }
            for (const auto& addition : changes->patch.newClips) {
                CollectBlockIds(addition.clip, neededIds);
            }
            {
                for (const auto id : neededIds) {
                    if (id <= 0 || changes->blocks.count(id)) {
                        continue;
                    }
                    const auto url = urls.find(id);
                    if (url == urls.end()) {
                        throw Failure { "the server has no block " + std::to_string(id) };
                    }
                    const auto data = Download(url->second);
                    auto block = DecompressBlock(data.data(), data.size());
                    if (!block) {
                        throw Failure { "can't read block " + std::to_string(id) };
                    }
                    changes->blocks.emplace(id, std::move(*block));
                }
            }
        } catch (const Failure& failure) {
            error = failure.reason;
        } catch (const std::exception& e) {
            error = e.what();
        }
        HeadChangesPtr result = error.empty() ? changes : nullptr;
        BasicUI::CallAfter([onDone, result, error] { onDone(result, error); });
    }).detach();
}

namespace {
std::string AddNewClipsAndTracks(AudacityProject& project, const HeadChanges& changes)
{
    auto& trackList = TrackList::Get(project);

    // New clips in existing tracks, grouped by track and clip
    std::map<std::pair<std::string, long long>, std::vector<const DocumentElement*> > newClipChannels;
    for (const auto& addition : changes.patch.newClips) {
        newClipChannels[{ addition.trackUid, UidAttribute(addition.clip) }].push_back(&addition.clip);
    }
    for (const auto& [key, channels] : newClipChannels) {
        WaveTrack* track = nullptr;
        for (auto* t : trackList.Any<WaveTrack>()) {
            if (std::to_string(t->GetPersistentId()) == key.first) {
                track = t;
            }
        }
        if (!track) {
            return "a track that got a new clip isn't in this project any more";
        }
        AddClip(*track, channels, changes.blocks);
    }

    // New tracks: consecutive channel elements share the track's uid
    for (size_t i = 0; i < changes.patch.newTracks.size();) {
        std::vector<const DocumentElement*> channels { &changes.patch.newTracks[i++] };
        while (i < changes.patch.newTracks.size()
               && UidAttribute(changes.patch.newTracks[i]) == UidAttribute(*channels.front())) {
            channels.push_back(&changes.patch.newTracks[i++]);
        }
        const auto& first = *channels.front();
        const auto format = static_cast<sampleFormat>(first.IntAttribute("sampleformat").value_or(static_cast<long long>(floatSample)));
        const double rate = DoubleAttribute(first, "rate", 44100.0);
        auto track = WaveTrackFactory::Get(project).Create(channels.size(), format, rate);
        track->SetName(wxString::FromUTF8(first.StringAttribute("name").value_or("")));
        track->SetPersistentId(UidAttribute(first));
        for (const auto& clipChannels : ClipsByUid(channels)) {
            AddClip(*track, clipChannels, changes.blocks);
        }
        trackList.Add(track);
    }
    return {};
}
}

std::string ApplyHeadChanges(AudacityProject& project, const HeadChanges& changes)
{
    auto factory = SampleBlockFactory::New(project);

    // Find all runs first, so that nothing is changed if one is missing
    struct Target {
        std::shared_ptr<WaveClip> clip;
        size_t channel;
        size_t first;
        const ClipReplacement* replacement;
    };
    std::vector<Target> targets;
    for (const auto& replacement : changes.patch.replacements) {
        std::optional<Target> target;
        for (auto* track : TrackList::Get(project).Any<WaveTrack>()) {
            for (const auto& clip : track->Intervals()) {
                if (target || std::to_string(clip->GetPersistentId()) != replacement.clipUid
                    || replacement.channel >= static_cast<long long>(clip->NChannels())) {
                    continue;
                }
                std::vector<long long> ids;
                for (const auto& block : clip->GetSequence(replacement.channel)->GetBlockArray()) {
                    ids.push_back(block.sb->GetBlockID());
                }
                // Same lookup as ApplyPatch on documents
                if (const auto at = FindBlockRun(ids, replacement.removedBlockIds)) {
                    target = Target { clip, static_cast<size_t>(replacement.channel), *at, &replacement };
                }
            }
        }
        if (!target || replacement.addedBlockIds.empty()) {
            return "the audio changed on the server isn't in this project any more";
        }
        targets.push_back(*target);
    }

    for (const auto& target : targets) {
        const auto format = target.clip->GetSequence(target.channel)->GetSampleFormats().Stored();
        std::vector<std::shared_ptr<SampleBlock> > newBlocks;
        const auto& replacement = *target.replacement;
        for (size_t i = 0; i < replacement.addedBlockIds.size(); ++i) {
            const auto id = replacement.addedBlockIds[i];
            if (id <= 0) {
                newBlocks.push_back(factory->CreateSilent(replacement.addedBlockLengths[i], format));
                continue;
            }
            const auto& block = changes.blocks.at(id);
            const auto count = block.Data.size() / SAMPLE_SIZE(block.Format);
            newBlocks.push_back(factory->Create(reinterpret_cast<constSamplePtr>(block.Data.data()), count, block.Format));
        }
        target.clip->ReplaceBlocks(target.channel, target.first, replacement.removedBlockIds.size(), newBlocks);
    }

    // New clips and tracks are built from their saved form, keeping their uids
    // so that later patches can refer to them
    if (const auto error = AddNewClipsAndTracks(project, changes); !error.empty()) {
        return error;
    }

    // The head is the new base: the next save builds on it
    auto& cloudExtension = ProjectCloudExtension::Get(project);
    auto& database = CloudProjectsDatabase::Get();
    auto data = database.GetProjectData(cloudExtension.GetCloudProjectId());
    if (!data) {
        return "the project isn't in the cloud projects database";
    }
    data->SnapshotId = changes.headSnapshotId;
    database.UpdateProjectData(*data);
    cloudExtension.UpdateIdFromDatabase();
    return {};
}
} // namespace audacity::cloud::audiocom::sync
