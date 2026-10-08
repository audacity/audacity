/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CheckoutRebase.cpp

**********************************************************************/
#include "CheckoutRebase.h"

#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <tuple>

namespace audacity::cloud::audiocom::sync {
namespace {
//! Not the track: a clip keeps its uid when moved to another track
using ClipKey = std::tuple<long long /*channel*/, std::string /*clip uid*/>;

struct Block final
{
    long long start {};
    long long length {};
    long long id {};
};

std::string UidOf(const DocumentElement& element)
{
    if (const auto s = element.StringAttribute("uid")) {
        return *s;
    }
    if (const auto i = element.IntAttribute("uid")) {
        return std::to_string(*i);
    }
    return {};
}

template<typename Element, typename Visitor>
void ForEachClip(Element& root, Visitor visit)
{
    for (auto& track : root.children) {
        if (track.name != "wavetrack") {
            continue;
        }
        const auto channel = track.IntAttribute("channel").value_or(0);
        for (auto& clip : track.children) {
            // Nested waveclips are cutlines, not visited
            if (clip.name == "waveclip") {
                visit(ClipKey { channel, UidOf(clip) }, clip);
            }
        }
    }
}

template<typename Element>
auto SequenceOf(Element& clip) -> decltype(&clip.children.front())
{
    for (auto& child : clip.children) {
        if (child.name == "sequence") {
            return &child;
        }
    }
    return nullptr;
}

std::vector<Block> BlocksOf(const DocumentElement& sequence)
{
    std::vector<Block> blocks;
    for (const auto& child : sequence.children) {
        if (child.name == "waveblock") {
            blocks.push_back({ child.IntAttribute("start").value_or(0), child.IntAttribute("length").value_or(0),
                               child.IntAttribute("blockid").value_or(0) });
        }
    }
    return blocks;
}

void CollectBlockIds(const DocumentElement& element, long long& maxId)
{
    if (element.name == "waveblock") {
        maxId = std::max(maxId, element.IntAttribute("blockid").value_or(0));
    }
    for (const auto& child : element.children) {
        CollectBlockIds(child, maxId);
    }
}

bool SameIds(const std::vector<Block>& a, const std::vector<Block>& b)
{
    return std::equal(a.begin(), a.end(), b.begin(), b.end(), [](const Block& x, const Block& y) { return x.id == y.id; });
}

struct Diff final
{
    size_t prefix {};
    std::vector<Block> removed;
    std::vector<Block> added;
};

//! The run of blocks replaced between two versions of a sequence: what lies
//! between their common prefix and suffix
Diff DiffBlocks(const std::vector<Block>& before, const std::vector<Block>& after)
{
    size_t prefix = 0;
    while (prefix < before.size() && prefix < after.size() && before[prefix].id == after[prefix].id) {
        ++prefix;
    }
    size_t suffix = 0;
    while (suffix < before.size() - prefix && suffix < after.size() - prefix
           && before[before.size() - 1 - suffix].id == after[after.size() - 1 - suffix].id) {
        ++suffix;
    }
    return { prefix, { before.begin() + prefix, before.end() - suffix }, { after.begin() + prefix, after.end() - suffix } };
}
} // namespace

std::vector<ClipReplacement> ComputeReplacements(const DocumentElement& before, const DocumentElement& after)
{
    std::map<ClipKey, const DocumentElement*> afterClips;
    ForEachClip(after, [&](const ClipKey& key, const DocumentElement& clip) { afterClips[key] = &clip; });

    std::vector<ClipReplacement> replacements;
    ForEachClip(before, [&](const ClipKey& key, const DocumentElement& beforeClip) {
        const auto beforeSequence = SequenceOf(beforeClip);
        if (!beforeSequence) {
            return;
        }
        const auto beforeBlocks = BlocksOf(*beforeSequence);
        const auto it = afterClips.find(key);
        const auto afterSequence = it == afterClips.end() ? nullptr : SequenceOf(*it->second);
        // A clip that's gone counts as all its blocks replaced by nothing
        const auto afterBlocks = afterSequence ? BlocksOf(*afterSequence) : std::vector<Block> {};
        if (afterSequence && SameIds(beforeBlocks, afterBlocks)) {
            return;
        }
        const auto diff = DiffBlocks(beforeBlocks, afterBlocks);
        ClipReplacement replacement { std::get<0>(key), std::get<1>(key), {}, {}, {} };
        for (const auto& b : diff.removed) {
            replacement.removedBlockIds.push_back(b.id);
        }
        for (const auto& b : diff.added) {
            replacement.addedBlockIds.push_back(b.id);
            replacement.addedBlockLengths.push_back(b.length);
        }
        replacements.push_back(std::move(replacement));
    });
    return replacements;
}

ProjectPatch ComputePatch(const DocumentElement& before, const DocumentElement& after)
{
    ProjectPatch patch { ComputeReplacements(before, after), {}, {} };

    std::set<std::string> beforeTracks;
    std::set<std::string> beforeClips;
    for (const auto& track : before.children) {
        if (track.name == "wavetrack") {
            beforeTracks.insert(UidOf(track));
            for (const auto& clip : track.children) {
                if (clip.name == "waveclip") {
                    beforeClips.insert(UidOf(clip));
                }
            }
        }
    }
    for (const auto& track : after.children) {
        if (track.name != "wavetrack") {
            continue;
        }
        const auto trackUid = UidOf(track);
        if (!beforeTracks.count(trackUid)) {
            patch.newTracks.push_back(track);
            continue;
        }
        const auto channel = track.IntAttribute("channel").value_or(0);
        for (const auto& clip : track.children) {
            // A clip that existed may just have moved track: not new
            if (clip.name == "waveclip" && !beforeClips.count(UidOf(clip))) {
                patch.newClips.push_back({ trackUid, channel, clip });
            }
        }
    }
    return patch;
}

std::optional<size_t> FindBlockRun(const std::vector<long long>& blockIds, const std::vector<long long>& run)
{
    if (run.empty() || run.size() > blockIds.size()) {
        return {};
    }
    const auto it = std::search(blockIds.begin(), blockIds.end(), run.begin(), run.end());
    if (it == blockIds.end()) {
        return {};
    }
    return static_cast<size_t>(it - blockIds.begin());
}

std::variant<RebaseResult, RebaseConflict> ApplyPatch(const ProjectPatch& patch, const DocumentElement& target, long long firstFreeId)
{
    RebaseResult result { target, {} };
    std::map<ClipKey, DocumentElement*> targetClips;
    ForEachClip(result.document, [&](const ClipKey& key, DocumentElement& clip) { targetClips[key] = &clip; });

    long long nextId = firstFreeId;
    std::map<long long, long long> renumbered;
    const auto newIdFor = [&](long long id) {
        // Silent blocks have non-positive ids and aren't stored
        if (id <= 0) {
            return id;
        }
        const auto [it, inserted] = renumbered.emplace(id, nextId);
        if (inserted) {
            ++nextId;
            result.newBlocks.push_back({ id, it->second });
        }
        return it->second;
    };

    const std::function<void(DocumentElement&)> renumberBlocks = [&](DocumentElement& element) {
        if (element.name == "waveblock") {
            element.SetAttribute("blockid", newIdFor(element.IntAttribute("blockid").value_or(0)));
        }
        for (auto& child : element.children) {
            renumberBlocks(child);
        }
    };

    for (const auto& replacement : patch.replacements) {
        if (replacement.removedBlockIds.empty() || replacement.addedBlockIds.empty()) {
            return RebaseConflict { "audio was added or removed instead of replaced" };
        }
        const auto clipIt = targetClips.find({ replacement.channel, replacement.clipUid });
        const auto sequence = clipIt == targetClips.end() ? nullptr : SequenceOf(*clipIt->second);
        if (!sequence) {
            return RebaseConflict { "a clip changed by the checkout no longer exists" };
        }

        auto& children = sequence->children;
        std::vector<size_t> blockChildIndices;
        for (size_t i = 0; i < children.size(); ++i) {
            if (children[i].name == "waveblock") {
                blockChildIndices.push_back(i);
            }
        }
        const auto blocks = BlocksOf(*sequence);
        std::vector<long long> ids;
        for (const auto& b : blocks) {
            ids.push_back(b.id);
        }
        const auto at = FindBlockRun(ids, replacement.removedBlockIds);
        if (!at) {
            return RebaseConflict { "audio changed by the checkout was edited" };
        }

        // Splice, and shift what follows if the length changed
        const size_t count = replacement.removedBlockIds.size();
        long long removedLength = 0;
        for (size_t i = *at; i < *at + count; ++i) {
            removedLength += blocks[i].length;
        }
        long long start = blocks[*at].start;
        long long addedLength = 0;
        std::vector<DocumentElement> newChildren;
        for (size_t i = 0; i < replacement.addedBlockIds.size(); ++i) {
            DocumentElement waveblock;
            waveblock.name = "waveblock";
            waveblock.SetAttribute("start", start);
            waveblock.SetAttribute("length", replacement.addedBlockLengths[i]);
            waveblock.SetAttribute("blockid", newIdFor(replacement.addedBlockIds[i]));
            newChildren.push_back(std::move(waveblock));
            start += replacement.addedBlockLengths[i];
            addedLength += replacement.addedBlockLengths[i];
        }
        const long long delta = addedLength - removedLength;

        const size_t firstChild = blockChildIndices[*at];
        const size_t lastChild = blockChildIndices[*at + count - 1] + 1;
        for (size_t i = lastChild; i < children.size(); ++i) {
            if (children[i].name == "waveblock") {
                children[i].SetAttribute("start", children[i].IntAttribute("start").value_or(0) + delta);
            }
        }
        children.erase(children.begin() + firstChild, children.begin() + lastChild);
        children.insert(children.begin() + firstChild, newChildren.begin(), newChildren.end());
        sequence->SetAttribute("numsamples", sequence->IntAttribute("numsamples").value_or(0) + delta);
    }

    auto& tracks = result.document.children;
    for (const auto& addition : patch.newClips) {
        const auto track = std::find_if(tracks.begin(), tracks.end(), [&](const DocumentElement& t) {
            return t.name == "wavetrack" && UidOf(t) == addition.trackUid && t.IntAttribute("channel").value_or(0) == addition.channel;
        });
        if (track == tracks.end()) {
            return RebaseConflict { "a track that got a new clip no longer exists" };
        }
        auto clip = addition.clip;
        renumberBlocks(clip);
        // Before the track's trailing elements, if any, as the clips are saved
        const auto lastClip = std::find_if(track->children.rbegin(), track->children.rend(),
                                           [](const DocumentElement& c) { return c.name == "waveclip"; });
        track->children.insert(lastClip.base(), std::move(clip));
    }

    auto insertAt = std::find_if(tracks.rbegin(), tracks.rend(), [](const DocumentElement& t) {
        return t.name == "wavetrack" || t.name == "labeltrack";
    }).base();
    for (const auto& newTrack : patch.newTracks) {
        auto track = newTrack;
        renumberBlocks(track);
        insertAt = tracks.insert(insertAt, std::move(track)) + 1;
    }
    return result;
}

std::variant<RebaseResult, RebaseConflict>
RebaseCheckout(const DocumentElement& base, const DocumentElement& local, const DocumentElement& head)
{
    bool missingIds = false;
    ForEachClip(local, [&](const ClipKey& key, const DocumentElement&) { missingIds |= std::get<1>(key).empty(); });
    if (missingIds) {
        return RebaseConflict { "the project was saved before clips had ids" };
    }

    long long maxId = 0;
    CollectBlockIds(base, maxId);
    CollectBlockIds(local, maxId);
    CollectBlockIds(head, maxId);
    return ApplyPatch(ComputePatch(base, local), head, maxId + 1);
}
} // namespace audacity::cloud::audiocom::sync
