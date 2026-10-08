/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CheckoutRebase.h

**********************************************************************/
#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "ProjectDocument.h"

namespace audacity::cloud::audiocom::sync {
//! A block of the rebasing checkout that the rebased document references
//! under a new id, so that it can't collide with ids used elsewhere
struct RebasedBlock final
{
    long long localId {};
    long long newId {};
};

struct RebaseResult final
{
    DocumentElement document;
    std::vector<RebasedBlock> newBlocks;
};

struct RebaseConflict final
{
    std::string reason;
};

//! A clip's run of blocks replaced by another between two versions of a project
struct ClipReplacement final
{
    long long channel {};
    std::string clipUid;
    std::vector<long long> removedBlockIds;
    std::vector<long long> addedBlockIds;
    //! Sample counts of the added blocks (needed for silent ones, which aren't stored)
    std::vector<long long> addedBlockLengths;
};

//! The block replacements from `before` to `after`, per clip (matched by channel
//! and clip uid). A clip missing from `after` has all its blocks replaced by none;
//! clips new in `after` aren't listed.
CLOUD_AUDIOCOM_API std::vector<ClipReplacement> ComputeReplacements(const DocumentElement& before, const DocumentElement& after);

//! What a checkout did to a project, relative to the snapshot it was based on:
//! the runs of blocks it replaced, and the tracks and clips it created (e.g.
//! stems of a separation). Computed and applied the same way by the
//! checkout (onto the server's head, to push it) and by the main instance
//! (onto its open project, to integrate it).
//! A clip that's new, in a track that already existed
struct ClipAddition final
{
    std::string trackUid;
    long long channel {};
    DocumentElement clip;
};

struct ProjectPatch final
{
    std::vector<ClipReplacement> replacements;
    //! New tracks, as saved: one `wavetrack` element per channel
    std::vector<DocumentElement> newTracks;
    std::vector<ClipAddition> newClips;
};

CLOUD_AUDIOCOM_API ProjectPatch ComputePatch(const DocumentElement& before, const DocumentElement& after);

//! Where `run` appears, contiguously, in `blockIds`: how both appliers find a
//! replaced run wherever its clip now is and whatever happened around it
CLOUD_AUDIOCOM_API std::optional<size_t> FindBlockRun(const std::vector<long long>& blockIds, const std::vector<long long>& run);

//! Applies a patch to a project document: replacements, then new clips appended
//! to their tracks and new tracks after the existing ones. New blocks are given
//! ids from `firstFreeId` on, so that they can't collide with the target's.
CLOUD_AUDIOCOM_API std::variant<RebaseResult, RebaseConflict>
ApplyPatch(const ProjectPatch& patch, const DocumentElement& target, long long firstFreeId);

//! Replays the audio changes of a checkout on top of a newer project state.
/*!
 ApplyPatch(ComputePatch(base, local), head): each run of blocks the checkout
 replaced must still be found unchanged in `head` (the main instance keeps it
 locked). Clips are matched by channel and clip uid, wherever they are in
 `head` (they may have moved track).

 Other changes of the checkout (clip moves, trims, new clips or tracks) are not
 replayed: `head`'s state wins.

 The checkout's new blocks are given ids not used in any of the documents.
 */
CLOUD_AUDIOCOM_API std::variant<RebaseResult, RebaseConflict>
RebaseCheckout(const DocumentElement& base, const DocumentElement& local, const DocumentElement& head);
} // namespace audacity::cloud::audiocom::sync
