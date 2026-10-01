/*
* Audacity: A Digital Audio Editor
*/
#include <algorithm>

#include <gtest/gtest.h>

#include "au3-cloud-audiocom/sync/CheckoutRebase.h"

using namespace audacity::cloud::audiocom::sync;

namespace {
struct Block {
    long long start;
    long long length;
    long long id;
};

DocumentElement Element(std::string name)
{
    DocumentElement element;
    element.name = std::move(name);
    return element;
}

//! A project with one mono track holding one clip, as saved by Audacity
DocumentElement Project(const std::vector<Block>& blocks, double clipOffset = 0.0)
{
    auto sequence = Element("sequence");
    long long numSamples = 0;
    for (const auto& b : blocks) {
        auto waveblock = Element("waveblock");
        waveblock.SetAttribute("start", b.start);
        waveblock.SetAttribute("length", b.length);
        waveblock.SetAttribute("blockid", b.id);
        sequence.children.push_back(waveblock);
        numSamples += b.length;
    }
    sequence.SetAttribute("numsamples", numSamples);

    auto clip = Element("waveclip");
    clip.SetAttribute("offset", clipOffset);
    clip.SetAttribute("uid", std::string("clip-1"));
    clip.children.push_back(sequence);
    clip.children.push_back(Element("envelope"));

    auto track = Element("wavetrack");
    track.SetAttribute("uid", std::string("track-1"));
    track.SetAttribute("channel", 0LL);
    track.children.push_back(clip);

    auto project = Element("project");
    project.SetAttribute("rate", 44100.0);
    project.children.push_back(track);
    return project;
}

const DocumentElement& ClipOf(const DocumentElement& project)
{
    return project.children.at(0).children.at(0);
}

std::vector<Block> BlocksOf(const DocumentElement& project)
{
    std::vector<Block> blocks;
    for (const auto& child : ClipOf(project).children.at(0).children) {
        blocks.push_back({ *child.IntAttribute("start"), *child.IntAttribute("length"), *child.IntAttribute("blockid") });
    }
    return blocks;
}

bool operator==(const Block& a, const Block& b)
{
    return a.start == b.start && a.length == b.length && a.id == b.id;
}
}

TEST(CheckoutRebaseTests, ProjectBlobRoundTrips)
{
    const auto project = Project({ { 0, 100, 1 }, { 100, 50, 2 } }, 1.25);

    const auto decoded = DecodeProjectBlob(EncodeProjectBlob(project));

    ASSERT_TRUE(decoded.has_value());
    EXPECT_EQ(decoded->name, "project");
    EXPECT_EQ(*decoded->Attribute("rate"), DocumentValue(44100.0));
    EXPECT_EQ(*ClipOf(*decoded).Attribute("offset"), DocumentValue(1.25));
    EXPECT_EQ(ClipOf(*decoded).StringAttribute("uid"), "clip-1");
    EXPECT_EQ(BlocksOf(*decoded), BlocksOf(project));
}

TEST(CheckoutRebaseTests, ReplacedBlocksAreReplayedOnHeadAndRenumbered)
{
    //! [GIVEN] The checkout replaced block 2 by blocks 7 and 8 (same total length)
    const auto base = Project({ { 0, 100, 1 }, { 100, 50, 2 }, { 150, 100, 3 } });
    const auto local = Project({ { 0, 100, 1 }, { 100, 20, 7 }, { 120, 30, 8 }, { 150, 100, 3 } });
    //! [GIVEN] Meanwhile the main moved the clip and created block 7 of its own
    auto head = Project({ { 0, 100, 1 }, { 100, 50, 2 }, { 150, 100, 3 } }, 2.0);
    auto otherTrack = Project({ { 0, 10, 7 } }).children.at(0);
    otherTrack.SetAttribute("uid", std::string("track-2"));
    otherTrack.children.at(0).SetAttribute("uid", std::string("clip-2"));
    head.children.push_back(otherTrack);

    //! [WHEN] Rebasing
    const auto result = RebaseCheckout(base, local, head);

    //! [THEN] Head keeps its move, block 2 is replaced, and the new blocks get ids no document uses
    ASSERT_TRUE(std::holds_alternative<RebaseResult>(result));
    const auto& rebased = std::get<RebaseResult>(result);
    EXPECT_EQ(*ClipOf(rebased.document).Attribute("offset"), DocumentValue(2.0));
    EXPECT_EQ(BlocksOf(rebased.document), (std::vector<Block> { { 0, 100, 1 }, { 100, 20, 9 }, { 120, 30, 10 }, { 150, 100, 3 } }));
    ASSERT_EQ(rebased.newBlocks.size(), 2u);
    EXPECT_EQ(rebased.newBlocks[0].localId, 7);
    EXPECT_EQ(rebased.newBlocks[0].newId, 9);
    EXPECT_EQ(rebased.newBlocks[1].localId, 8);
    EXPECT_EQ(rebased.newBlocks[1].newId, 10);
}

TEST(CheckoutRebaseTests, LengthChangeShiftsFollowingBlocks)
{
    const auto base = Project({ { 0, 100, 1 }, { 100, 50, 2 }, { 150, 100, 3 } });
    const auto local = Project({ { 0, 100, 1 }, { 100, 80, 4 }, { 180, 100, 3 } });
    const auto head = base;

    const auto result = RebaseCheckout(base, local, head);

    ASSERT_TRUE(std::holds_alternative<RebaseResult>(result));
    const auto& rebased = std::get<RebaseResult>(result);
    EXPECT_EQ(BlocksOf(rebased.document), (std::vector<Block> { { 0, 100, 1 }, { 100, 80, 5 }, { 180, 100, 3 } }));
    EXPECT_EQ(ClipOf(rebased.document).children.at(0).IntAttribute("numsamples"), 280);
}

TEST(CheckoutRebaseTests, EditOfReplacedAudioInHeadIsAConflict)
{
    const auto base = Project({ { 0, 100, 1 }, { 100, 50, 2 }, { 150, 100, 3 } });
    const auto local = Project({ { 0, 100, 1 }, { 100, 50, 4 }, { 150, 100, 3 } });
    //! [GIVEN] The main replaced block 2 too, which the lock should have prevented
    const auto head = Project({ { 0, 100, 1 }, { 100, 50, 5 }, { 150, 100, 3 } });

    const auto result = RebaseCheckout(base, local, head);

    EXPECT_TRUE(std::holds_alternative<RebaseConflict>(result));
}


TEST(CheckoutRebaseTests, ClipsWithoutIdsAreAConflict)
{
    //! [GIVEN] A project saved before clips had persistent ids
    auto base = Project({ { 0, 100, 1 } });
    auto local = Project({ { 0, 100, 2 } });
    for (auto* project : { &base, &local }) {
        auto& attributes = project->children.at(0).children.at(0).attributes;
        attributes.erase(std::remove_if(attributes.begin(), attributes.end(), [](const auto& a) { return a.first == "uid"; }),
                         attributes.end());
    }

    const auto result = RebaseCheckout(base, local, base);

    EXPECT_TRUE(std::holds_alternative<RebaseConflict>(result));
}

TEST(CheckoutRebaseTests, ClipMovedToAnotherTrackInHeadIsFound)
{
    const auto base = Project({ { 0, 100, 1 }, { 100, 50, 2 } });
    const auto local = Project({ { 0, 100, 1 }, { 100, 50, 3 } });
    //! [GIVEN] The main moved the clip to a new track
    auto head = Project({});
    head.children.at(0).children.clear();
    auto newTrack = base.children.at(0);
    newTrack.SetAttribute("uid", std::string("track-2"));
    head.children.push_back(newTrack);

    const auto result = RebaseCheckout(base, local, head);

    ASSERT_TRUE(std::holds_alternative<RebaseResult>(result));
    const auto& document = std::get<RebaseResult>(result).document;
    const auto& movedClip = document.children.at(1).children.at(0);
    EXPECT_EQ(*movedClip.children.at(0).children.at(1).IntAttribute("blockid"), 4);
}


TEST(CheckoutRebaseTests, ReplacementsListReplacedAndRemovedBlocks)
{
    const auto before = Project({ { 0, 100, 1 }, { 100, 50, 2 }, { 150, 100, 3 } });
    const auto after = Project({ { 0, 100, 1 }, { 100, 20, 7 }, { 120, 30, 8 }, { 150, 100, 3 } });

    const auto replacements = ComputeReplacements(before, after);

    ASSERT_EQ(replacements.size(), 1u);
    EXPECT_EQ(replacements[0].clipUid, "clip-1");
    EXPECT_EQ(replacements[0].removedBlockIds, (std::vector<long long> { 2 }));
    EXPECT_EQ(replacements[0].addedBlockIds, (std::vector<long long> { 7, 8 }));

    //! A clip that's gone has all its blocks removed
    auto withoutClip = before;
    withoutClip.children.at(0).children.clear();
    const auto removal = ComputeReplacements(before, withoutClip);
    ASSERT_EQ(removal.size(), 1u);
    EXPECT_EQ(removal[0].removedBlockIds, (std::vector<long long> { 1, 2, 3 }));
    EXPECT_TRUE(removal[0].addedBlockIds.empty());

    EXPECT_TRUE(ComputeReplacements(before, before).empty());
}

TEST(CheckoutRebaseTests, FindBlockRunFindsContiguousRunsOnly)
{
    const std::vector<long long> ids { 1, 2, 3, 4 };
    EXPECT_EQ(FindBlockRun(ids, { 2, 3 }), 1u);
    EXPECT_EQ(FindBlockRun(ids, { 4 }), 3u);
    EXPECT_FALSE(FindBlockRun(ids, { 2, 4 }).has_value());
    EXPECT_FALSE(FindBlockRun(ids, {}).has_value());
}

TEST(CheckoutRebaseTests, EditNextToReplacedRunInHeadIsKept)
{
    const auto base = Project({ { 0, 100, 1 }, { 100, 50, 2 }, { 150, 100, 3 } });
    const auto local = Project({ { 0, 100, 1 }, { 100, 50, 4 }, { 150, 100, 3 } });
    //! [GIVEN] The main replaced block 1, just before the locked block 2
    const auto head = Project({ { 0, 100, 5 }, { 100, 50, 2 }, { 150, 100, 3 } });

    const auto result = RebaseCheckout(base, local, head);

    ASSERT_TRUE(std::holds_alternative<RebaseResult>(result));
    EXPECT_EQ(BlocksOf(std::get<RebaseResult>(result).document),
              (std::vector<Block> { { 0, 100, 5 }, { 100, 50, 6 }, { 150, 100, 3 } }));
}

TEST(CheckoutRebaseTests, NewTracksAndClipsAreInThePatchAndApplied)
{
    const auto base = Project({ { 0, 100, 1 } });
    //! [GIVEN] The checkout created a track (e.g. a stem) and a clip in the existing track
    auto local = base;
    auto stem = Project({ { 0, 100, 2 } }).children.at(0);
    stem.SetAttribute("uid", std::string("track-stem"));
    stem.children.at(0).SetAttribute("uid", std::string("clip-stem"));
    local.children.push_back(stem);
    auto newClip = Project({ { 0, 50, 3 } }).children.at(0).children.at(0);
    newClip.SetAttribute("uid", std::string("clip-new"));
    local.children.at(0).children.insert(local.children.at(0).children.begin() + 1, newClip);

    const auto patch = ComputePatch(base, local);

    ASSERT_EQ(patch.newTracks.size(), 1u);
    EXPECT_EQ(patch.newTracks[0].StringAttribute("uid"), "track-stem");
    ASSERT_EQ(patch.newClips.size(), 1u);
    EXPECT_EQ(patch.newClips[0].trackUid, "track-1");
    EXPECT_EQ(patch.newClips[0].clip.StringAttribute("uid"), "clip-new");
    EXPECT_TRUE(patch.replacements.empty());

    //! [WHEN] Applying it on a head where the main created block 2 of its own
    const auto head = Project({ { 0, 100, 1 }, { 100, 10, 2 } });
    const auto result = ApplyPatch(patch, head, 10);

    //! [THEN] Both are added, with blocks renumbered from the first free id
    ASSERT_TRUE(std::holds_alternative<RebaseResult>(result));
    const auto& document = std::get<RebaseResult>(result).document;
    ASSERT_EQ(document.children.size(), 2u);
    EXPECT_EQ(document.children.at(1).StringAttribute("uid"), "track-stem");
    EXPECT_EQ(*document.children.at(1).children.at(0).children.at(0).children.at(0).IntAttribute("blockid"), 11);
    const auto& trackClips = document.children.at(0).children;
    ASSERT_EQ(trackClips.size(), 2u);
    EXPECT_EQ(trackClips.at(1).StringAttribute("uid"), "clip-new");
    EXPECT_EQ(*trackClips.at(1).children.at(0).children.at(0).IntAttribute("blockid"), 10);
}

TEST(CheckoutRebaseTests, ClipMovedToAnotherTrackIsNotNew)
{
    const auto base = Project({ { 0, 100, 1 } });
    auto local = Project({});
    local.children.at(0).children.clear();
    auto otherTrack = base.children.at(0);
    otherTrack.SetAttribute("uid", std::string("track-2"));
    local.children.push_back(otherTrack);

    const auto patch = ComputePatch(base, local);

    ASSERT_EQ(patch.newTracks.size(), 1u) << "the track is new";
    EXPECT_TRUE(patch.newClips.empty()) << "its clip isn't";
}
