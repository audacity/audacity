/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "au3-cloud-audiocom/sync/SnapshotReconcile.h"

namespace audacity::cloud::audiocom::sync {
namespace {
const std::string LOCAL_HEAD = "local-head";

SnapshotInfo makeServerHead(std::string id, std::string parentId, const SampleBlockHashes& hashes, int64_t synced = 0)
{
    SnapshotInfo snapshot {};
    snapshot.Id = std::move(id);
    snapshot.ParentId = std::move(parentId);
    snapshot.Synced = synced;
    for (const auto& hash : hashes) {
        snapshot.Blocks.push_back({ hash, {} });
    }
    return snapshot;
}
}

TEST(SnapshotReconcileTests, ResendWhenServerHeadIsLocalHead)
{
    const auto serverHead = makeServerHead(LOCAL_HEAD, "older", { "AA" });

    EXPECT_EQ(DecideReconcile(LOCAL_HEAD, serverHead, { "AA", "BB" }), ReconcileDecision::Resend);
}

TEST(SnapshotReconcileTests, AdoptWhenServerHeadIsChildWithSameBlocks)
{
    const auto serverHead = makeServerHead("new", LOCAL_HEAD, { "AA", "BB" });

    EXPECT_EQ(DecideReconcile(LOCAL_HEAD, serverHead, { "AA", "BB" }), ReconcileDecision::Adopt);
}

TEST(SnapshotReconcileTests, AdoptIgnoresLetterCaseAndOrderOfHashes)
{
    const auto serverHead = makeServerHead("new", LOCAL_HEAD, { "bb", "aa" });

    EXPECT_EQ(DecideReconcile(LOCAL_HEAD, serverHead, { "AA", "BB" }), ReconcileDecision::Adopt);
}

TEST(SnapshotReconcileTests, AdoptWhenProjectHasNoBlocks)
{
    const auto serverHead = makeServerHead("new", LOCAL_HEAD, {});

    EXPECT_EQ(DecideReconcile(LOCAL_HEAD, serverHead, {}), ReconcileDecision::Adopt);
}

TEST(SnapshotReconcileTests, DivergedWhenServerHeadHasOtherParent)
{
    const auto serverHead = makeServerHead("new", "other", { "AA", "BB" });

    EXPECT_EQ(DecideReconcile(LOCAL_HEAD, serverHead, { "AA", "BB" }), ReconcileDecision::Diverged);
}

TEST(SnapshotReconcileTests, DivergedWhenServerHeadHasOtherBlocks)
{
    const auto serverHead = makeServerHead("new", LOCAL_HEAD, { "AA", "CC" });

    EXPECT_EQ(DecideReconcile(LOCAL_HEAD, serverHead, { "AA", "BB" }), ReconcileDecision::Diverged);
}

TEST(SnapshotReconcileTests, DivergedWhenServerHeadIsSynced)
{
    const auto serverHead = makeServerHead("new", LOCAL_HEAD, { "AA", "BB" }, 1);

    EXPECT_EQ(DecideReconcile(LOCAL_HEAD, serverHead, { "AA", "BB" }), ReconcileDecision::Diverged);
}
}
