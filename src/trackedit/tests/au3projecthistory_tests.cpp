/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/au3/au3projecthistory.h"
#include "au3interactiontestbase.h"

#include "interactive/tests/mocks/interactivemock.h"

#include "au3-wave-track/Sequence.h"
#include "au3-wave-track/SampleBlock.h"

using namespace ::testing;

namespace au::trackedit {
class Au3ProjectHistoryTests : public Au3InteractionTestBase
{
public:
    void SetUp() override
    {
        m_testCtx = std::make_shared<muse::modularity::Context>(1002);
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackEditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();
        ON_CALL(*m_globalContext, currentProject()).WillByDefault(Return(m_currentProject));
        ON_CALL(*m_globalContext, currentTrackeditProject()).WillByDefault(Return(m_trackEditProject));
        initTestProject();

        m_interactive = std::make_shared<NiceMock<muse::InteractiveMock> >();
        ON_CALL(*m_interactive, buttonData(_)).WillByDefault([](muse::IInteractive::Button button) {
            return muse::IInteractive::ButtonData(static_cast<int>(button), "");
        });
        auto* ioc = muse::modularity::ioc(m_testCtx);
        ioc->registerExport<context::IGlobalContext>("utests", m_globalContext);
        ioc->registerExport<muse::IInteractive>("utests", m_interactive);

        m_trackId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
        m_history = std::make_unique<Au3ProjectHistory>(m_testCtx);
        m_history->init();
    }

    void TearDown() override
    {
        removeTrack(m_trackId);
        m_history.reset();
        Au3InteractionTestBase::TearDown();
        muse::modularity::removeIoC(m_testCtx);
    }

    WaveTrack& track() { return *DomAccessor::findWaveTrack(projectRef(), Au3TrackId(m_trackId)); }

    size_t lockedBlockCount()
    {
        size_t count = 0;
        for (const auto& clip : track().Intervals()) {
            for (const auto& block : clip->GetSequence(0)->GetBlockArray()) {
                count += block.sb->IsEditLocked() ? 1 : 0;
            }
        }
        return count;
    }

    void lockAndCommit()
    {
        const auto clip = *track().Intervals().begin();
        clip->LockBlocks(clip->GetPlayStartTime() + 100 * SAMPLE_INTERVAL, clip->GetPlayStartTime() + 200 * SAMPLE_INTERVAL);
        m_history->pushHistoryState("Lock", "Lock");
        ASSERT_GT(lockedBlockCount(), 0u);
    }

    void answerWarning(muse::IInteractive::Button button)
    {
        EXPECT_CALL(*m_interactive, warningSync(_, _, _, _, _, _)).WillOnce(Return(muse::IInteractive::Result(static_cast<int>(button))));
    }

    muse::modularity::ContextPtr m_testCtx;
    std::shared_ptr<muse::InteractiveMock> m_interactive;
    std::unique_ptr<Au3ProjectHistory> m_history;
    TrackId m_trackId = INVALID_TRACK;
};

TEST_F(Au3ProjectHistoryTests, EditRemovingLockedBlocksIsRolledBackOnCancel)
{
    lockAndCommit();
    const size_t statesBefore = m_history->undoRedoActionCount();
    const size_t lockedBefore = lockedBlockCount();

    //! [GIVEN] The user cancels the warning
    answerWarning(muse::IInteractive::Button::Cancel);

    //! [EXPECT] The UI is resynced, since the edit may already have been reported to it
    EXPECT_CALL(*m_trackEditProject, reload()).Times(1);

    //! [WHEN] An edit deletes the locked audio
    track().Clear(track().GetStartTime(), track().GetEndTime(), false);
    m_history->pushHistoryState("Delete", "Delete");

    //! [THEN] The edit is rolled back and no state is pushed
    EXPECT_EQ(lockedBlockCount(), lockedBefore);
    EXPECT_EQ(m_history->undoRedoActionCount(), statesBefore);
}

TEST_F(Au3ProjectHistoryTests, EditRemovingLockedBlocksIsCommittedOnOk)
{
    lockAndCommit();
    const size_t statesBefore = m_history->undoRedoActionCount();

    //! [GIVEN] The user confirms the warning
    answerWarning(muse::IInteractive::Button::Ok);

    //! [WHEN] An edit deletes the locked audio
    track().Clear(track().GetStartTime(), track().GetEndTime(), false);
    m_history->pushHistoryState("Delete", "Delete");

    //! [THEN] The edit is committed
    EXPECT_EQ(lockedBlockCount(), 0u);
    EXPECT_EQ(m_history->undoRedoActionCount(), statesBefore + 1);
}

TEST_F(Au3ProjectHistoryTests, EditKeepingLockedBlocksDoesNotWarn)
{
    lockAndCommit();

    //! [THEN] No warning
    EXPECT_CALL(*m_interactive, warningSync(_, _, _, _, _, _)).Times(0);

    //! [WHEN] An edit moves the clip, keeping its blocks
    (*track().Intervals().begin())->ShiftBy(1.0);
    m_history->pushHistoryState("Move", "Move");
}

TEST_F(Au3ProjectHistoryTests, UnlockedBlocksDoNotWarn)
{
    lockAndCommit();

    //! [GIVEN] The blocks were unlocked in the meantime
    for (const auto& block : (*track().Intervals().begin())->GetSequence(0)->GetBlockArray()) {
        block.sb->SetEditLocked(false);
    }

    //! [THEN] No warning
    EXPECT_CALL(*m_interactive, warningSync(_, _, _, _, _, _)).Times(0);

    //! [WHEN] An edit deletes the audio
    track().Clear(track().GetStartTime(), track().GetEndTime(), false);
    m_history->pushHistoryState("Delete", "Delete");
}

TEST_F(Au3ProjectHistoryTests, UndoPastLockIsCancelledOnCancel)
{
    lockAndCommit();
    const size_t stateBefore = m_history->currentStateIndex();
    const size_t lockedBefore = lockedBlockCount();

    //! [GIVEN] The user cancels the warning
    answerWarning(muse::IInteractive::Button::Cancel);

    //! [WHEN] Undoing the lock
    m_history->undo();

    //! [THEN] The undo is cancelled
    EXPECT_EQ(m_history->currentStateIndex(), stateBefore);
    EXPECT_EQ(lockedBlockCount(), lockedBefore);
}

TEST_F(Au3ProjectHistoryTests, UndoPastLockProceedsOnOk)
{
    lockAndCommit();
    const size_t stateBefore = m_history->currentStateIndex();

    //! [GIVEN] The user confirms the warning
    answerWarning(muse::IInteractive::Button::Ok);

    //! [WHEN] Undoing the lock
    m_history->undo();

    //! [THEN] The lock is undone
    EXPECT_EQ(m_history->currentStateIndex(), stateBefore - 1);
    EXPECT_EQ(lockedBlockCount(), 0u);
}

TEST_F(Au3ProjectHistoryTests, UndoToIndexPastLockIsCancelledOnCancel)
{
    lockAndCommit();
    const size_t stateBefore = m_history->currentStateIndex();

    //! [GIVEN] The user cancels the warning
    answerWarning(muse::IInteractive::Button::Cancel);

    //! [WHEN] Jumping in the history to before the lock
    m_history->undoRedoToIndex(0);

    //! [THEN] The jump is cancelled
    EXPECT_EQ(m_history->currentStateIndex(), stateBefore);
    EXPECT_GT(lockedBlockCount(), 0u);
}
}
