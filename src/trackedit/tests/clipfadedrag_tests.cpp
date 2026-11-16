/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "trackedit/clipfadedrag.h"
#include "trackedit/tests/mocks/projecthistorymock.h"
#include "trackedit/tests/mocks/trackeditinteractionmock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"

using namespace ::testing;

namespace au::trackedit {
namespace {
const ClipKey CLIP_A { 1, 10 };
const ClipKey CLIP_B { 2, 20 };

constexpr double CLIP_DURATION = 10.0;

constexpr double CLIP_A_FADE_IN = 0.1;
constexpr double CLIP_A_FADE_OUT = 0.2;
constexpr double CLIP_B_FADE_IN = 0.3;
constexpr double CLIP_B_FADE_OUT = 0.4;

//! By how much the lead clip's fades are dragged, when what matters is only that they are
constexpr double OFFSET = 0.4;

Clip makeClip(const ClipKey& key, double fadeIn, double fadeOut)
{
    Clip clip;
    clip.key = key;
    clip.startTime = 0.0;
    clip.endTime = CLIP_DURATION;
    clip.fadeInDuration = fadeIn;
    clip.fadeOutDuration = fadeOut;
    return clip;
}
}

class ClipFadeDragTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        ON_CALL(m_interaction, changeClipFadeIn(_, _, _)).WillByDefault(Return(true));
        ON_CALL(m_interaction, changeClipFadeOut(_, _, _)).WillByDefault(Return(true));
        // An EXPECT_CALL, m_history being a StrictMock
        EXPECT_CALL(m_history, interactionOngoing()).WillRepeatedly(Return(false));
        // A drag that isn't committed, and that most tests leave to `TearDown`, rolls back
        EXPECT_CALL(m_history, rollbackState()).Times(AnyNumber());
    }

    void TearDown() override
    {
        // The test's expectations are about what it did, not about the rollback of a drag it didn't destroy
        Mock::VerifyAndClearExpectations(&m_interaction);
        m_drag.reset();
    }

    //! Whatever the test, the drag starts and ends the user interaction once.
    //! CLIP_A comes first, to check that, by default, it is still the one whose change completes the drag.
    ClipFadeDrag& startDrag(const ClipKey& leadClip, ClipSide side)
    {
        EXPECT_CALL(m_history, startUserInteraction()).Times(1);
        EXPECT_CALL(m_history, endUserInteraction(_)).WillOnce([this](bool) { m_interactionEnded = true; });
        m_drag = ClipFadeDrag::start(m_interaction, m_history, m_project,
                                     { makeClip(CLIP_A, CLIP_A_FADE_IN, CLIP_A_FADE_OUT),
                                       makeClip(CLIP_B, CLIP_B_FADE_IN, CLIP_B_FADE_OUT) },
                                     leadClip, side);
        // Not an ASSERT_NE, which only works in functions returning void
        EXPECT_NE(m_drag, nullptr);
        return *m_drag;
    }

    NiceMock<TrackeditInteractionMock> m_interaction;
    StrictMock<ProjectHistoryMock> m_history;
    std::shared_ptr<NiceMock<TrackeditProjectMock> > m_project = std::make_shared<NiceMock<TrackeditProjectMock> >();
    bool m_interactionEnded = false;
    std::unique_ptr<ClipFadeDrag> m_drag;
};

TEST_F(ClipFadeDragTests, OtherClipsFadeInsChangeByAsMuch)
{
    //! [GIVEN] a drag of CLIP_A's fade-in
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Left);

    //! [THEN] CLIP_B's fade-in is lengthened by as much, and no fade-out changes
    EXPECT_CALL(m_interaction, changeClipFadeIn(CLIP_A, secs_t(CLIP_A_FADE_IN + OFFSET), false));
    EXPECT_CALL(m_interaction, changeClipFadeIn(CLIP_B, secs_t(CLIP_B_FADE_IN + OFFSET), false));
    EXPECT_CALL(m_interaction, changeClipFadeOut(_, _, _)).Times(0);

    //! [WHEN] CLIP_A's fade-in is lengthened
    EXPECT_TRUE(drag.changeFade(CLIP_A_FADE_IN + OFFSET));
}

TEST_F(ClipFadeDragTests, OtherClipsFadeOutsChangeByAsMuch)
{
    //! [GIVEN] a drag of CLIP_A's fade-out
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Right);

    //! [THEN] CLIP_B's fade-out is shortened by as much, and no fade-in changes
    constexpr double shortening = CLIP_A_FADE_OUT / 2;
    EXPECT_CALL(m_interaction, changeClipFadeOut(CLIP_A, secs_t(CLIP_A_FADE_OUT - shortening), false));
    EXPECT_CALL(m_interaction, changeClipFadeOut(CLIP_B, secs_t(CLIP_B_FADE_OUT - shortening), false));
    EXPECT_CALL(m_interaction, changeClipFadeIn(_, _, _)).Times(0);

    //! [WHEN] CLIP_A's fade-out is shortened
    EXPECT_TRUE(drag.changeFade(CLIP_A_FADE_OUT - shortening));
}

TEST_F(ClipFadeDragTests, OtherClipsFadesDontGoBelowZero)
{
    //! [GIVEN] a drag of CLIP_B's fade-in, which is longer than CLIP_A's
    ClipFadeDrag& drag = startDrag(CLIP_B, ClipSide::Left);

    //! [THEN] CLIP_A's fade-in stops at zero
    constexpr double shortening = 2 * CLIP_A_FADE_IN;
    static_assert(shortening > CLIP_A_FADE_IN);
    EXPECT_CALL(m_interaction, changeClipFadeIn(CLIP_A, secs_t(0.0), false));
    EXPECT_CALL(m_interaction, changeClipFadeIn(CLIP_B, secs_t(CLIP_B_FADE_IN - shortening), false));

    //! [WHEN] CLIP_B's fade-in is shortened by more than CLIP_A's fade-in length
    drag.changeFade(CLIP_B_FADE_IN - shortening);
}

TEST_F(ClipFadeDragTests, FollowersDontMoveWhenTheLeadIsDraggedBelowZero)
{
    //! [GIVEN] a drag of CLIP_A's fade-in
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Left);

    //! [THEN] CLIP_A's fade-in stops at zero, and CLIP_B's is shortened by no more than CLIP_A's
    EXPECT_CALL(m_interaction, changeClipFadeIn(CLIP_A, secs_t(0.0), false));
    EXPECT_CALL(m_interaction, changeClipFadeIn(CLIP_B, secs_t(CLIP_B_FADE_IN - CLIP_A_FADE_IN), false));

    //! [WHEN] CLIP_A's fade-in is dragged below zero
    static_assert(OFFSET > CLIP_A_FADE_IN);
    drag.changeFade(-OFFSET);
}

TEST_F(ClipFadeDragTests, FollowersDontMoveWhenTheLeadIsDraggedBeyondItsLength)
{
    //! [GIVEN] a drag of CLIP_A's fade-out
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Right);

    //! [THEN] CLIP_A's fade-out stops at the clip's length, and CLIP_B's is lengthened by no more than CLIP_A's
    EXPECT_CALL(m_interaction, changeClipFadeOut(CLIP_A, secs_t(CLIP_DURATION), false));
    EXPECT_CALL(m_interaction, changeClipFadeOut(CLIP_B, secs_t(CLIP_B_FADE_OUT + CLIP_DURATION - CLIP_A_FADE_OUT), false));

    //! [WHEN] CLIP_A's fade-out is dragged beyond the clip's length
    drag.changeFade(CLIP_DURATION + OFFSET);
}

TEST_F(ClipFadeDragTests, ChangesAreNotCompleted)
{
    //! [GIVEN] a drag of CLIP_A's fade-in
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Left);

    //! [THEN] no change completes, which would publish it
    EXPECT_CALL(m_interaction, changeClipFadeIn(_, _, false)).Times(AnyNumber());
    EXPECT_CALL(m_interaction, changeClipFadeIn(_, _, true)).Times(0);

    //! [WHEN] CLIP_A's fade-in is dragged
    drag.changeFade(CLIP_A_FADE_IN + OFFSET);
}

TEST_F(ClipFadeDragTests, CommitCompletesOnlyTheLeadClipsChange)
{
    //! [GIVEN] CLIP_A's fade-in has been dragged
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Left);
    drag.changeFade(CLIP_A_FADE_IN + OFFSET);

    //! [THEN] CLIP_A's change is made again, completed, so that one state is pushed for all the clips
    EXPECT_CALL(m_interaction, changeClipFadeIn(_, _, true)).Times(0);
    EXPECT_CALL(m_interaction, changeClipFadeIn(CLIP_A, secs_t(CLIP_A_FADE_IN + OFFSET), true));

    //! [WHEN] the drag is committed
    EXPECT_TRUE(drag.commit());
}

TEST_F(ClipFadeDragTests, NothingToCommitWithoutAChange)
{
    //! [GIVEN] a drag of CLIP_A's fade-in, without any change
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Left);

    //! [THEN] no fade changes
    EXPECT_CALL(m_interaction, changeClipFadeIn(_, _, _)).Times(0);
    EXPECT_CALL(m_interaction, changeClipFadeOut(_, _, _)).Times(0);

    //! [WHEN] the drag is committed
    const bool committed = drag.commit();

    //! [THEN] there was nothing to commit
    EXPECT_FALSE(committed);
}

TEST_F(ClipFadeDragTests, TheResultIsThatOfTheDraggedClip)
{
    //! [GIVEN] a drag of CLIP_A's fade-in, where CLIP_B's change succeeds but CLIP_A's fails
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Left);
    EXPECT_CALL(m_interaction, changeClipFadeIn(CLIP_B, _, _)).WillOnce(Return(true));
    EXPECT_CALL(m_interaction, changeClipFadeIn(CLIP_A, _, _)).WillOnce(Return(false));

    //! [WHEN] CLIP_A's fade-in is dragged
    const bool changed = drag.changeFade(CLIP_A_FADE_IN + OFFSET);

    //! [THEN] the drag reports a failure
    EXPECT_FALSE(changed);
}

TEST_F(ClipFadeDragTests, WithoutCommitTheDragRollsBackToTheStateBeforeIt)
{
    //! [GIVEN] CLIP_A's fade-in has been dragged, without a commit
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Left);
    drag.changeFade(CLIP_A_FADE_IN + OFFSET);

    //! [THEN] the project rolls back to the state the user interaction started from, and reloads, rather than the drag
    //! changing the fades back itself
    {
        InSequence sequence;
        EXPECT_CALL(m_history, rollbackState());
        EXPECT_CALL(*m_project, reload());
    }
    EXPECT_CALL(m_interaction, changeClipFadeIn(_, _, _)).Times(0);
    EXPECT_CALL(m_interaction, changeClipFadeOut(_, _, _)).Times(0);

    //! [WHEN] the drag is destroyed
    m_drag.reset();
}

TEST_F(ClipFadeDragTests, CommittedTheDragKeepsTheFades)
{
    //! [GIVEN] CLIP_A's fade-in has been dragged and committed
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Left);
    drag.changeFade(CLIP_A_FADE_IN + OFFSET);
    drag.commit();

    //! [THEN] no fade changes, nor does the project roll back
    EXPECT_CALL(m_interaction, changeClipFadeIn(_, _, _)).Times(0);
    EXPECT_CALL(m_interaction, changeClipFadeOut(_, _, _)).Times(0);
    EXPECT_CALL(m_history, rollbackState()).Times(0);

    //! [WHEN] the drag is destroyed
    m_drag.reset();
}

TEST_F(ClipFadeDragTests, NoDragIfTheLeadClipIsNotAmongTheClips)
{
    //! [GIVEN] a lead clip that is not among the clips
    const ClipKey otherClip { 3, 30 };

    //! [THEN] no user interaction starts, which the StrictMock checks, and no fade changes
    EXPECT_CALL(m_interaction, changeClipFadeIn(_, _, _)).Times(0);
    EXPECT_CALL(m_interaction, changeClipFadeOut(_, _, _)).Times(0);

    //! [WHEN] a drag is started
    const std::unique_ptr<ClipFadeDrag> drag = ClipFadeDrag::start(m_interaction, m_history, m_project,
                                                                   { makeClip(CLIP_A, CLIP_A_FADE_IN, CLIP_A_FADE_OUT),
                                                                     makeClip(CLIP_B, CLIP_B_FADE_IN, CLIP_B_FADE_OUT) },
                                                                   otherClip, ClipSide::Left);

    //! [THEN] there is no drag
    EXPECT_EQ(drag, nullptr);
}

TEST_F(ClipFadeDragTests, TheUserInteractionLastsAsLongAsTheDrag)
{
    //! [GIVEN] a drag of CLIP_A's fade-in, committed
    ClipFadeDrag& drag = startDrag(CLIP_A, ClipSide::Left);
    drag.changeFade(CLIP_A_FADE_IN + OFFSET);
    drag.commit();
    EXPECT_FALSE(m_interactionEnded);

    //! [WHEN] the drag is destroyed
    m_drag.reset();

    //! [THEN] the user interaction ends
    EXPECT_TRUE(m_interactionEnded);
}

TEST_F(ClipFadeDragTests, NoDragWhileAnInteractionIsOngoing)
{
    //! [GIVEN] a user interaction is ongoing
    EXPECT_CALL(m_history, interactionOngoing()).WillRepeatedly(Return(true));

    //! [THEN] no user interaction starts, which the StrictMock checks, and no fade changes
    EXPECT_CALL(m_interaction, changeClipFadeIn(_, _, _)).Times(0);
    EXPECT_CALL(m_interaction, changeClipFadeOut(_, _, _)).Times(0);

    //! [WHEN] a drag is started
    const std::unique_ptr<ClipFadeDrag> drag = ClipFadeDrag::start(m_interaction, m_history, m_project,
                                                                   { makeClip(CLIP_A, CLIP_A_FADE_IN, CLIP_A_FADE_OUT),
                                                                     makeClip(CLIP_B, CLIP_B_FADE_IN, CLIP_B_FADE_OUT) },
                                                                   CLIP_A, ClipSide::Left);

    //! [THEN] there is no drag
    EXPECT_EQ(drag, nullptr);
}
}
