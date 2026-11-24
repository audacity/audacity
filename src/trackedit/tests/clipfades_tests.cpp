/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "trackedit/internal/clipfades.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "trackedit/tests/mocks/projecthistorymock.h"
#include "trackedit/tests/mocks/trackeditinteractionmock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"

using namespace ::testing;

namespace au::trackedit {
namespace {
const ClipKey CLIP { 1, 10 };

constexpr double CLIP_START = 0.0;
constexpr double CLIP_END = 10.0;
constexpr double FADE_IN = 1.0;
constexpr double FADE_OUT = 2.0;
}

//! A real service, on a clip whose fade-in the mocks remember, and whose history rolls back to where the user interaction
//! started
class ClipFadesTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_clipFades = std::make_unique<ClipFades>(muse::modularity::globalCtx());
        m_clipFades->globalContext.set(m_globalContext);
        m_clipFades->trackeditInteraction.set(m_interaction);
        m_clipFades->projectHistory.set(m_history);

        ON_CALL(*m_globalContext, currentTrackeditProject()).WillByDefault(Return(m_project));
        ON_CALL(*m_project, clip(_)).WillByDefault([this](const ClipKey& key) {
            Clip clip;
            clip.key = key;
            clip.startTime = CLIP_START;
            clip.endTime = CLIP_END;
            clip.fadeInDuration = m_fadeIn;
            clip.fadeOutDuration = FADE_OUT;
            return clip;
        });
        ON_CALL(*m_interaction, changeClipFadeIn(_, _, _)).WillByDefault([this](const ClipKey&, secs_t duration, bool) {
            m_fadeIn = duration;
            return true;
        });
        ON_CALL(*m_interaction, cancelDragEditRequested()).WillByDefault(Return(m_cancelDragEditRequested));
        ON_CALL(*m_history, startUserInteraction()).WillByDefault([this]() {
            m_interactionOngoing = true;
            m_fadeInAtInteractionStart = m_fadeIn;
        });
        ON_CALL(*m_history, rollbackState()).WillByDefault([this]() {
            m_interactionOngoing = false;
            m_fadeIn = m_fadeInAtInteractionStart;
        });
        ON_CALL(*m_history, endUserInteraction(_)).WillByDefault([this](bool) { m_interactionOngoing = false; });
        ON_CALL(*m_history, interactionOngoing()).WillByDefault([this]() { return m_interactionOngoing; });

        m_clipFades->init();
    }

    std::unique_ptr<ClipFades> m_clipFades;
    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
    std::shared_ptr<NiceMock<TrackeditProjectMock> > m_project = std::make_shared<NiceMock<TrackeditProjectMock> >();
    std::shared_ptr<NiceMock<TrackeditInteractionMock> > m_interaction = std::make_shared<NiceMock<TrackeditInteractionMock> >();
    std::shared_ptr<NiceMock<ProjectHistoryMock> > m_history = std::make_shared<NiceMock<ProjectHistoryMock> >();
    muse::async::Notification m_cancelDragEditRequested;

    double m_fadeIn = FADE_IN;
    bool m_interactionOngoing = false;
    double m_fadeInAtInteractionStart = FADE_IN;
};

TEST_F(ClipFadesTests, AbandonedDragIsNotTheStartingPointOfTheNextOne)
{
    //! [GIVEN] A drag of the fade-in that previews a longer fade, and is never completed nor canceled
    m_clipFades->startFadeDrag({ CLIP }, CLIP, ClipSide::Left);
    m_clipFades->changeFade(2 * FADE_IN);

    //! [WHEN] Another drag of it starts, previews another length, and is canceled
    m_clipFades->startFadeDrag({ CLIP }, CLIP, ClipSide::Left);
    m_clipFades->changeFade(3 * FADE_IN);
    m_clipFades->cancelFadeDrag();

    //! [THEN] The fade-in is as it was before either drag
    EXPECT_DOUBLE_EQ(m_fadeIn, FADE_IN);
}

TEST_F(ClipFadesTests, WithoutADragAllFadeEndsAreSnappable)
{
    EXPECT_EQ(m_clipFades->snappableFadeEnds(m_project->clip(CLIP)), (IClipFades::FadeEnds { CLIP_START + FADE_IN, CLIP_END - FADE_OUT }));
}

TEST_F(ClipFadesTests, TheDraggedFadeIsntSnappable)
{
    //! [WHEN] The fade-in is dragged
    m_clipFades->startFadeDrag({ CLIP }, CLIP, ClipSide::Left);

    //! [THEN] Only the fade-out is snappable
    EXPECT_EQ(m_clipFades->snappableFadeEnds(m_project->clip(CLIP)), (IClipFades::FadeEnds { std::nullopt, CLIP_END - FADE_OUT }));
}

TEST_F(ClipFadesTests, WhileTheDragRollsBackNoFadeIsDragged)
{
    //! [GIVEN] A drag of the fade-in, which changed it
    m_clipFades->startFadeDrag({ CLIP }, CLIP, ClipSide::Left);
    m_clipFades->changeFade(2 * FADE_IN);

    //! [THEN] As the drag rolls back, e.g. when the snap points are rebuilt, no fade is dragged, and all are snappable
    std::optional<IClipFades::DraggedFade> draggedFadeDuringRollback;
    IClipFades::FadeEnds fadeEndsDuringRollback;
    EXPECT_CALL(*m_history, rollbackState()).WillOnce([&]() {
        m_fadeIn = m_fadeInAtInteractionStart;
        draggedFadeDuringRollback = m_clipFades->draggedFade();
        fadeEndsDuringRollback = m_clipFades->snappableFadeEnds(m_project->clip(CLIP));
    });

    //! [WHEN] The drag is canceled
    m_clipFades->cancelFadeDrag();

    EXPECT_FALSE(draggedFadeDuringRollback.has_value());
    EXPECT_EQ(fadeEndsDuringRollback, (IClipFades::FadeEnds { CLIP_START + FADE_IN, CLIP_END - FADE_OUT }));
}

TEST_F(ClipFadesTests, CancelingDragEditsCancelsTheDrag)
{
    //! [GIVEN] A drag of the fade-in, which changed it
    m_clipFades->startFadeDrag({ CLIP }, CLIP, ClipSide::Left);
    m_clipFades->changeFade(2 * FADE_IN);

    //! [WHEN] Drag edits are canceled, e.g. by Escape
    m_cancelDragEditRequested.notify();

    //! [THEN] The drag is over, and the fade-in is as it was before it
    EXPECT_FALSE(m_clipFades->draggedFade().has_value());
    EXPECT_DOUBLE_EQ(m_fadeIn, FADE_IN);
}

TEST_F(ClipFadesTests, ObserversAreToldWhenADragStartsAndEnds)
{
    int notifications = 0;
    muse::async::Asyncable observer;
    m_clipFades->draggedFadeChanged().onNotify(&observer, [&]() { ++notifications; });

    //! [WHEN] A drag starts
    m_clipFades->startFadeDrag({ CLIP }, CLIP, ClipSide::Left);
    //! [THEN] Observers are told
    EXPECT_EQ(notifications, 1);

    //! [WHEN] It ends
    m_clipFades->cancelFadeDrag();
    //! [THEN] Observers are told again
    EXPECT_EQ(notifications, 2);
}

TEST_F(ClipFadesTests, ObserversAreToldWhenTheDragChangesFades)
{
    //! [GIVEN] A drag of the fade-in
    m_clipFades->startFadeDrag({ CLIP }, CLIP, ClipSide::Left);
    int notifications = 0;
    muse::async::Asyncable observer;
    m_clipFades->draggedFadeChanged().onNotify(&observer, [&]() { ++notifications; });

    //! [WHEN] It lengthens the fade-in over the fade-out, which it pushes
    m_clipFades->changeFade(CLIP_END - CLIP_START);

    //! [THEN] Observers are told
    EXPECT_EQ(notifications, 1);
}
}
