/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "projectscene/view/tracksitemsview/trackclipslistmodel.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "global/tests/mocks/applicationmock.h"
#include "trackedit/tests/mocks/projecthistorymock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditinteractionmock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"

using namespace ::testing;

namespace au::projectscene {
namespace {
const ClipKey CLIP { trackedit::TrackItemKey(1, 10) };

constexpr double CLIP_DURATION = 10.0;
constexpr double FADE_IN = 1.0;
}

//! A fade edit through the model, on a clip whose fade-in the mocks remember
class TrackClipsFadeEditTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_model = new TrackClipsListModel();
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_project = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();
        m_interaction = std::make_shared<NiceMock<trackedit::TrackeditInteractionMock> >();
        m_history = std::make_shared<NiceMock<trackedit::ProjectHistoryMock> >();
        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_application = std::make_shared<NiceMock<muse::ApplicationMock> >();

        m_model->globalContext.set(m_globalContext);
        m_model->trackeditInteraction.set(m_interaction);
        m_model->projectHistory.set(m_history);
        m_model->selectionController.set(m_selectionController);
        m_model->application.set(m_application);

        ON_CALL(*m_globalContext, currentTrackeditProject()).WillByDefault(Return(m_project));
        ON_CALL(*m_project, clip(_)).WillByDefault([this](const trackedit::ClipKey& key) {
            trackedit::Clip clip;
            clip.key = key;
            clip.startTime = 0.0;
            clip.endTime = CLIP_DURATION;
            clip.fadeInDuration = m_fadeIn;
            return clip;
        });
        ON_CALL(*m_interaction, changeClipFadeIn(_, _, _)).WillByDefault([this](const trackedit::ClipKey&, trackedit::secs_t duration, bool) {
            m_fadeIn = duration;
            return true;
        });
        ON_CALL(*m_interaction, changeClipFadeOut(_, _, _)).WillByDefault(Return(true));
        // Like the real history: rolling back returns to the state the user interaction started from
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
        ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(trackedit::ClipKeyList {}));
        ON_CALL(*m_application, keyboardModifiers()).WillByDefault(Return(Qt::NoModifier));
    }

    void TearDown() override
    {
        delete m_model;
    }

    TrackClipsListModel* m_model = nullptr;
    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext;
    std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_project;
    std::shared_ptr<NiceMock<trackedit::TrackeditInteractionMock> > m_interaction;
    std::shared_ptr<NiceMock<trackedit::ProjectHistoryMock> > m_history;
    std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selectionController;
    std::shared_ptr<NiceMock<muse::ApplicationMock> > m_application;

    double m_fadeIn = FADE_IN;
    bool m_interactionOngoing = false;
    double m_fadeInAtInteractionStart = FADE_IN;
};

TEST_F(TrackClipsFadeEditTests, AbandonedDragIsNotTheStartingPointOfTheNextOne)
{
    //! [GIVEN] A drag of the fade-in that previews a longer fade, and is never completed nor canceled
    m_model->startClipFadeEdit(CLIP, ClipSides::Side::Left);
    m_model->changeClipFade(CLIP, 2 * FADE_IN, false);

    //! [WHEN] Another drag of it starts, previews another length, and is canceled
    m_model->startClipFadeEdit(CLIP, ClipSides::Side::Left);
    m_model->changeClipFade(CLIP, 3 * FADE_IN, false);
    m_model->cancelClipFadeEdit();

    //! [THEN] The fade-in is as it was before either drag
    EXPECT_DOUBLE_EQ(m_fadeIn, FADE_IN);
}
}
