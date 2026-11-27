/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "projectscene/view/tracksitemsview/trackclipslistmodel.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "global/tests/mocks/applicationmock.h"
#include "trackedit/tests/mocks/clipfadesmock.h"
#include "trackedit/tests/mocks/projecthistorymock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditinteractionmock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"

using namespace ::testing;

namespace au::projectscene {
namespace {
const ClipKey CLIP { trackedit::TrackItemKey(1, 10) };
const trackedit::ClipKey OTHER_CLIP { 1, 20 };

constexpr double CLIP_DURATION = 10.0;
constexpr double FADE_IN = 1.0;
}

//! Fade edits through the model, which leaves the drags to the clip fades service
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
        m_clipFades = std::make_shared<NiceMock<trackedit::ClipFadesMock> >();

        m_model->globalContext.set(m_globalContext);
        m_model->trackeditInteraction.set(m_interaction);
        m_model->projectHistory.set(m_history);
        m_model->selectionController.set(m_selectionController);
        m_model->application.set(m_application);
        m_model->clipFades.set(m_clipFades);

        ON_CALL(*m_globalContext, currentTrackeditProject()).WillByDefault(Return(m_project));
        ON_CALL(*m_project, clip(_)).WillByDefault([](const trackedit::ClipKey& key) {
            trackedit::Clip clip;
            clip.key = key;
            clip.startTime = 0.0;
            clip.endTime = CLIP_DURATION;
            clip.fadeInDuration = FADE_IN;
            return clip;
        });
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
    std::shared_ptr<NiceMock<trackedit::ClipFadesMock> > m_clipFades;
};

TEST_F(TrackClipsFadeEditTests, AFadeEditDragsTheFadesOfTheClipsItAppliesTo)
{
    //! [GIVEN] Two selected clips
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(trackedit::ClipKeyList { CLIP.key, OTHER_CLIP }));

    //! [THEN] The fade-ins of both are dragged, led by that of the clip whose fade is edited
    EXPECT_CALL(*m_clipFades, startFadeDrag(trackedit::ClipKeyList { CLIP.key, OTHER_CLIP }, CLIP.key, trackedit::ClipSide::Left));

    //! [WHEN] The fade-in of one is edited
    m_model->startClipFadeEdit(CLIP, ClipSides::Side::Left);
}

TEST_F(TrackClipsFadeEditTests, ReleasingTheFadeChangesItAndCommitsTheDrag)
{
    //! [GIVEN] A drag of the clip's fade-in
    ON_CALL(*m_clipFades, draggedFade()).WillByDefault(Return(trackedit::IClipFades::DraggedFade { CLIP.key, trackedit::ClipSide::Left }));

    //! [THEN] The fade changes, and then the drag is committed
    constexpr double releasedFadeIn = 2 * FADE_IN;
    {
        InSequence seq;
        EXPECT_CALL(*m_clipFades, changeFade(trackedit::secs_t(releasedFadeIn))).WillOnce(Return(true));
        EXPECT_CALL(*m_clipFades, commitFadeDrag()).WillOnce(Return(true));
    }

    //! [WHEN] The fade is released
    EXPECT_TRUE(m_model->changeClipFade(CLIP, releasedFadeIn, true));
}

TEST_F(TrackClipsFadeEditTests, TheFadeOfAClipIsntChangedWhileThatOfAnotherIsDragged)
{
    //! [GIVEN] A drag of another clip's fade-in
    const trackedit::IClipFades::DraggedFade otherClipsFadeIn { OTHER_CLIP, trackedit::ClipSide::Left };
    ON_CALL(*m_clipFades, draggedFade()).WillByDefault(Return(otherClipsFadeIn));

    //! [THEN] Nothing changes
    EXPECT_CALL(*m_clipFades, changeFade(_)).Times(0);
    EXPECT_CALL(*m_clipFades, commitFadeDrag()).Times(0);

    //! [WHEN] The clip's fade-in is changed
    EXPECT_FALSE(m_model->changeClipFade(CLIP, 2 * FADE_IN, true));
}

TEST_F(TrackClipsFadeEditTests, FadeTypeChangesApplyToTheSelectionInOneUndoStep)
{
    //! [GIVEN] Two selected clips
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(trackedit::ClipKeyList { CLIP.key, OTHER_CLIP }));
    constexpr auto type = FadeTypes::Type::EQUAL_POWER;
    constexpr auto trackeditType = trackedit::FadeType::EqualPower;

    //! [THEN] Changing the fade-in type of one changes both, and only the last change completes, so that it pushes one
    //! undo step for both
    {
        InSequence seq;
        EXPECT_CALL(*m_interaction, changeClipFadeInType(CLIP.key, trackeditType, false)).WillOnce(Return(true));
        EXPECT_CALL(*m_interaction, changeClipFadeInType(OTHER_CLIP, trackeditType, true)).WillOnce(Return(true));
    }
    //! [WHEN] The fade-in type of one is changed
    EXPECT_TRUE(m_model->changeClipFadeInType(CLIP, type));

    //! [THEN] Likewise with the fade-out
    {
        InSequence seq;
        EXPECT_CALL(*m_interaction, changeClipFadeOutType(CLIP.key, trackeditType, false)).WillOnce(Return(true));
        EXPECT_CALL(*m_interaction, changeClipFadeOutType(OTHER_CLIP, trackeditType, true)).WillOnce(Return(true));
    }
    //! [WHEN] The fade-out type of one is changed
    EXPECT_TRUE(m_model->changeClipFadeOutType(CLIP, type));
}

TEST_F(TrackClipsFadeEditTests, FadeTypeChangesApplyToTheSelectedClipsOfAnotherTypeEvenIfTheClickedOneHasIt)
{
    //! [GIVEN] Two selected clips: the clicked one with an equal-power fade-in and fade-out, the other with linear ones
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(trackedit::ClipKeyList { CLIP.key, OTHER_CLIP }));
    constexpr auto clickedClipType = trackedit::FadeType::EqualPower;
    constexpr auto otherClipType = trackedit::FadeType::Linear;
    ON_CALL(*m_project, clip(_)).WillByDefault([&](const trackedit::ClipKey& key) {
        trackedit::Clip clip;
        clip.key = key;
        clip.fadeInType = clip.fadeOutType = key == CLIP.key ? clickedClipType : otherClipType;
        return clip;
    });

    //! [THEN] Only the other clip changes, and its change completes, so that it pushes the undo step
    EXPECT_CALL(*m_interaction, changeClipFadeInType(CLIP.key, _, _)).Times(0);
    EXPECT_CALL(*m_interaction, changeClipFadeInType(OTHER_CLIP, clickedClipType, true)).WillOnce(Return(true));
    //! [WHEN] The clicked clip's fade-in type is chosen
    EXPECT_TRUE(m_model->changeClipFadeInType(CLIP, FadeTypes::Type::EQUAL_POWER));

    //! [THEN] Likewise with the fade-out
    EXPECT_CALL(*m_interaction, changeClipFadeOutType(CLIP.key, _, _)).Times(0);
    EXPECT_CALL(*m_interaction, changeClipFadeOutType(OTHER_CLIP, clickedClipType, true)).WillOnce(Return(true));
    //! [WHEN] The clicked clip's fade-out type is chosen
    EXPECT_TRUE(m_model->changeClipFadeOutType(CLIP, FadeTypes::Type::EQUAL_POWER));
}

TEST_F(TrackClipsFadeEditTests, ChoosingTheFadeTypeAllSelectedClipsHaveChangesNothing)
{
    //! [GIVEN] Two selected clips with fades of the same type
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(trackedit::ClipKeyList { CLIP.key, OTHER_CLIP }));
    constexpr auto type = trackedit::FadeType::EqualPower;
    ON_CALL(*m_project, clip(_)).WillByDefault([&](const trackedit::ClipKey& key) {
        trackedit::Clip clip;
        clip.key = key;
        clip.fadeInType = clip.fadeOutType = type;
        return clip;
    });

    //! [THEN] No clip changes, so no undo step is pushed
    EXPECT_CALL(*m_interaction, changeClipFadeInType(_, _, _)).Times(0);
    EXPECT_CALL(*m_interaction, changeClipFadeOutType(_, _, _)).Times(0);

    //! [WHEN] That type is chosen for the fade-in, then for the fade-out
    EXPECT_FALSE(m_model->changeClipFadeInType(CLIP, FadeTypes::Type::EQUAL_POWER));
    EXPECT_FALSE(m_model->changeClipFadeOutType(CLIP, FadeTypes::Type::EQUAL_POWER));
}
}
