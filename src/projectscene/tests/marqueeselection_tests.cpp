/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "projectscene/view/tracksitemsview/selectionviewcontroller.h"
#include "projectscene/view/timeline/timelinecontext.h"

#include "snaptestaccess.h"

#include "global/tests/mocks/applicationmock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "playback/tests/mocks/playbackmock.h"
#include "spectrogram/tests/mocks/globalspectrogramconfigurationmock.h"
#include "spectrogram/tests/mocks/frequencyselectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/tracknavigationcontrollermock.h"
#include "mocks/audiooutputmock.h"
#include "mocks/projectviewstatemock.h"

using namespace ::testing;

namespace au::projectscene {
//! The timeline context is left at its default zoom, so one second is one pixel
class MarqueeSelectionTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_project = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackeditProject = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();
        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_trackNavigationController = std::make_shared<NiceMock<trackedit::TrackNavigationControllerMock> >();
        m_spectrogramConfiguration = std::make_shared<NiceMock<spectrogram::GlobalSpectrogramConfigurationMock> >();
        m_frequencySelectionController = std::make_shared<NiceMock<spectrogram::FrequencySelectionControllerMock> >();
        m_application = std::make_shared<NiceMock<muse::ApplicationMock> >();
        m_viewState = std::make_shared<NiceMock<ProjectViewStateMock> >();
        m_playback = std::make_shared<NiceMock<playback::PlaybackMock> >();
        m_audioOutput = std::make_shared<NiceMock<playback::AudioOutputMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();

        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_project));
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_globalContext, playbackState())
        .WillByDefault(Return(m_playbackState));
        ON_CALL(*m_project, viewState())
        .WillByDefault(Return(m_viewState));
        ON_CALL(*m_project, trackeditProject())
        .WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_playback, audioOutput())
        .WillByDefault(Return(m_audioOutput));
        ON_CALL(*m_application, keyboardModifiers())
        .WillByDefault(Return(Qt::ControlModifier));

        m_context = new TimelineContext();
        SnapTestAccess::wireContext(m_context, m_globalContext, m_playback);

        m_controller = new SelectionViewController();
        SnapTestAccess::wireSelection(m_controller, m_globalContext, m_selectionController, m_trackNavigationController,
                                      m_spectrogramConfiguration, m_frequencySelectionController, m_application);
        m_controller->setTimelineContext(m_context);
    }

    void TearDown() override
    {
        delete m_controller;
        delete m_context;
    }

    //! Ctrl+press at (1s, 10px) and drag to (20s, 40px), which reaches two clips on track 1 and a label on track 2
    void dragBoxOverItems()
    {
        ON_CALL(*m_viewState, tracksInRange(10.0, 40.0))
        .WillByDefault(Return(trackedit::TrackIdList { 1, 2 }));
        ON_CALL(*m_selectionController,
                itemsTouchingRange(trackedit::TrackIdList { 1, 2 }, trackedit::secs_t(1.0), trackedit::secs_t(20.0)))
        .WillByDefault(Return(m_touched));

        m_controller->onPressed(1.0, 10.0);
        m_controller->onPositionChanged(20.0, 40.0);
    }

    const trackedit::ItemKeys m_touched { { { 1, 11 }, { 1, 12 } }, { { 2, 21 } } };

    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_project;
    std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_trackeditProject;
    std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selectionController;
    std::shared_ptr<NiceMock<trackedit::TrackNavigationControllerMock> > m_trackNavigationController;
    std::shared_ptr<NiceMock<spectrogram::GlobalSpectrogramConfigurationMock> > m_spectrogramConfiguration;
    std::shared_ptr<NiceMock<spectrogram::FrequencySelectionControllerMock> > m_frequencySelectionController;
    std::shared_ptr<NiceMock<muse::ApplicationMock> > m_application;
    std::shared_ptr<NiceMock<ProjectViewStateMock> > m_viewState;
    std::shared_ptr<NiceMock<playback::PlaybackMock> > m_playback;
    std::shared_ptr<NiceMock<playback::AudioOutputMock> > m_audioOutput;
    std::shared_ptr<NiceMock<context::PlaybackStateMock> > m_playbackState;

    TimelineContext* m_context = nullptr;
    SelectionViewController* m_controller = nullptr;
};

TEST_F(MarqueeSelectionTests, CtrlDragSelectsWhatTheBoxTouchesAndDropsTheTimeSelection)
{
    //! [GIVEN] A Ctrl+press on empty track area
    ON_CALL(*m_viewState, tracksInRange(10.0, 40.0))
    .WillByDefault(Return(trackedit::TrackIdList { 1, 2 }));
    ON_CALL(*m_selectionController, itemsTouchingRange(trackedit::TrackIdList { 1, 2 }, trackedit::secs_t(1.0), trackedit::secs_t(20.0)))
    .WillByDefault(Return(m_touched));
    m_controller->onPressed(1.0, 10.0);
    ASSERT_FALSE(m_controller->marqueeActive());

    //! [EXPECT] The touched items and their tracks become the selection and the time selection is cleared
    EXPECT_CALL(*m_selectionController, resetDataSelection()).Times(1);
    EXPECT_CALL(*m_selectionController, setSelectedClips(m_touched.clips, true)).Times(1);
    EXPECT_CALL(*m_selectionController, setSelectedLabels(m_touched.labels, true)).Times(1);
    EXPECT_CALL(*m_selectionController, setSelectedTracks(trackedit::TrackIdList { 1, 2 }, true)).Times(1);
    EXPECT_CALL(*m_selectionController, setDataSelectedStartTime(_, _)).Times(0);
    EXPECT_CALL(*m_selectionController, setDataSelectedEndTime(_, _)).Times(0);

    //! [WHEN] The pointer is dragged past the threshold
    m_controller->onPositionChanged(20.0, 40.0);

    //! [THEN] The marquee is active and spans the press and the pointer
    EXPECT_TRUE(m_controller->marqueeActive());
    EXPECT_EQ(m_controller->marqueeRect(), QRectF(1.0, 10.0, 19.0, 30.0));
}

TEST_F(MarqueeSelectionTests, ContractingTheBoxDeselectsTheItemsItLeaves)
{
    //! [GIVEN] A marquee that reaches two clips and a label
    dragBoxOverItems();
    ASSERT_TRUE(m_controller->marqueeActive());

    //! [GIVEN] Pulled back, the box only reaches the first clip
    ON_CALL(*m_viewState, tracksInRange(10.0, 15.0))
    .WillByDefault(Return(trackedit::TrackIdList { 1 }));
    const trackedit::ItemKeys stillTouched { { { 1, 11 } }, {} };
    ON_CALL(*m_selectionController, itemsTouchingRange(trackedit::TrackIdList { 1 }, trackedit::secs_t(1.0), trackedit::secs_t(5.0)))
    .WillByDefault(Return(stillTouched));

    //! [EXPECT] Only that clip and its track stay selected
    EXPECT_CALL(*m_selectionController, setSelectedClips(stillTouched.clips, true)).Times(1);
    EXPECT_CALL(*m_selectionController, setSelectedLabels(trackedit::LabelKeyList {}, true)).Times(1);
    EXPECT_CALL(*m_selectionController, setSelectedTracks(trackedit::TrackIdList { 1 }, true)).Times(1);

    //! [WHEN] The pointer moves back towards the press point
    m_controller->onPositionChanged(5.0, 15.0);

    //! [THEN] The box follows the pointer
    EXPECT_EQ(m_controller->marqueeRect(), QRectF(1.0, 10.0, 4.0, 5.0));
}

TEST_F(MarqueeSelectionTests, ABoxDraggedLeftAndUpIsNormalised)
{
    //! [GIVEN] A Ctrl+press at the bottom right of the items
    ON_CALL(*m_viewState, tracksInRange(40.0, 10.0))
    .WillByDefault(Return(trackedit::TrackIdList { 1, 2 }));
    m_controller->onPressed(20.0, 40.0);

    //! [EXPECT] The range query gets the box's ordered time span
    EXPECT_CALL(*m_selectionController,
                itemsTouchingRange(trackedit::TrackIdList { 1, 2 }, trackedit::secs_t(1.0), trackedit::secs_t(20.0)))
    .WillOnce(Return(m_touched));

    //! [WHEN] The pointer is dragged up and to the left
    m_controller->onPositionChanged(1.0, 10.0);

    //! [THEN] The box is the same rectangle as a drag in the other direction
    EXPECT_EQ(m_controller->marqueeRect(), QRectF(1.0, 10.0, 19.0, 30.0));
}

TEST_F(MarqueeSelectionTests, CtrlClickWithoutADragKeepsTheClickBehaviour)
{
    //! [GIVEN] A Ctrl+press on empty track area
    m_controller->onPressed(1.0, 10.0);

    //! [EXPECT] No item query is made and the release still completes the time selection
    EXPECT_CALL(*m_selectionController, itemsTouchingRange(_, _, _)).Times(0);
    EXPECT_CALL(*m_selectionController, setDataSelectedStartTime(_, true)).Times(1);
    EXPECT_CALL(*m_selectionController, setDataSelectedEndTime(_, true)).Times(1);

    //! [WHEN] The pointer moves less than the drag threshold and is released
    m_controller->onPositionChanged(3.0, 12.0);
    EXPECT_FALSE(m_controller->marqueeActive());
    m_controller->onReleased(3.0, 12.0);

    //! [THEN] No marquee was started
    EXPECT_FALSE(m_controller->marqueeActive());
    EXPECT_FALSE(m_controller->selectionInProgress());
}

TEST_F(MarqueeSelectionTests, ReleaseEndsTheMarqueeAndAnchorsTheRangeOnItsFirstItem)
{
    //! [GIVEN] A marquee that reaches two clips and a label
    dragBoxOverItems();
    ASSERT_TRUE(m_controller->marqueeActive());

    //! [EXPECT] The first item of the topmost track anchors later range selections and takes the focus,
    //! and the time selection is left alone
    const trackedit::TrackItemKey firstItem { 1, 11 };
    EXPECT_CALL(*m_selectionController, setItemSelectionAnchor(trackedit::secs_t(1.0), firstItem)).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(trackedit::TrackFocus::item(firstItem), _)).Times(1);
    EXPECT_CALL(*m_selectionController, setDataSelectedStartTime(_, _)).Times(0);
    EXPECT_CALL(*m_selectionController, setDataSelectedEndTime(_, _)).Times(0);

    //! [WHEN] The button is released
    m_controller->onReleased(20.0, 40.0);

    //! [THEN] The gesture and the box are over, the selection stays
    EXPECT_FALSE(m_controller->marqueeActive());
    EXPECT_FALSE(m_controller->selectionInProgress());
    EXPECT_EQ(m_controller->marqueeRect(), QRectF());
}

TEST_F(MarqueeSelectionTests, CancellingTheMarqueeDropsItAndItsSelection)
{
    //! [GIVEN] A marquee that reaches two clips and a label
    dragBoxOverItems();
    ASSERT_TRUE(m_controller->marqueeActive());

    //! [EXPECT] The items it selected are deselected again
    EXPECT_CALL(*m_selectionController, resetSelectedClips()).Times(1);
    EXPECT_CALL(*m_selectionController, resetSelectedLabels()).Times(1);

    //! [WHEN] The marquee is cancelled, as Escape does
    m_controller->cancelMarquee();

    //! [THEN] The marquee and the gesture carrying it are over
    EXPECT_FALSE(m_controller->marqueeActive());
    EXPECT_FALSE(m_controller->selectionInProgress());
}

TEST_F(MarqueeSelectionTests, CancellingWithoutAMarqueeLeavesTheGestureAlone)
{
    //! [GIVEN] A plain selection gesture without Ctrl
    ON_CALL(*m_application, keyboardModifiers())
    .WillByDefault(Return(Qt::NoModifier));
    m_controller->onPressed(1.0, 10.0);
    ASSERT_TRUE(m_controller->selectionInProgress());

    //! [WHEN] A marquee cancel arrives, as Escape sends one regardless
    m_controller->cancelMarquee();

    //! [THEN] The gesture is untouched
    EXPECT_TRUE(m_controller->selectionInProgress());
}

TEST_F(MarqueeSelectionTests, AnEmptyBoxLeavesFocusAndAnchorAlone)
{
    //! [GIVEN] A marquee over track area without items
    ON_CALL(*m_viewState, tracksInRange(10.0, 40.0))
    .WillByDefault(Return(trackedit::TrackIdList { 1, 2 }));
    m_controller->onPressed(1.0, 10.0);
    m_controller->onPositionChanged(20.0, 40.0);
    ASSERT_TRUE(m_controller->marqueeActive());

    //! [EXPECT] Nothing gets anchored or focused on release
    EXPECT_CALL(*m_selectionController, setItemSelectionAnchor(_, _)).Times(0);
    EXPECT_CALL(*m_trackNavigationController, setFocus(_, _)).Times(0);

    //! [WHEN] The button is released
    m_controller->onReleased(20.0, 40.0);

    //! [THEN] The marquee is over
    EXPECT_FALSE(m_controller->marqueeActive());
}
}
