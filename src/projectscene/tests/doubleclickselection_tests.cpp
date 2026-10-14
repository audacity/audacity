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
//! Two 50px high tracks sit at the top of the view: label track 1 spans 0..50px, audio track 2 spans 50..100px
class DoubleClickSelectionTests : public ::testing::Test
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
        .WillByDefault(Return(Qt::NoModifier));

        trackedit::Track labelTrack;
        labelTrack.id = LABEL_TRACK;
        labelTrack.type = trackedit::TrackType::Label;
        trackedit::Track audioTrack;
        audioTrack.id = AUDIO_TRACK;
        audioTrack.type = trackedit::TrackType::Mono;
        ON_CALL(*m_trackeditProject, track(LABEL_TRACK))
        .WillByDefault(Return(labelTrack));
        ON_CALL(*m_trackeditProject, track(AUDIO_TRACK))
        .WillByDefault(Return(audioTrack));

        ON_CALL(*m_viewState, trackAtPosition(LABEL_TRACK_Y))
        .WillByDefault(Return(LABEL_TRACK));
        ON_CALL(*m_viewState, trackAtPosition(AUDIO_TRACK_Y))
        .WillByDefault(Return(AUDIO_TRACK));
        ON_CALL(*m_viewState, tracksInRange(LABEL_TRACK_Y, LABEL_TRACK_Y))
        .WillByDefault(Return(trackedit::TrackIdList { LABEL_TRACK }));
        ON_CALL(*m_viewState, tracksInRange(LABEL_TRACK_Y, AUDIO_TRACK_Y))
        .WillByDefault(Return(trackedit::TrackIdList { LABEL_TRACK, AUDIO_TRACK }));

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

    static constexpr trackedit::TrackId LABEL_TRACK = 1;
    static constexpr trackedit::TrackId AUDIO_TRACK = 2;
    static constexpr double LABEL_TRACK_Y = 10.0;
    static constexpr double AUDIO_TRACK_Y = 70.0;

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

TEST_F(DoubleClickSelectionTests, DoubleClickSelectsTheAudioOfTheClickedTrackNotOfAnEarlierPress)
{
    //! [GIVEN] The last press that set the selection start was on the label track
    m_controller->onPressed(1.0, LABEL_TRACK_Y);
    m_controller->onReleased(1.0, LABEL_TRACK_Y);

    //! [EXPECT] Only the double-clicked audio track gets its audio selected
    EXPECT_CALL(*m_selectionController, setSelectedTrackAudioData(AUDIO_TRACK)).Times(1);
    EXPECT_CALL(*m_selectionController, setSelectedTrackAudioData(LABEL_TRACK)).Times(0);

    //! [WHEN] The audio track is double-clicked
    m_controller->selectTrackAudioData(AUDIO_TRACK_Y);
}

TEST_F(DoubleClickSelectionTests, HoveredItemOnAnotherTrackIsNotSelected)
{
    //! [GIVEN] The hovered item key still points at a label hovered earlier
    const TrackItemKey staleLabel(trackedit::TrackItemKey(LABEL_TRACK, 11));

    //! [EXPECT] Neither the label nor its track get selected
    EXPECT_CALL(*m_selectionController, setSelectedLabels(_, _)).Times(0);
    EXPECT_CALL(*m_selectionController, setSelectedClips(_, _)).Times(0);

    //! [WHEN] The audio track is double-clicked
    //! [THEN] The item is reported as not selected, so the caller selects the track's audio instead
    EXPECT_FALSE(m_controller->selectItemData(staleLabel, AUDIO_TRACK_Y));
}

TEST_F(DoubleClickSelectionTests, HoveredItemOnTheClickedTrackIsSelected)
{
    //! [GIVEN] A clip on the audio track is hovered
    const trackedit::TrackItemKey clip(AUDIO_TRACK, 21);

    //! [EXPECT] The clip gets selected
    EXPECT_CALL(*m_selectionController, setSelectedClips(trackedit::ClipKeyList { clip }, _)).Times(1);

    //! [WHEN] The clip is double-clicked
    //! [THEN] The item is reported as selected
    EXPECT_TRUE(m_controller->selectItemData(TrackItemKey(clip), AUDIO_TRACK_Y));
}
}
