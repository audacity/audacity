/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/au3/au3selectioncontroller.h"

#include "au3interactiontestbase.h"
#include "mocks/projecthistorymock.h"
#include "spectrogram/tests/mocks/frequencyselectioncontrollermock.h"

#include "au3-label-track/LabelTrack.h"
#include "au3wrap/internal/domaccessor.h"

namespace au::trackedit {
class Au3SelectionControllerTests : public Au3InteractionTestBase
{
public:
    void SetUp() override
    {
        m_selectionController = std::make_shared<Au3SelectionController>(muse::modularity::globalCtx());
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackEditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();
        m_projectHistory = std::make_shared<NiceMock<ProjectHistoryMock> >();
        m_frequencySelectionController = std::make_shared<NiceMock<spectrogram::FrequencySelectionControllerMock> >();

        m_selectionController->globalContext.set(m_globalContext);
        m_selectionController->projectHistory.set(m_projectHistory);
        m_selectionController->frequencySelectionController.set(m_frequencySelectionController);

        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackEditProject));
        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));
        ON_CALL(*m_globalContext, playbackState())
        .WillByDefault(Return(m_playbackState));
        ON_CALL(*m_globalContext, currentTrackeditProjectChanged())
        .WillByDefault(Return(m_projectChanged));
        ON_CALL(*m_currentProject, trackeditProject())
        .WillByDefault(Return(m_trackEditProject));
        ON_CALL(*m_projectHistory, historyChanged())
        .WillByDefault(Return(m_historyChanged));
        ON_CALL(*m_frequencySelectionController, frequencySelectionChanged())
        .WillByDefault(Return(m_frequencySelectionChanged));

        initTestProject();

        m_selectionController->init();
        m_projectChanged.notify();
    }

    std::shared_ptr<Au3SelectionController> m_selectionController;
    std::shared_ptr<ProjectHistoryMock> m_projectHistory;
    std::shared_ptr<spectrogram::FrequencySelectionControllerMock> m_frequencySelectionController;

    muse::async::Notification m_projectChanged;
    muse::async::Channel<HistoryEvent> m_historyChanged;
    muse::async::Channel<bool> m_frequencySelectionChanged;
};

TEST_F(Au3SelectionControllerTests, ItemsTouchingRangeReturnsEveryItemTheRangeReaches)
{
    //! [GIVEN] Two clips on a wave track and a range, a point and a far label on a label track
    const TrackId waveTrackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);
    const TrackId labelTrackId = factory.addLabelTrackFromTemplate("Label Track", {
            { 0.0, 8 * SAMPLE_INTERVAL, "Range" },
            { 22 * SAMPLE_INTERVAL, 22 * SAMPLE_INTERVAL, "Point" },
            { 40 * SAMPLE_INTERVAL, 50 * SAMPLE_INTERVAL, "Beyond" }
        });
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(waveTrackId));
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(labelTrackId));
    ASSERT_NE(waveTrack, nullptr);
    ASSERT_NE(labelTrack, nullptr);

    //! [WHEN] The range only partially overlaps both clips and the range label
    const ItemKeys items = m_selectionController->itemsTouchingRange({ waveTrackId, labelTrackId },
                                                                     5 * SAMPLE_INTERVAL, 25 * SAMPLE_INTERVAL);

    //! [THEN] Every item the range reaches is returned, the far label is not
    const ClipKeyList expectedClips {
        { waveTrackId, waveTrack->GetClip(0)->GetId() },
        { waveTrackId, waveTrack->GetClip(1)->GetId() }
    };
    const LabelKeyList expectedLabels {
        { labelTrackId, labelTrack->GetLabel(0)->GetId() },
        { labelTrackId, labelTrack->GetLabel(1)->GetId() }
    };
    EXPECT_EQ(items.clips, expectedClips);
    EXPECT_EQ(items.labels, expectedLabels);

    // Cleanup
    removeTrack(waveTrackId);
    removeTrack(labelTrackId);
}

TEST_F(Au3SelectionControllerTests, ItemsTouchingRangeSkipsTracksOutsideTheList)
{
    //! [GIVEN] Two wave tracks with clips in the same time range
    const TrackId firstTrackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    const TrackId secondTrackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    Au3WaveTrack* secondTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(secondTrackId));
    ASSERT_NE(secondTrack, nullptr);

    //! [WHEN] A range covering both tracks' clips is queried for the second track only
    const ItemKeys items = m_selectionController->itemsTouchingRange({ secondTrackId }, 0.0, 40 * SAMPLE_INTERVAL);

    //! [THEN] Only the second track's clips are returned
    const ClipKeyList expectedClips {
        { secondTrackId, secondTrack->GetClip(0)->GetId() },
        { secondTrackId, secondTrack->GetClip(1)->GetId() },
        { secondTrackId, secondTrack->GetClip(2)->GetId() }
    };
    EXPECT_EQ(items.clips, expectedClips);
    EXPECT_TRUE(items.labels.empty());

    // Cleanup
    removeTrack(firstTrackId);
    removeTrack(secondTrackId);
}

TEST_F(Au3SelectionControllerTests, SelectionBoxFromAnchorReachesTheAnchorItemEndWhenPulledLeft)
{
    //! [GIVEN] Two clips on a wave track and a label next to each on a label track
    const TrackId waveTrackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);
    const TrackId labelTrackId = factory.addLabelTrackFromTemplate("Label Track", {
            { 5 * SAMPLE_INTERVAL, 8 * SAMPLE_INTERVAL, "First" },
            { 25 * SAMPLE_INTERVAL, 28 * SAMPLE_INTERVAL, "Second" }
        });
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(waveTrackId));
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(labelTrackId));
    ASSERT_NE(waveTrack, nullptr);
    ASSERT_NE(labelTrack, nullptr);
    const ClipKey firstClip { waveTrackId, waveTrack->GetClip(0)->GetId() };
    const ClipKey secondClip { waveTrackId, waveTrack->GetClip(1)->GetId() };
    const LabelKey firstLabel { labelTrackId, labelTrack->GetLabel(0)->GetId() };
    const LabelKey secondLabel { labelTrackId, labelTrack->GetLabel(1)->GetId() };

    //! [GIVEN] The second clip was clicked at its start and anchors the box
    ON_CALL(*m_trackEditProject, itemTimeSpan(secondClip))
    .WillByDefault(Return(TimeSpan(TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END)));
    m_selectionController->setItemSelectionAnchor(TRACK_TWO_CLIPS_CLIP2_START, secondClip);

    //! [WHEN] The box is pulled right onto the label track, short of the second clip's end
    const ItemKeys rightwards = m_selectionController->itemsTouchingSelectionBox(26 * SAMPLE_INTERVAL, labelTrackId);

    //! [THEN] It reaches the anchor clip and the label beside it
    EXPECT_EQ(rightwards.clips, ClipKeyList { secondClip });
    EXPECT_EQ(rightwards.labels, LabelKeyList { secondLabel });

    //! [WHEN] The box is pulled left onto the label track instead
    const ItemKeys leftwards = m_selectionController->itemsTouchingSelectionBox(2 * SAMPLE_INTERVAL, labelTrackId);

    //! [THEN] It still ends at the anchor clip's end, so everything on both tracks is reached
    const ClipKeyList expectedClips { firstClip, secondClip };
    const LabelKeyList expectedLabels { firstLabel, secondLabel };
    EXPECT_EQ(leftwards.clips, expectedClips);
    EXPECT_EQ(leftwards.labels, expectedLabels);

    // Cleanup
    removeTrack(waveTrackId);
    removeTrack(labelTrackId);
}

TEST_F(Au3SelectionControllerTests, ItemsTouchingRangeIsEmptyInAGap)
{
    //! [GIVEN] A wave track with a gap between its two clips
    const TrackId waveTrackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);

    //! [WHEN] The range lies entirely in the gap
    const ItemKeys items = m_selectionController->itemsTouchingRange({ waveTrackId }, 12 * SAMPLE_INTERVAL, 18 * SAMPLE_INTERVAL);

    //! [THEN] Nothing is returned
    EXPECT_TRUE(items.empty());

    // Cleanup
    removeTrack(waveTrackId);
}
}
