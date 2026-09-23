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

TEST_F(Au3SelectionControllerTests, HistoryResyncPublishesClipsAndLabelsConsistently)
{
    //! [GIVEN] Two selected clips and two selected labels
    const TrackId waveTrackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);
    const TrackId labelTrackId = factory.addLabelTrackFromTemplate("Label Track", {
            { 0.0, 1.0, "First" },
            { 2.0, 3.0, "Second" }
        });
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(waveTrackId));
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(labelTrackId));
    ASSERT_NE(waveTrack, nullptr);
    ASSERT_NE(labelTrack, nullptr);

    const ClipKey firstClip { waveTrackId, waveTrack->GetClip(0)->GetId() };
    const ClipKey secondClip { waveTrackId, waveTrack->GetClip(1)->GetId() };
    const LabelKey firstLabel { labelTrackId, labelTrack->GetLabel(0)->GetId() };
    const LabelKey secondLabel { labelTrackId, labelTrack->GetLabel(1)->GetId() };
    m_selectionController->setSelectedClips({ firstClip, secondClip }, true);
    m_selectionController->setSelectedLabels({ firstLabel, secondLabel }, true);

    //! [GIVEN] A receiver that, like the timeline, reads both selections whenever either one changes
    int notifications = 0;
    const auto readBothSelections = [&]() {
        ++notifications;
        EXPECT_EQ(m_selectionController->selectedClips(), ClipKeyList { firstClip });
        EXPECT_EQ(m_selectionController->selectedLabels(), LabelKeyList { firstLabel });
        m_selectionController->leftMostSelectedItemStartTime();
        m_selectionController->rightMostSelectedItemEndTime();
    };
    m_selectionController->clipsSelected().onReceive(m_selectionController.get(), [&](const ClipKeyList&) {
        readBothSelections();
    });
    m_selectionController->labelsSelected().onReceive(m_selectionController.get(), [&](const LabelKeyList&) {
        readBothSelections();
    });

    //! [WHEN] Undo removes the second clip and label and the history reports the restored state
    waveTrack->RemoveInterval(waveTrack->GetClip(1));
    labelTrack->DeleteLabelById(secondLabel.itemId);
    m_historyChanged.send(HistoryEvent::RestoredState);

    //! [THEN] Both selections were published once, each seeing the other already restored
    EXPECT_EQ(notifications, 2);
    EXPECT_EQ(m_selectionController->selectedClips(), ClipKeyList { firstClip });
    EXPECT_EQ(m_selectionController->selectedLabels(), LabelKeyList { firstLabel });

    // Cleanup
    removeTrack(waveTrackId);
    removeTrack(labelTrackId);
}

TEST_F(Au3SelectionControllerTests, SetSelectedItemsSelectsBothKindsAndTheirTracksAtOnce)
{
    //! [GIVEN] A clip on a wave track and a label on a label track
    const TrackId waveTrackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);
    const TrackId labelTrackId = factory.addLabelTrackFromTemplate("Label Track", { { 0.0, 1.0, "Label" } });
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(waveTrackId));
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(labelTrackId));
    ASSERT_NE(waveTrack, nullptr);
    ASSERT_NE(labelTrack, nullptr);
    const ItemKeys items { { { waveTrackId, waveTrack->GetClip(0)->GetId() } }, { { labelTrackId, labelTrack->GetLabel(0)->GetId() } } };
    const TrackIdList tracks { waveTrackId, labelTrackId };

    //! [GIVEN] Every publication already sees all three selections in place
    int publications = 0;
    const auto expectAllSet = [&]() {
        ++publications;
        EXPECT_EQ(m_selectionController->selectedClips(), items.clips);
        EXPECT_EQ(m_selectionController->selectedLabels(), items.labels);
        EXPECT_EQ(m_selectionController->selectedTracks(), tracks);
    };
    m_selectionController->clipsSelected().onReceive(m_selectionController.get(), [&](const ClipKeyList&) { expectAllSet(); });
    m_selectionController->labelsSelected().onReceive(m_selectionController.get(), [&](const LabelKeyList&) { expectAllSet(); });
    m_selectionController->tracksSelected().onReceive(m_selectionController.get(), [&](const TrackIdList&) { expectAllSet(); });

    //! [WHEN] Both are selected at once
    m_selectionController->setSelectedItems(items, true);

    //! [THEN] Each selection was published once and the project flags match
    EXPECT_EQ(publications, 3);
    EXPECT_EQ(DomAccessor::findSelectedClips(projectRef()), items.clips);
    EXPECT_EQ(DomAccessor::findSelectedLabels(projectRef()), items.labels);
    EXPECT_EQ(DomAccessor::findSelectedTracks(projectRef()), tracks);

    // Cleanup
    removeTrack(waveTrackId);
    removeTrack(labelTrackId);
}

TEST_F(Au3SelectionControllerTests, DeselectingAllItemsDropsTheRangeAnchor)
{
    //! [GIVEN] A selected clip that anchors a range selection
    const TrackId trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    ASSERT_NE(track, nullptr);
    const ClipKey first { trackId, track->GetClip(0)->GetId() };
    const ClipKey second { trackId, track->GetClip(1)->GetId() };
    const auto spanOf = [track](const TrackItemKey& key) -> std::optional<TimeSpan> {
        const auto clip = DomAccessor::findWaveClip(track, key.itemId);
        return clip ? std::optional<TimeSpan>(TimeSpan(clip->GetPlayStartTime(), clip->GetPlayEndTime())) : std::nullopt;
    };
    ON_CALL(*m_trackEditProject, itemTimeSpan(::testing::_)).WillByDefault(spanOf);
    ON_CALL(*m_trackEditProject, itemTimeSpansSorted(trackId)).WillByDefault([&](const TrackId&) {
        return ItemTimeSpanList { { first, *spanOf(first) }, { second, *spanOf(second) } };
    });
    m_selectionController->setSelectedItems({ { first }, {} }, true);
    m_selectionController->setItemSelectionAnchor(0.0, first);
    ASSERT_FALSE(m_selectionController->itemKeysInRange(second).empty());

    //! [WHEN] Everything is deselected through the items setter
    m_selectionController->setSelectedItems({}, true);

    //! [THEN] A following range selection starts afresh instead of growing from the old anchor
    EXPECT_TRUE(m_selectionController->itemKeysInRange(second).empty());

    removeTrack(trackId);
}
}
