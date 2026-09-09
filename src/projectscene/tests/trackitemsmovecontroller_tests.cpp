/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <QCoreApplication>

#include "global/async/async.h"
#include "projectscene/view/tracksitemsview/trackitemsmovecontroller.h"
#include "projectscene/view/tracksitemsview/tracklabelslistmodel.h"
#include "projectscene/internal/projectviewstate.h"
#include "trackedit/internal/au3/au3tracksinteraction.h"
#include "trackedit/internal/tracksviewrequestsservice.h"
#include "trackedit/tests/mocks/tracknavigationcontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "trackedit/tests/mocks/trackeditinteractionmock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/projecthistorymock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "playback/tests/mocks/playbackmock.h"
#include "mocks/audiooutputmock.h"
#include "snaptestaccess.h"

using namespace ::testing;

namespace au::projectscene {
namespace {
class DragViewState : public ProjectViewState
{
public:
    explicit DragViewState(const std::vector<trackedit::Track>& tracks)
        : ProjectViewState(muse::modularity::globalCtx()), m_tracks(tracks) {}

    trackedit::TrackId trackAtPosition(double y) const override
    {
        const int index = static_cast<int>((y + tracksVerticalOffset().val) / 100);
        return index >= 0 && index < static_cast<int>(m_tracks.size()) ? m_tracks[index].id : trackedit::INVALID_TRACK;
    }

    muse::ValCh<int> totalTrackHeight() const override { return { static_cast<int>(m_tracks.size()) * 100, {} }; }
    int trackDefaultHeight() const override { return 100; }
    void updateItemsBoundaries(bool, const trackedit::TrackItemKey&) override {}

private:
    const std::vector<trackedit::Track>& m_tracks;
};

class DragTracksInteraction : public trackedit::Au3TracksInteraction
{
public:
    DragTracksInteraction()
        : Au3TracksInteraction(muse::modularity::globalCtx()) {}
    MOCK_METHOD(trackedit::TrackId, addWaveTrack, (int), (override));
    MOCK_METHOD(void, removeDragAddedTracks, (size_t, bool), (override));
};

class DragLabelsModel : public TrackLabelsListModel
{
public:
    DragLabelsModel(const std::shared_ptr<context::IGlobalContext>& context,
                    const std::shared_ptr<trackedit::ISelectionController>& selection)
    {
        globalContext.set(context);
        selectionController.set(selection);
    }
};
}

class TrackItemsMoveControllerTests : public Test
{
protected:
    void SetUp() override
    {
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_project = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackeditProject = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();
        m_selection = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_interaction = std::make_shared<StrictMock<trackedit::TrackeditInteractionMock> >();
        m_history = std::make_shared<NiceMock<trackedit::ProjectHistoryMock> >();
        m_tracksInteraction = std::make_shared<NiceMock<DragTracksInteraction> >();
        m_viewState = std::make_shared<DragViewState>(m_tracks);
        m_playback = std::make_shared<NiceMock<playback::PlaybackMock> >();
        m_audioOutput = std::make_shared<NiceMock<playback::AudioOutputMock> >();
        m_navigation = std::make_shared<NiceMock<trackedit::TrackNavigationControllerMock> >();
        m_requests = std::make_shared<trackedit::TracksViewRequestsService>(muse::modularity::globalCtx());

        ON_CALL(*m_navigation, focusedItem()).WillByDefault([this] { return m_focusedItem; });
        ON_CALL(*m_history, historyChanged()).WillByDefault(Return(m_historyChanged));

        ON_CALL(*m_globalContext, currentProject()).WillByDefault(Return(m_project));
        ON_CALL(*m_globalContext, currentTrackeditProject()).WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_project, viewState()).WillByDefault(Return(m_viewState));
        ON_CALL(*m_playback, audioOutput()).WillByDefault(Return(m_audioOutput));
        ON_CALL(*m_audioOutput, sampleRate()).WillByDefault(Return(44100));
        ON_CALL(*m_selection, timeSelectionIsEmpty()).WillByDefault(Return(true));
        ON_CALL(*m_selection, selectedClipsInTrackOrder()).WillByDefault([this] { return m_selectedClips; });
        ON_CALL(*m_selection, selectedClips()).WillByDefault([this] { return m_selectedClips; });
        ON_CALL(*m_selection, selectedLabels()).WillByDefault([this] { return m_selectedLabels; });
        ON_CALL(*m_trackeditProject, trackList()).WillByDefault([this] { return m_tracks; });
        ON_CALL(*m_trackeditProject, track(_)).WillByDefault([this](trackedit::TrackId id) -> std::optional<trackedit::Track> {
            for (const auto& track : m_tracks) {
                if (track.id == id) {
                    return track;
                }
            }
            return {};
        });
        ON_CALL(*m_trackeditProject, label(_)).WillByDefault([this](const trackedit::LabelKey& key) {
            return key == m_label.key ? m_label : trackedit::Label {};
        });
        ON_CALL(*m_trackeditProject, clip(_)).WillByDefault([this](const trackedit::ClipKey& key) {
            return key == m_clip.key ? m_clip : trackedit::Clip {};
        });
        ON_CALL(*m_trackeditProject, labelList(_)).WillByDefault([this](trackedit::TrackId id) {
            muse::async::NotifyList<trackedit::Label> labels(std::make_shared<muse::async::ChangedNotify<trackedit::Label> >());
            if (id == m_label.key.trackId) {
                labels.push_back(m_label);
            }
            return labels;
        });

        m_context = std::make_unique<TimelineContext>();
        SnapTestAccess::wireContext(m_context.get(), m_globalContext, m_playback);
        m_controller = std::make_unique<TrackItemsMoveController>();
        m_controller->globalContext.set(m_globalContext);
        m_controller->selectionController.set(m_selection);
        m_controller->trackeditInteraction.set(m_interaction);
        m_controller->tracksInteraction.set(m_tracksInteraction);
        m_controller->projectHistory.set(m_history);
        m_controller->trackNavigationController.set(m_navigation);
        m_controller->tracksViewRequestsService.set(m_requests);
        m_controller->setTimelineContext(m_context.get());
        m_controller->init();
    }

    void TearDown() override
    {
        m_controller.reset();
        m_context.reset();
    }

    void selectLabel()
    {
        m_tracks = { { 1, {}, trackedit::TrackType::Label }, { 2, {}, trackedit::TrackType::Label } };
        m_label.key = { 1, 10 };
        m_label.startTime = 10.0;
        m_label.endTime = 15.0;
        m_selectedLabels = { m_label.key };
        movePointer(10.0, 50.0);
    }

    void selectClip()
    {
        m_tracks = { { 1, {}, trackedit::TrackType::Mono }, { 2, {}, trackedit::TrackType::Mono } };
        m_clip.key = { 1, 20 };
        m_clip.startTime = 10.0;
        m_clip.endTime = 15.0;
        m_selectedClips = { m_clip.key };
        movePointer(10.0, 50.0);
    }

    void movePointer(double time, double y)
    {
        m_context->updateMousePositionTime(time);
        m_viewState->setMousePositionY(y);
    }

    std::unique_ptr<DragLabelsModel> labelModel(trackedit::TrackId track)
    {
        auto model = std::make_unique<DragLabelsModel>(m_globalContext, m_selection);
        initLabelModel(*model, track);
        return model;
    }

    void initLabelModel(DragLabelsModel& model, trackedit::TrackId track)
    {
        model.tracksViewRequestsService.set(m_requests);
        model.setTrackId(QVariant::fromValue(track));
        model.setTimelineContext(m_context.get());
        model.setMoveController(m_controller.get());
        model.reload();
    }

    void expectFinished()
    {
        EXPECT_FALSE(m_controller->active());
        EXPECT_FALSE(m_controller->keyboardActive());
        EXPECT_FALSE(m_viewState->keyboardMoveActive().val);
        EXPECT_FALSE(m_controller->isDragged(m_label.key));
        EXPECT_FALSE(m_viewState->moveInitiated());
        EXPECT_DOUBLE_EQ(m_viewState->itemEditStartTimeOffset(), -1.0);
        EXPECT_TRUE(m_controller->itemsOnTrack(2).empty());
    }

    std::vector<trackedit::Track> m_tracks;
    trackedit::Label m_label;
    trackedit::Clip m_clip;
    trackedit::LabelKeyList m_selectedLabels;
    trackedit::ClipKeyList m_selectedClips;
    trackedit::TrackItemKey m_focusedItem;
    muse::async::Channel<trackedit::HistoryEvent> m_historyChanged;
    std::shared_ptr<NiceMock<trackedit::TrackNavigationControllerMock> > m_navigation;
    std::shared_ptr<trackedit::TracksViewRequestsService> m_requests;
    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_project;
    std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_trackeditProject;
    std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selection;
    std::shared_ptr<StrictMock<trackedit::TrackeditInteractionMock> > m_interaction;
    std::shared_ptr<NiceMock<trackedit::ProjectHistoryMock> > m_history;
    std::shared_ptr<NiceMock<DragTracksInteraction> > m_tracksInteraction;
    std::shared_ptr<DragViewState> m_viewState;
    std::shared_ptr<NiceMock<playback::PlaybackMock> > m_playback;
    std::shared_ptr<NiceMock<playback::AudioOutputMock> > m_audioOutput;
    std::unique_ptr<TimelineContext> m_context;
    std::unique_ptr<TrackItemsMoveController> m_controller;
};

TEST_F(TrackItemsMoveControllerTests, DropAfterSourceDelegateIsDestroyed)
{
    selectLabel();
    auto source = labelModel(1);
    m_controller->start(TrackItemKey(m_label.key));
    movePointer(20.0, 150.0);
    m_controller->update();
    source.reset();

    auto destination = labelModel(2);
    ASSERT_EQ(destination->rowCount({}), 1);
    const auto ghost = destination->data(destination->index(0), Qt::UserRole + 1).value<ViewTrackItem*>();
    ASSERT_NE(ghost, nullptr);
    EXPECT_TRUE(ghost->isDragGhost());
    EXPECT_DOUBLE_EQ(ghost->time().startTime, 20.0);

    const trackedit::LabelKeyList moved { { 2, 30 } };
    EXPECT_CALL(*m_interaction, moveLabels(m_selectedLabels, trackedit::secs_t(10.0), 1))
    .WillOnce(Return(muse::RetVal<trackedit::LabelKeyList>::make_ok(moved)));
    EXPECT_CALL(*m_selection, setSelectedLabels(moved, true));
    EXPECT_CALL(*m_history, endUserInteraction(false));
    EXPECT_EQ(m_controller->finish().key, moved.front());

    expectFinished();
    EXPECT_EQ(destination->rowCount({}), 0);
}

TEST_F(TrackItemsMoveControllerTests, CancelAfterSourceDelegateIsDestroyed)
{
    selectLabel();
    auto source = labelModel(1);
    m_controller->start(TrackItemKey(m_label.key));
    movePointer(20.0, 150.0);
    m_controller->update();
    source.reset();

    EXPECT_CALL(*m_tracksInteraction, removeDragAddedTracks(2, true));
    EXPECT_CALL(*m_history, endUserInteraction(false));
    EXPECT_TRUE(m_controller->cancel());
    expectFinished();
    EXPECT_DOUBLE_EQ(m_label.startTime, 10.0);
}

TEST_F(TrackItemsMoveControllerTests, HeaderClickEndsWithoutModifyingHistory)
{
    for (bool label : { false, true }) {
        m_selectedClips.clear();
        m_selectedLabels.clear();
        if (label) {
            selectLabel();
        } else {
            selectClip();
        }
        const TrackItemKey key(label ? m_label.key : m_clip.key);
        EXPECT_CALL(*m_history, startUserInteraction());
        EXPECT_CALL(*m_history, endUserInteraction(false));
        m_controller->start(key);
        EXPECT_EQ(m_controller->finish().key, key.key);
        expectFinished();
    }
}

TEST_F(TrackItemsMoveControllerTests, ClipsMoveOnlyOnceOnDrop)
{
    selectClip();
    m_controller->start(TrackItemKey(m_clip.key));
    movePointer(20.0, 150.0);
    m_controller->update();
    movePointer(25.0, 150.0);
    m_controller->update();

    const trackedit::ClipKeyList moved { { 2, 20 } };
    EXPECT_CALL(*m_interaction, moveClips(m_selectedClips, trackedit::secs_t(15.0), 1))
    .WillOnce(Return(muse::RetVal<trackedit::ClipKeyList>::make_ok(moved)));
    EXPECT_CALL(*m_selection, setSelectedClips(moved, true));
    m_controller->finish();
    expectFinished();
}

TEST_F(TrackItemsMoveControllerTests, CancelRemovesOnlyTracksAddedForPreview)
{
    selectClip();
    EXPECT_CALL(*m_tracksInteraction, addWaveTrack(1)).WillOnce([this](int) {
        m_tracks.push_back({ 3, {}, trackedit::TrackType::Mono });
        return 3;
    });
    m_controller->start(TrackItemKey(m_clip.key));
    movePointer(20.0, 250.0);
    m_controller->update();
    EXPECT_EQ(m_controller->itemsOnTrack(3), m_selectedClips);

    EXPECT_CALL(*m_tracksInteraction, removeDragAddedTracks(2, true));
    m_controller->cancel();
    expectFinished();
}

TEST_F(TrackItemsMoveControllerTests, MixedSelectionClampsTogetherAtZeroAndTopAudioTrack)
{
    selectClip();
    m_tracks.push_back({ 3, {}, trackedit::TrackType::Label });
    m_tracks.push_back({ 4, {}, trackedit::TrackType::Label });
    m_label.key = { 4, 10 };
    m_label.startTime = 5.0;
    m_label.endTime = 8.0;
    m_selectedLabels = { m_label.key };
    movePointer(5.0, 350.0);
    m_controller->start(TrackItemKey(m_label.key));
    movePointer(-10.0, 250.0);
    m_controller->update();

    EXPECT_DOUBLE_EQ(m_controller->timeOffset(), -5.0);
    EXPECT_EQ(m_controller->itemsOnTrack(1), m_selectedClips);
    EXPECT_EQ(m_controller->itemsOnTrack(4), m_selectedLabels);
    m_controller->cancel();
}

TEST_F(TrackItemsMoveControllerTests, LabelPreviewsClampIndividuallyAtBoundaryTracks)
{
    selectLabel();
    m_tracks = { { 1, {}, trackedit::TrackType::Label }, { 2, {}, trackedit::TrackType::Mono },
        { 3, {}, trackedit::TrackType::Label }, { 4, {}, trackedit::TrackType::Mono },
        { 5, {}, trackedit::TrackType::Label } };
    m_selectedLabels = { { 1, 10 }, { 3, 30 }, { 5, 50 } };
    ON_CALL(*m_trackeditProject, label(_)).WillByDefault([this](const trackedit::LabelKey& key) {
        auto label = m_label;
        label.key = key;
        return label;
    });

    for (int offset : { -1, 1 }) {
        movePointer(10.0, 250.0);
        m_controller->start(TrackItemKey(m_selectedLabels[1]));
        movePointer(10.0, offset < 0 ? 50.0 : 450.0);
        m_controller->update();

        const trackedit::LabelKeyList firstPair { { 1, 10 }, { 3, 30 } };
        const trackedit::LabelKeyList lastPair { { 3, 30 }, { 5, 50 } };
        const trackedit::LabelKeyList firstOnly { { 1, 10 } };
        const trackedit::LabelKeyList lastOnly { { 5, 50 } };
        EXPECT_EQ(m_controller->itemsOnTrack(1), offset < 0 ? firstPair : trackedit::LabelKeyList {});
        EXPECT_EQ(m_controller->itemsOnTrack(3), offset < 0 ? lastOnly : firstOnly);
        EXPECT_EQ(m_controller->itemsOnTrack(5), offset < 0 ? trackedit::LabelKeyList {} : lastPair);
        EXPECT_TRUE(m_controller->itemsOnTrack(2).empty());
        EXPECT_TRUE(m_controller->itemsOnTrack(4).empty());

        EXPECT_CALL(*m_interaction, moveLabels(m_selectedLabels, trackedit::secs_t(0.0), offset))
        .WillOnce(Return(muse::RetVal<trackedit::LabelKeyList>::make_ok(m_selectedLabels)));
        m_controller->finish();
        expectFinished();
    }
}

TEST_F(TrackItemsMoveControllerTests, KeyboardRepeatsPreviewOriginalItemsUntilModifiersAreReleased)
{
    selectClip();
    m_tracks.push_back({ 3, {}, trackedit::TrackType::Mono });
    m_focusedItem = m_clip.key;
    const auto original = m_selectedClips;
    EXPECT_CALL(*m_history, startUserInteraction()).Times(1);
    EXPECT_CALL(*m_history, endUserInteraction(false)).Times(1);
    EXPECT_CALL(*m_navigation, focusedItem()).Times(1);

    m_requests->requestItemMove(0.0, 1);
    EXPECT_TRUE(m_controller->keyboardActive());
    EXPECT_TRUE(m_viewState->keyboardMoveActive().val);
    EXPECT_EQ(m_controller->itemsOnTrack(2), original);

    m_focusedItem = { 2, trackedit::INVALID_TRACK_ITEM };
    m_selectedClips = { { 2, 20 } };
    for (int i = 0; i < 20; ++i) {
        m_requests->requestItemMove(0.0, 1);
        EXPECT_EQ(m_controller->itemsOnTrack(3), original);
        m_requests->requestItemMove(0.0, -1);
        EXPECT_EQ(m_controller->itemsOnTrack(2), original);
    }
    m_requests->requestItemMove(0.25, 0);
    m_requests->requestItemMove(0.25, 0);
    movePointer(100.0, 50.0);
    m_controller->update();
    EXPECT_DOUBLE_EQ(m_controller->timeOffset(), 0.5);
    EXPECT_EQ(m_controller->itemsOnTrack(2), original);

    const trackedit::ClipKeyList moved { { 2, 20 } };
    EXPECT_CALL(*m_interaction, moveClips(original, trackedit::secs_t(0.5), 1))
    .WillOnce(Return(muse::RetVal<trackedit::ClipKeyList>::make_ok(moved)));
    EXPECT_CALL(*m_selection, setSelectedClips(original, false));
    EXPECT_CALL(*m_selection, setSelectedClips(moved, true));
    EXPECT_CALL(*m_navigation, setFocusedItem(moved.front(), true));
    m_viewState->modifiersReleased().notify();
    expectFinished();
    m_viewState->modifiersReleased().notify();
}

TEST_F(TrackItemsMoveControllerTests, ReusedTrackModelDoesNotRunOldGhostCleanup)
{
    selectLabel();
    std::optional<DragLabelsModel> destination;
    destination.emplace(m_globalContext, m_selection);
    initLabelModel(*destination, 2);
    m_requests->requestItemMove(0.0, 1);
    ASSERT_EQ(destination->rowCount({}), 1);
    QPointer<ViewTrackItem> ghost = destination->data(destination->index(0), Qt::UserRole + 1).value<ViewTrackItem*>();
    ASSERT_NE(ghost, nullptr);

    m_requests->requestItemMove(0.0, -1);
    ASSERT_EQ(destination->rowCount({}), 0);
    EXPECT_FALSE(ghost.isNull());
    const auto address = &*destination;
    destination.reset();
    EXPECT_TRUE(ghost.isNull());

    destination.emplace(m_globalContext, m_selection);
    ASSERT_EQ(&*destination, address);
    initLabelModel(*destination, 2);
    muse::async::Async::call(&*destination, [] {});
    kors::async::QueuePool::instance()->processMessages();
    QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
    EXPECT_EQ(destination->rowCount({}), 0);
    m_controller->cancel();
}

TEST_F(TrackItemsMoveControllerTests, RepeatedKeyboardPreviewsDeleteOnlyRetiredGhosts)
{
    selectLabel();
    auto destination = labelModel(2);
    QList<QPointer<ViewTrackItem> > retired;
    for (int i = 0; i < 100; ++i) {
        m_requests->requestItemMove(0.0, 1);
        ASSERT_EQ(destination->rowCount({}), 1);
        QPointer<ViewTrackItem> ghost = destination->data(destination->index(0), Qt::UserRole + 1).value<ViewTrackItem*>();
        ASSERT_NE(ghost, nullptr);
        retired.append(ghost);
        m_requests->requestItemMove(0.0, -1);
        EXPECT_FALSE(ghost.isNull());
    }
    m_requests->requestItemMove(0.0, 1);
    QPointer<ViewTrackItem> current = destination->data(destination->index(0), Qt::UserRole + 1).value<ViewTrackItem*>();
    ASSERT_NE(current, nullptr);

    kors::async::QueuePool::instance()->processMessages();
    QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
    for (const auto& ghost : retired) {
        EXPECT_TRUE(ghost.isNull());
    }
    EXPECT_FALSE(current.isNull());
    EXPECT_EQ(destination->rowCount({}), 1);
    m_controller->cancel();
}

TEST_F(TrackItemsMoveControllerTests, KeyboardMovesUnselectedFocusedItemEvenWithTimeSelection)
{
    selectLabel();
    m_focusedItem = m_label.key;
    m_selectedLabels.clear();
    ON_CALL(*m_selection, timeSelectionIsEmpty()).WillByDefault(Return(false));
    m_requests->requestItemMove(0.5, 1);
    const trackedit::LabelKeyList original { m_label.key };
    EXPECT_EQ(m_controller->itemsOnTrack(2), original);
    EXPECT_TRUE(m_controller->isDragged(m_label.key));

    const trackedit::LabelKeyList moved { { 2, 10 } };
    EXPECT_CALL(*m_interaction, moveLabels(original, trackedit::secs_t(0.5), 1))
    .WillOnce(Return(muse::RetVal<trackedit::LabelKeyList>::make_ok(moved)));
    EXPECT_CALL(*m_navigation, setFocusedItem(moved.front(), true));
    m_viewState->modifiersReleased().notify();
    expectFinished();
}

TEST_F(TrackItemsMoveControllerTests, KeyboardIgnoresStaleFocusAndSelectionKeys)
{
    selectClip();
    m_focusedItem = { 2, m_clip.key.itemId };
    m_selectedClips.push_back(m_focusedItem);
    m_requests->requestItemMove(0.5, 0);

    const trackedit::ClipKeyList original { m_clip.key };
    EXPECT_EQ(m_controller->itemsOnTrack(1), original);
    EXPECT_CALL(*m_interaction, moveClips(original, trackedit::secs_t(0.5), 0))
    .WillOnce(Return(muse::RetVal<trackedit::ClipKeyList>::make_ok(original)));
    m_viewState->modifiersReleased().notify();
    expectFinished();
}

TEST_F(TrackItemsMoveControllerTests, KeyboardCreatesPreviewTracksAndCanCancel)
{
    selectClip();
    EXPECT_CALL(*m_tracksInteraction, addWaveTrack(1)).Times(2).WillRepeatedly([this](int) {
        m_tracks.push_back({ 3, {}, trackedit::TrackType::Mono });
        return 3;
    });
    EXPECT_CALL(*m_tracksInteraction, removeDragAddedTracks(2, true)).Times(2).WillRepeatedly([this](size_t count, bool) {
        m_tracks.resize(count);
    });

    m_requests->requestItemMove(0.0, 1);
    EXPECT_EQ(m_controller->itemsOnTrack(2), m_selectedClips);
    m_requests->requestItemMove(0.0, 1);
    EXPECT_EQ(m_controller->itemsOnTrack(3), m_selectedClips);
    EXPECT_EQ(m_tracks.size(), 3u);

    m_requests->requestItemMove(0.0, -1);
    EXPECT_EQ(m_controller->itemsOnTrack(2), m_selectedClips);
    EXPECT_EQ(m_tracks.size(), 2u);

    m_requests->requestItemMove(0.0, 1);
    EXPECT_EQ(m_controller->itemsOnTrack(3), m_selectedClips);
    EXPECT_EQ(m_tracks.size(), 3u);
    EXPECT_TRUE(m_controller->cancel());
    EXPECT_EQ(m_tracks.size(), 2u);
    m_viewState->modifiersReleased().notify();
    expectFinished();
}

TEST_F(TrackItemsMoveControllerTests, KeyboardCanReverseImmediatelyAtLabelTrackAndTimeBoundaries)
{
    selectLabel();
    m_requests->requestItemMove(-20.0, -1);
    EXPECT_DOUBLE_EQ(m_controller->timeOffset(), -10.0);
    EXPECT_EQ(m_controller->itemsOnTrack(1), m_selectedLabels);
    m_requests->requestItemMove(1.0, 1);
    EXPECT_DOUBLE_EQ(m_controller->timeOffset(), -9.0);
    EXPECT_EQ(m_controller->itemsOnTrack(2), m_selectedLabels);
    m_requests->requestItemMove(0.0, 1);
    m_requests->requestItemMove(0.0, -1);
    EXPECT_EQ(m_controller->itemsOnTrack(1), m_selectedLabels);
    m_controller->cancel();
}

TEST_F(TrackItemsMoveControllerTests, HistoryChangeCancelsKeyboardPreview)
{
    selectClip();
    m_requests->requestItemMove(0.5, 1);
    EXPECT_CALL(*m_tracksInteraction, removeDragAddedTracks(_, _)).Times(0);
    m_historyChanged.send(trackedit::HistoryEvent::RestoredState);
    m_viewState->modifiersReleased().notify();
    expectFinished();
}

TEST_F(TrackItemsMoveControllerTests, KeyboardReturnToStartDoesNotCommit)
{
    selectClip();
    m_requests->requestItemMove(0.5, 1);
    m_requests->requestItemMove(-0.5, -1);
    m_viewState->modifiersReleased().notify();
    expectFinished();
}

TEST_F(TrackItemsMoveControllerTests, KeyboardDoesNotInterruptMouseDrag)
{
    selectClip();
    m_controller->start(TrackItemKey(m_clip.key));
    m_requests->requestItemMove(0.5, 1);
    EXPECT_FALSE(m_controller->keyboardActive());
    EXPECT_DOUBLE_EQ(m_controller->timeOffset(), 0.0);
    m_viewState->modifiersReleased().notify();
    EXPECT_TRUE(m_controller->active());
    m_controller->cancel();
}

TEST_F(TrackItemsMoveControllerTests, KeyboardStillReordersFocusedTrack)
{
    selectClip();
    m_focusedItem = { 1, trackedit::INVALID_TRACK_ITEM };
    EXPECT_CALL(*m_interaction, moveTracks(trackedit::TrackIdList { 1 }, trackedit::TrackMoveDirection::Down));
    m_requests->requestItemMove(0.0, 1);
    m_requests->requestItemMove(0.5, 0);
    expectFinished();
}

TEST_F(TrackItemsMoveControllerTests, RangeMovesStayIncrementalWithoutGhosts)
{
    selectLabel();
    ON_CALL(*m_selection, timeSelectionIsEmpty()).WillByDefault(Return(false));
    EXPECT_CALL(*m_interaction, moveRangeSelection(trackedit::secs_t(10.0), false)).WillOnce([this](trackedit::secs_t offset, bool) {
        m_label.startTime += offset;
        m_label.endTime += offset;
        return true;
    });
    EXPECT_CALL(*m_interaction, moveRangeSelection(trackedit::secs_t(5.0), false)).WillOnce([this](trackedit::secs_t offset, bool) {
        m_label.startTime += offset;
        m_label.endTime += offset;
        return true;
    });
    EXPECT_CALL(*m_interaction, moveRangeSelection(trackedit::secs_t(0.0), false)).WillOnce(Return(true));
    EXPECT_CALL(*m_interaction, moveRangeSelection(trackedit::secs_t(0.0), true)).WillOnce(Return(true));
    m_controller->start(TrackItemKey(m_label.key));
    movePointer(20.0, 50.0);
    m_controller->update();
    movePointer(25.0, 50.0);
    m_controller->update();
    EXPECT_FALSE(m_controller->isDragged(m_label.key));
    EXPECT_TRUE(m_controller->itemsOnTrack(1).empty());
    m_controller->finish();
    expectFinished();
}
}
