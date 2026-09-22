/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/trackeditoperationcontroller.h"
#include "../internal/au3/au3clipsinteraction.h"
#include "../internal/au3/au3labelsinteraction.h"
#include "../internal/au3/au3projecthistory.h"
#include "../internal/au3/au3selectioncontroller.h"
#include "au3interactiontestbase.h"
#include "mocks/projecthistorymock.h"
#include "mocks/clipsinteractionmock.h"
#include "mocks/tracknavigationcontrollermock.h"
#include "mocks/trackeditconfigurationmock.h"
#include "interactive/tests/mocks/interactivemock.h"
#include "trackediterrors.h"
#include "spectrogram/internal/frequencyselectioncontroller.h"
#include "spectrogram/internal/au3/au3frequencyselectionrestorer.h"
#include "au3-label-track/LabelTrack.h"

using namespace ::testing;

namespace au::trackedit {
class TrackeditOperationMoveTests : public Au3InteractionTestBase, public WithParamInterface<bool>
{
public:
    void SetUp() override
    {
        m_testCtx = std::make_shared<muse::modularity::Context>(1001);
        const auto ctx = m_testCtx;
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackEditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();
        ON_CALL(*m_globalContext, currentProject()).WillByDefault(Return(m_currentProject));
        ON_CALL(*m_globalContext, currentTrackeditProject()).WillByDefault(Return(m_trackEditProject));
        initTestProject();

        m_selection = std::make_shared<Au3SelectionController>(ctx);
        m_history = std::make_shared<NiceMock<ProjectHistoryMock> >();
        m_realHistory = std::make_unique<Au3ProjectHistory>(ctx);
        ON_CALL(*m_history, historyChanged()).WillByDefault(Return(m_realHistory->historyChanged()));
        ON_CALL(*m_history, pushHistoryState(_, _)).WillByDefault([this](const std::string& description, const std::string& action) {
            m_realHistory->pushHistoryState(description, action);
        });
        m_clips = std::make_shared<Au3ClipsInteraction>(ctx);
        m_labels = std::make_shared<Au3LabelsInteraction>(ctx);
        m_operation = std::make_unique<TrackeditOperationController>(ctx, nullptr);
        auto* ioc = muse::modularity::ioc(ctx);
        ioc->registerExport<context::IGlobalContext>("utests", m_globalContext);
        ioc->registerExport<ISelectionController>("utests", m_selection);
        ioc->registerExport<IClipsInteraction>("utests", m_clips);
        ioc->registerExport<ILabelsInteraction>("utests", m_labels);
        ioc->registerExport<IProjectHistory>("utests", m_history);
        m_navigation = std::make_shared<NiceMock<TrackNavigationControllerMock> >();
        ioc->registerExport<ITrackNavigationController>("utests", m_navigation);
        m_configuration = std::make_shared<NiceMock<TrackeditConfigurationMock> >();
        muse::modularity::globalIoc()->registerExport<ITrackeditConfiguration>("utests", m_configuration);
        m_interactive = std::make_shared<NiceMock<muse::InteractiveMock> >();
        ioc->registerExport<muse::IInteractive>("utests", m_interactive);
        auto frequencyRestorer = std::make_unique<spectrogram::FrequencySelectionRestorer>(ctx);
        auto frequencySelection = std::make_shared<spectrogram::FrequencySelectionController>(ctx, std::move(frequencyRestorer));
        ioc->registerExport<spectrogram::IFrequencySelectionController>("utests", frequencySelection);

        TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);
        const auto source = factory.createTrackFromTemplate("source", { { 1.0, { { 0.1, TrackTemplateFactory::createNoise } } } });
        const auto sourceId = factory.addTrackToProject(source);
        m_destinationClipTrack = factory.addTrackFromTemplate("destination", {});
        m_sourceClip = { sourceId, source->GetSortedClipByIndex(0)->GetId() };
        auto& tracks = Au3TrackList::Get(projectRef());
        auto* sourceLabelTrack = ::LabelTrack::Create(tracks);
        m_destinationLabelTrack = ::LabelTrack::Create(tracks)->GetId();
        m_sourceLabel = { sourceLabelTrack->GetId(), sourceLabelTrack->AddLabel(SelectedRegion(2.0, 3.0), wxString()) };
        ON_CALL(*m_trackEditProject, label(_)).WillByDefault([this](const LabelKey& key) {
            auto* track = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(key.trackId));
            const auto* label = track ? DomAccessor::findLabel(track, key.itemId) : nullptr;
            return label ? DomConverter::label(track, label) : Label {};
        });

        muse::async::Notification projectChanged;
        ON_CALL(*m_globalContext, currentTrackeditProjectChanged()).WillByDefault(Return(projectChanged));
        m_selection->init();
        projectChanged.notify();

        if (GetParam()) {
            m_selection->setSelectedClips({ m_sourceClip }, true);
            m_selection->setSelectedLabels({ m_sourceLabel }, true);
        } else {
            m_selection->setSelectedLabels({ m_sourceLabel }, true);
            m_selection->setSelectedClips({ m_sourceClip }, true);
        }
        ON_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).WillByDefault([this](const Track&) { checkSelection(); });
        ON_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).WillByDefault([this](const Clip&) { checkSelection(); });
        ON_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).WillByDefault([this](const Clip&) { checkSelection(); });
        ON_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).WillByDefault([this](const Clip&) { checkSelection(); });
        m_realHistory->init();
    }

    void TearDown() override
    {
        Au3InteractionTestBase::TearDown();
        m_operation.reset();
        muse::modularity::globalIoc()->unregister<ITrackeditConfiguration>("utests");
        muse::modularity::removeIoC(m_testCtx);
    }

    void checkSelection()
    {
        for (const auto& key : m_selection->selectedClips()) {
            auto* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(key.trackId));
            ASSERT_NE(track, nullptr);
            EXPECT_NE(DomAccessor::findWaveClip(track, key.itemId), nullptr);
        }
        for (const auto& key : m_selection->selectedLabels()) {
            auto* track = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(key.trackId));
            ASSERT_NE(track, nullptr);
            EXPECT_NE(DomAccessor::findLabel(track, key.itemId), nullptr);
        }
    }

    muse::RetVal<TrackItemKeyList> move(secs_t offset, int tracks)
    {
        return GetParam() ? m_operation->moveLabels(m_selection->selectedLabels(), offset, tracks)
               : m_operation->moveClips(m_selection->selectedClips(), offset, tracks);
    }

    muse::modularity::ContextPtr m_testCtx;
    std::shared_ptr<Au3SelectionController> m_selection;
    std::shared_ptr<Au3ClipsInteraction> m_clips;
    std::shared_ptr<Au3LabelsInteraction> m_labels;
    std::shared_ptr<ProjectHistoryMock> m_history;
    std::shared_ptr<TrackNavigationControllerMock> m_navigation;
    std::shared_ptr<TrackeditConfigurationMock> m_configuration;
    std::shared_ptr<muse::InteractiveMock> m_interactive;
    std::unique_ptr<Au3ProjectHistory> m_realHistory;
    std::unique_ptr<TrackeditOperationController> m_operation;
    ClipKey m_sourceClip;
    LabelKey m_sourceLabel;
    TrackId m_destinationClipTrack;
    TrackId m_destinationLabelTrack;
};

TEST_P(TrackeditOperationMoveTests, GroupItemsJoinsClipAndLabelUnderOneId)
{
    //! [GIVEN] The project resolves track types and hands out group id 7
    ON_CALL(*m_trackEditProject, track(_)).WillByDefault([this](TrackId id) -> std::optional<Track> {
        const auto* track = DomAccessor::findTrack(projectRef(), Au3TrackId(id));
        return track ? std::optional<Track>(DomConverter::track(track)) : std::nullopt;
    });
    ON_CALL(*m_trackEditProject, createNewGroupID(_)).WillByDefault(Return(int64_t(7)));

    //! [WHEN] The clip and the label are grouped
    m_operation->groupItems({ m_sourceClip, m_sourceLabel });

    //! [THEN] Both share the id and the group lists each of them
    EXPECT_EQ(m_operation->itemGroupId(m_sourceClip), 7);
    EXPECT_EQ(m_operation->itemGroupId(m_sourceLabel), 7);
    const ItemKeys group = m_operation->itemsInGroup(7);
    EXPECT_EQ(group.clips, ClipKeyList { m_sourceClip });
    EXPECT_EQ(group.labels, LabelKeyList { m_sourceLabel });

    //! [WHEN] The label is ungrouped on its own
    m_operation->ungroupItems({ m_sourceLabel });

    //! [THEN] It leaves the group and the clip stays
    EXPECT_EQ(m_operation->itemGroupId(m_sourceLabel), -1);
    EXPECT_EQ(m_operation->itemGroupId(m_sourceClip), 7);
    EXPECT_TRUE(m_operation->itemsInGroup(7).labels.empty());
}

TEST_P(TrackeditOperationMoveTests, MixedMoveUpdatesBothSelectionsBeforeHistory)
{
    EXPECT_CALL(*m_history, pushHistoryState(_, _)).WillOnce([this](const std::string& description, const std::string& action) {
        EXPECT_EQ(description, "Items moved");
        EXPECT_EQ(action, "Move items");
        checkSelection();
        ASSERT_EQ(m_selection->selectedClips().size(), 1u);
        ASSERT_EQ(m_selection->selectedLabels().size(), 1u);
        EXPECT_EQ(m_selection->selectedClips().front().trackId, m_destinationClipTrack);
        EXPECT_EQ(m_selection->selectedLabels().front().trackId, m_destinationLabelTrack);
        EXPECT_EQ(DomAccessor::findSelectedClips(projectRef()), m_selection->selectedClips());
        EXPECT_EQ(DomAccessor::findSelectedLabels(projectRef()), m_selection->selectedLabels());
        m_realHistory->pushHistoryState(description, action);
    });

    const auto result = move(0.5, 1);
    ASSERT_TRUE(result.ret);
    EXPECT_EQ(result.val, GetParam() ? m_selection->selectedLabels() : m_selection->selectedClips());
    checkSelection();
    EXPECT_EQ(m_selection->leftMostSelectedClipStartTime(), 1.5);
    EXPECT_EQ(m_selection->leftMostSelectedLabelStartTime(), 2.5);
    EXPECT_EQ(m_selection->selectedTracks(), (TrackIdList { m_destinationClipTrack, m_destinationLabelTrack }));

    const auto movedClips = m_selection->selectedClips();
    ASSERT_TRUE(m_realHistory->undoAvailable());
    m_realHistory->undo();
    checkSelection();
    EXPECT_EQ(DomAccessor::findSelectedClips(projectRef()), (ClipKeyList { m_sourceClip }));
    ASSERT_EQ(m_selection->selectedLabels().size(), 1u);
    EXPECT_EQ(m_selection->selectedLabels().front().trackId, m_sourceLabel.trackId);
    EXPECT_EQ(m_selection->leftMostSelectedLabelStartTime(), 2.0);
    EXPECT_EQ(DomAccessor::findSelectedLabels(projectRef()), m_selection->selectedLabels());
    ASSERT_TRUE(m_realHistory->redoAvailable());
    m_realHistory->redo();
    checkSelection();
    EXPECT_EQ(DomAccessor::findSelectedClips(projectRef()), movedClips);
    ASSERT_EQ(m_selection->selectedLabels().size(), 1u);
    EXPECT_EQ(m_selection->selectedLabels().front().trackId, m_destinationLabelTrack);
    EXPECT_EQ(m_selection->leftMostSelectedLabelStartTime(), 2.5);
    EXPECT_EQ(DomAccessor::findSelectedLabels(projectRef()), m_selection->selectedLabels());
}

TEST_P(TrackeditOperationMoveTests, MixedMoveDropsFocusFromMovedItemBeforeAnythingIsPublished)
{
    //! [GIVEN] The focus sits on the item that is about to change tracks
    ON_CALL(*m_navigation, focus()).WillByDefault(Return(TrackFocus::item(GetParam() ? m_sourceLabel : m_sourceClip)));
    bool focusDropped = false;
    EXPECT_CALL(*m_navigation, setFocus(_, _)).WillOnce([&focusDropped](const TrackFocus& focus, bool) {
        EXPECT_TRUE(focus.isTrack());
        focusDropped = true;
    });

    //! [GIVEN] Nothing observes the move while the focus still names the source key
    const auto expectFocusDropped = [&focusDropped]() {
        EXPECT_TRUE(focusDropped);
    };
    ON_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).WillByDefault([&](const Track&) { expectFocusDropped(); });
    ON_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).WillByDefault([&](const Clip&) { expectFocusDropped(); });
    m_selection->clipsSelected().onReceive(m_operation.get(), [&](const ClipKeyList&) { expectFocusDropped(); });
    m_selection->labelsSelected().onReceive(m_operation.get(), [&](const LabelKeyList&) { expectFocusDropped(); });

    //! [WHEN] The mixed selection is moved one track down
    const auto result = move(0.5, 1);
    ASSERT_TRUE(result.ret);
    EXPECT_TRUE(focusDropped);
}

TEST_P(TrackeditOperationMoveTests, MixedMoveLeavesFocusOnUnmovedItem)
{
    //! [GIVEN] The focus sits on the track panel, not on a moved item
    ON_CALL(*m_navigation, focus()).WillByDefault(Return(TrackFocus::track(m_sourceClip.trackId)));
    EXPECT_CALL(*m_navigation, setFocus(_, _)).Times(0);

    //! [WHEN] The mixed selection is moved one track down
    const auto result = move(0.5, 1);
    ASSERT_TRUE(result.ret);
}

TEST_P(TrackeditOperationMoveTests, MixedMoveUpWithMonoMixdownPublishesExistingClips)
{
    //! [GIVEN] A mono track holding a clip above a stereo track holding two grouped clips, and two grouped labels
    TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);
    const auto upper = factory.createTrackFromTemplate("upper", { { 3.0, { { 0.1, TrackTemplateFactory::createNoise } } } });
    auto lower = factory.createTrackFromTemplate("lower", {
            { 0.0, { { 0.1, TrackTemplateFactory::createNoise } } },
            { 1.0, { { 0.1, TrackTemplateFactory::createNoise } } }
        });
    lower = lower->MonoToStereo();
    const TrackId upperId = factory.addTrackToProject(upper);
    const TrackId lowerId = factory.addTrackToProject(lower);
    const ClipKeyList clips {
        { lowerId, lower->GetSortedClipByIndex(0)->GetId() },
        { lowerId, lower->GetSortedClipByIndex(1)->GetId() }
    };
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(m_sourceLabel.trackId));
    const LabelKey secondLabel { m_sourceLabel.trackId, labelTrack->AddLabel(SelectedRegion(4.0, 5.0), wxString()) };
    ON_CALL(*m_trackEditProject, track(_)).WillByDefault([this](TrackId id) -> std::optional<Track> {
        const auto* track = DomAccessor::findTrack(projectRef(), Au3TrackId(id));
        return track ? std::optional<Track>(DomConverter::track(track)) : std::nullopt;
    });
    ON_CALL(*m_trackEditProject, createNewGroupID(_)).WillByDefault(Return(int64_t(7)));
    m_operation->groupItems({ clips.at(0), clips.at(1), m_sourceLabel, secondLabel });
    m_selection->setSelectedClips(clips, true);
    m_selection->setSelectedLabels({ m_sourceLabel, secondLabel }, true);
    ON_CALL(*m_navigation, focus()).WillByDefault(Return(TrackFocus::item(clips.front())));

    //! [GIVEN] Every published clip key must name an existing clip
    int publications = 0;
    m_selection->clipsSelected().onReceive(m_operation.get(), [&](const ClipKeyList& keys) {
        ++publications;
        for (const ClipKey& key : keys) {
            auto* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(key.trackId));
            ASSERT_NE(track, nullptr) << "track " << key.trackId;
            EXPECT_NE(DomAccessor::findWaveClip(track, key.itemId), nullptr) << "clip " << key.itemId << " on track " << key.trackId;
        }
    });

    //! [WHEN] The group is moved up by one track, which mixes the clips down to mono
    const auto result = m_operation->moveClips(clips, 0.2, -1);
    ASSERT_TRUE(result.ret);

    //! [THEN] The clips landed on the upper track and stayed grouped with the labels
    ASSERT_EQ(result.val.size(), 2u);
    for (const ClipKey& key : result.val) {
        EXPECT_EQ(key.trackId, upperId);
        EXPECT_EQ(m_operation->itemGroupId(key), m_operation->itemGroupId(m_sourceLabel));
    }
    EXPECT_GE(publications, 1);
    checkSelection();
}

TEST_P(TrackeditOperationMoveTests, MoveIgnoresDragCancelArrivingWhileAskingAboutMixdown)
{
    //! [GIVEN] A mono track holding a clip above a stereo track holding two clips, saved as the drag's starting state
    TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);
    const auto upper = factory.createTrackFromTemplate("upper", { { 3.0, { { 0.1, TrackTemplateFactory::createNoise } } } });
    auto lower = factory.createTrackFromTemplate("lower", {
            { 0.0, { { 0.1, TrackTemplateFactory::createNoise } } },
            { 1.0, { { 0.1, TrackTemplateFactory::createNoise } } }
        });
    lower = lower->MonoToStereo();
    const TrackId upperId = factory.addTrackToProject(upper);
    const TrackId lowerId = factory.addTrackToProject(lower);
    const ClipKeyList clips {
        { lowerId, lower->GetSortedClipByIndex(0)->GetId() },
        { lowerId, lower->GetSortedClipByIndex(1)->GetId() }
    };
    m_realHistory->modifyState(false);
    ON_CALL(*m_history, interactionOngoing()).WillByDefault(Return(true));
    ON_CALL(*m_history, rollbackState()).WillByDefault([this]() { m_realHistory->rollbackState(); });
    m_selection->setSelectedClips(clips, true);
    m_selection->setSelectedLabels({}, true);

    //! [GIVEN] Opening the mixdown question cancels the drag's mouse grab, which the view answers with a drag-edit cancel
    ON_CALL(*m_configuration, askBeforeConvertingToMonoOrStereo()).WillByDefault(Return(true));
    ON_CALL(*m_interactive, buttonData(_)).WillByDefault([](muse::IInteractive::Button button) {
        return muse::IInteractive::ButtonData(static_cast<int>(button), "");
    });
    ON_CALL(*m_interactive, warningSync(_, _, _, _, _, _)).WillByDefault([this](auto&&...) {
        m_operation->cancelItemDragEdit();
        return muse::IInteractive::Result(static_cast<int>(muse::IInteractive::Button::Yes), true);
    });

    //! [GIVEN] Every published clip key must name an existing clip
    m_selection->clipsSelected().onReceive(m_operation.get(), [&](const ClipKeyList& keys) {
        for (const ClipKey& key : keys) {
            auto* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(key.trackId));
            ASSERT_NE(track, nullptr) << "track " << key.trackId;
            EXPECT_NE(DomAccessor::findWaveClip(track, key.itemId), nullptr) << "clip " << key.itemId;
        }
    });

    //! [WHEN] The clips are moved up, which asks about the mixdown while the move is under way
    const auto result = m_operation->moveClips(clips, 0.2, -1);
    ASSERT_TRUE(result.ret);

    //! [THEN] The stray cancel changed nothing and the clips landed on the upper track
    const Au3WaveTrack* upperTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(upperId));
    ASSERT_NE(upperTrack, nullptr);
    EXPECT_EQ(upperTrack->NIntervals(), 3u);
    for (const ClipKey& key : result.val) {
        EXPECT_EQ(key.trackId, upperId);
    }
    checkSelection();
}

TEST_P(TrackeditOperationMoveTests, MixedHorizontalMoveClampsBothTypesTogether)
{
    EXPECT_CALL(*m_history, pushHistoryState(_, _)).Times(1);

    const auto result = move(-10.0, 0);

    ASSERT_TRUE(result.ret);
    checkSelection();
    EXPECT_EQ(m_selection->leftMostSelectedClipStartTime(), 0.0);
    EXPECT_EQ(m_selection->leftMostSelectedLabelStartTime(), 1.0);
}

TEST_P(TrackeditOperationMoveTests, ClipMoveFailureRollsBackLabelsAndBothSelections)
{
    const auto originalTracks = m_selection->selectedTracks();
    auto failedClips = std::make_shared<NiceMock<ClipsInteractionMock> >();
    auto* ioc = muse::modularity::ioc(m_testCtx);
    ioc->unregister<IClipsInteraction>("utests");
    ioc->registerExport<IClipsInteraction>("utests", failedClips);
    ON_CALL(*failedClips, clipStartTime(_)).WillByDefault(Return(1.0));
    EXPECT_CALL(*failedClips, moveClips(_, _, _))
    .WillOnce(Return(muse::RetVal<ClipKeyList>::make_ret(make_ret(Err::DownmixingIsNotAllowed))));
    EXPECT_CALL(*m_history, pushHistoryState(_, _)).Times(0);
    EXPECT_CALL(*m_history, rollbackState()).WillOnce([this] { m_realHistory->rollbackState(); });
    EXPECT_CALL(*m_trackEditProject, reload()).WillOnce([this] { checkSelection(); });

    const auto result = move(0.5, 1);

    EXPECT_EQ(result.ret.code(), static_cast<int>(Err::DownmixingIsNotAllowed));
    EXPECT_EQ(m_selection->selectedClips(), (ClipKeyList { m_sourceClip }));
    ASSERT_EQ(m_selection->selectedLabels().size(), 1u);
    EXPECT_EQ(m_selection->selectedLabels().front().trackId, m_sourceLabel.trackId);
    EXPECT_EQ(DomAccessor::findSelectedLabels(projectRef()), m_selection->selectedLabels());
    EXPECT_EQ(m_selection->selectedTracks(), originalTracks);
    checkSelection();
    EXPECT_EQ(m_selection->leftMostSelectedClipStartTime(), 1.0);
    EXPECT_EQ(m_selection->leftMostSelectedLabelStartTime(), 2.0);
}

INSTANTIATE_TEST_SUITE_P(SelectionOrder, TrackeditOperationMoveTests, Bool());
}
