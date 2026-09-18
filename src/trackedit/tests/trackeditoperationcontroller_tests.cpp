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
    std::unique_ptr<Au3ProjectHistory> m_realHistory;
    std::unique_ptr<TrackeditOperationController> m_operation;
    ClipKey m_sourceClip;
    LabelKey m_sourceLabel;
    TrackId m_destinationClipTrack;
    TrackId m_destinationLabelTrack;
};

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
