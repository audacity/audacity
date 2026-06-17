/*
 * Audacity: A Digital Audio Editor
 */
#include "../internal/projectviewstate.h"

#include "mocks/projectsceneconfigurationmock.h"

#include "au3wrap/tests/mocks/au3projectmock.h"
#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/domaccessor.h"

#include "playback/tests/mocks/playbackconfigurationmock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/projecthistorymock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "trackedit/trackedittypes.h"
#include "trackedit/dom/clip.h"
#include "trackedit/dom/label.h"

#include <gtest/gtest.h>

#include <set>

using namespace ::testing;
using ::testing::_;

namespace au::projectscene {
class ProjectViewStateTests : public ::testing::Test
{
public:

    void SetUp() override
    {
        m_projectViewState = std::make_shared<ProjectViewState>(muse::modularity::globalCtx());

        m_projectViewState->configuration.set(m_configurationMock);
        m_projectViewState->playbackConfiguration.set(m_playbackConfigurationMock);
        m_projectViewState->globalContext.set(m_globalContextMock);
        m_projectViewState->selectionController.set(m_selectionControllerMock);
        m_projectViewState->projectHistory.set(m_projectHistoryMock);

        ON_CALL(*m_globalContextMock, currentTrackeditProjectChanged())
        .WillByDefault(Return(muse::async::Notification()));

        ON_CALL(*m_projectHistoryMock, historyChanged())
        .WillByDefault(Return(muse::async::Channel<trackedit::HistoryEvent>()));

        ON_CALL(*m_globalContextMock, currentProject())
        .WillByDefault(Return(m_currentProject));

        ON_CALL(*m_globalContextMock, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProjectMock));

        initTestProject();

        m_projectViewState->init(m_au3ProjectMock);
    }

    void TearDown() override
    {
        m_au3ProjectAccessor->close();
    }

    trackedit::TrackIdList getAllTracks()
    {
        const auto au3ProjectPtr = reinterpret_cast<au3::Au3Project*>(m_au3ProjectAccessor->au3ProjectPtr());
        trackedit::TrackIdList tracks;
        size_t trackIndex = 0;
        while (true) {
            const auto track = au3::DomAccessor::findTrackByIndex(*au3ProjectPtr, trackIndex);
            if (track == nullptr) {
                break;
            }
            tracks.push_back(track->GetId());
            ++trackIndex;
        }
        return tracks;
    }

    void initTestProject()
    {
        const muse::io::path_t TEST_PROJECT_PATH = muse::String::fromUtf8(projectscene_tests_DATA_ROOT) + "/data/test.aup4";
        constexpr auto discardAutosave = false;
        muse::Ret ret = m_au3ProjectAccessor->load(TEST_PROJECT_PATH, discardAutosave);

        ON_CALL(*m_au3ProjectMock, au3ProjectPtr())
        .WillByDefault(Return(m_au3ProjectAccessor->au3ProjectPtr()));

        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(m_au3ProjectAccessor->au3ProjectPtr()));

        ON_CALL(*m_trackeditProjectMock, trackIdList())
        .WillByDefault(Return(getAllTracks()));
    }

    static trackedit::Clip makeClip(trackedit::TrackId trackId, trackedit::TrackItemId itemId, double start, double end)
    {
        trackedit::Clip clip;
        clip.key = { trackId, itemId };
        clip.startTime = start;
        clip.endTime = end;
        return clip;
    }

    static trackedit::Label makeLabel(trackedit::TrackId trackId, trackedit::TrackItemId itemId, double start, double end)
    {
        trackedit::Label label;
        label.key = { trackId, itemId };
        label.startTime = start;
        label.endTime = end;
        return label;
    }

    static muse::async::NotifyList<trackedit::Clip> makeClipList(std::initializer_list<trackedit::Clip> clips)
    {
        muse::async::NotifyList<trackedit::Clip> list;
        for (const auto& clip : clips) {
            list.push_back(clip);
        }
        return list;
    }

    static muse::async::NotifyList<trackedit::Label> makeLabelList(std::initializer_list<trackedit::Label> labels)
    {
        muse::async::NotifyList<trackedit::Label> list;
        for (const auto& label : labels) {
            list.push_back(label);
        }
        return list;
    }

protected:
    std::shared_ptr<ProjectViewState> m_projectViewState = nullptr;

    const std::shared_ptr<NiceMock<au::au3::Au3ProjectMock> > m_au3ProjectMock
        = std::make_shared<NiceMock<au::au3::Au3ProjectMock> >();

    const std::shared_ptr<NiceMock<ProjectSceneConfigurationMock> > m_configurationMock
        = std::make_shared<NiceMock<ProjectSceneConfigurationMock> >();

    const std::shared_ptr<NiceMock<au::playback::PlaybackConfigurationMock> > m_playbackConfigurationMock
        = std::make_shared<NiceMock<au::playback::PlaybackConfigurationMock> >();

    const std::shared_ptr<NiceMock<au::context::GlobalContextMock> > m_globalContextMock
        = std::make_shared<NiceMock<au::context::GlobalContextMock> >();

    const std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selectionControllerMock
        = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();

    const std::shared_ptr<NiceMock<trackedit::ProjectHistoryMock> > m_projectHistoryMock
        = std::make_shared<NiceMock<trackedit::ProjectHistoryMock> >();

    const std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_currentProject
        = std::make_shared<NiceMock<project::AudacityProjectMock> >();

    const std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_trackeditProjectMock
        = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();

    const std::shared_ptr<au3::Au3ProjectAccessor> m_au3ProjectAccessor = std::make_shared<au3::Au3ProjectAccessor>(
        muse::modularity::globalCtx());
};

TEST_F(ProjectViewStateTests, tracksInRange_InputIntervalIsClosedOpen)
{
    // i.e., [y1, y2)

    const trackedit::TrackIdList tracks = getAllTracks();
    ASSERT_EQ(tracks.size(), 2) << "Expecting 2 tracks in test.aup4";
    // Minimum track height is 44, so use values >= 44
    m_projectViewState->setTrackHeight(tracks[0], 50);
    m_projectViewState->setTrackHeight(tracks[1], 50);

    const auto tracksTop = m_projectViewState->trackVerticalPosition(tracks[0]);
    const auto firstTrackBottom = tracksTop + m_projectViewState->trackHeight(tracks[0]).val;
    const auto secondTrackTop = m_projectViewState->trackVerticalPosition(tracks[1]);
    const auto tracksBottom = secondTrackTop + m_projectViewState->trackHeight(tracks[1]).val;

    using namespace trackedit;

    EXPECT_EQ(TrackIdList({ }), m_projectViewState->tracksInRange(-10000, tracksTop));
    EXPECT_EQ(TrackIdList({ tracks[0] }), m_projectViewState->tracksInRange(-10000, tracksTop + 1));

    // Point selection
    EXPECT_EQ(TrackIdList({ tracks[0] }), m_projectViewState->tracksInRange(tracksTop + 1, tracksTop + 1));

    EXPECT_EQ(TrackIdList({ tracks[0], tracks[1] }), m_projectViewState->tracksInRange(tracksTop, secondTrackTop + 1));
    EXPECT_EQ(TrackIdList({ tracks[1] }), m_projectViewState->tracksInRange(firstTrackBottom, secondTrackTop + 1));

    // Outside
    EXPECT_EQ(TrackIdList({ }), m_projectViewState->tracksInRange(tracksBottom, tracksBottom + 1));
}

TEST_F(ProjectViewStateTests, AutoFitTrackHeights_UsesViewportHeightWithinBounds)
{
    const trackedit::TrackIdList tracks = getAllTracks();
    ASSERT_EQ(tracks.size(), 2) << "Expecting 2 tracks in test.aup4";

    m_projectViewState->setTracksViewportHeight(500);

    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 250);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 250);
    EXPECT_EQ(m_projectViewState->totalTrackHeight().val, 500);
}

TEST_F(ProjectViewStateTests, AutoFitTrackHeights_ClampsToMinAndMax)
{
    const trackedit::TrackIdList tracks = getAllTracks();
    ASSERT_EQ(tracks.size(), 2) << "Expecting 2 tracks in test.aup4";

    m_projectViewState->setTracksViewportHeight(1000);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 300);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 300);

    m_projectViewState->setTracksViewportHeight(100);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 110);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 110);
}

TEST_F(ProjectViewStateTests, ManualTrackHeightChangeDisengagesAutoFit)
{
    const trackedit::TrackIdList tracks = getAllTracks();
    ASSERT_EQ(tracks.size(), 2) << "Expecting 2 tracks in test.aup4";

    m_projectViewState->setTracksViewportHeight(400);
    m_projectViewState->changeTrackHeight(tracks[0], 20);
    m_projectViewState->setTracksViewportHeight(600);

    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 220);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 200);

    m_projectViewState->autoFitTrackHeights();
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 300);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 300);

    m_projectViewState->setTrackHeight(tracks[0], 150);
    m_projectViewState->setTracksViewportHeight(400);

    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 150);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 300);

    m_projectViewState->autoFitTrackHeights();
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 200);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 200);

    m_projectViewState->setTracksViewportHeight(500);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 250);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 250);
}

TEST_F(ProjectViewStateTests, ShrinkAndExpandTrackHeights)
{
    const trackedit::TrackIdList tracks = getAllTracks();
    ASSERT_EQ(tracks.size(), 2) << "Expecting 2 tracks in test.aup4";

    m_projectViewState->setTracksViewportHeight(180);
    m_projectViewState->setTrackHeight(tracks[0], 100);
    m_projectViewState->setTrackHeight(tracks[1], 120);

    m_projectViewState->collapseAllTrackHeights();
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 92);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 112);

    m_projectViewState->collapseTrackHeight(tracks[0]);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 84);

    m_projectViewState->expandAllTrackHeights();
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 92);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 120);

    m_projectViewState->expandTrackHeight(tracks[1]);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 128);

    m_projectViewState->setTrackHeight(tracks[0], 48);
    m_projectViewState->collapseTrackHeight(tracks[0]);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[0]).val, 44);

    m_projectViewState->setTrackHeight(tracks[1], 176);
    m_projectViewState->expandTrackHeight(tracks[1]);
    EXPECT_EQ(m_projectViewState->trackHeight(tracks[1]).val, 180);
}

TEST_F(ProjectViewStateTests, UpdateItemsBoundaries_IncludesAllClipsWhenNothingSelected)
{
    using namespace trackedit;

    // [GIVEN] Two tracks with one clip each and nothing currently selected.
    const TrackId track1 = 1;
    const TrackId track2 = 2;

    ON_CALL(*m_trackeditProjectMock, trackIdList())
    .WillByDefault(Return(TrackIdList { track1, track2 }));
    ON_CALL(*m_trackeditProjectMock, clipList(track1))
    .WillByDefault([](const TrackId&) { return makeClipList({ makeClip(1, 100, 1.0, 3.0) }); });
    ON_CALL(*m_trackeditProjectMock, clipList(track2))
    .WillByDefault([](const TrackId&) { return makeClipList({ makeClip(2, 200, 5.0, 8.0) }); });
    ON_CALL(*m_trackeditProjectMock, labelList(_))
    .WillByDefault([](const TrackId&) { return makeLabelList({}); });
    ON_CALL(*m_selectionControllerMock, selectedClips())
    .WillByDefault(Return(ClipKeyList {}));

    // [WHEN] Boundaries are rebuilt.
    m_projectViewState->updateItemsBoundaries(/*excludeCurrentSelection*/ false);

    // [THEN] Every clip edge is a snap target, even though nothing is selected.
    const std::set<muse::secs_t> expected { 1.0, 3.0, 5.0, 8.0 };
    EXPECT_EQ(m_projectViewState->itemsBoundaries(), expected);
}

TEST_F(ProjectViewStateTests, UpdateItemsBoundaries_RangeSelectionKeepsSelectedClipEdges)
{
    using namespace trackedit;

    // [GIVEN] One track with a selected clip and another, unselected clip.
    const TrackId track1 = 1;
    const Clip selected = makeClip(track1, 100, 2.0, 4.0);
    const Clip other = makeClip(track1, 200, 6.0, 9.0);

    ON_CALL(*m_trackeditProjectMock, trackIdList())
    .WillByDefault(Return(TrackIdList { track1 }));
    ON_CALL(*m_trackeditProjectMock, clipList(track1))
    .WillByDefault([selected, other](const TrackId&) { return makeClipList({ selected, other }); });
    ON_CALL(*m_trackeditProjectMock, labelList(_))
    .WillByDefault([](const TrackId&) { return makeLabelList({}); });
    ON_CALL(*m_selectionControllerMock, selectedClips())
    .WillByDefault(Return(ClipKeyList { selected.key }));

    // [WHEN/THEN] Without excluding the selection, both ends of the selected clip
    // remain snap targets (so a range selection snaps at start AND end).
    m_projectViewState->updateItemsBoundaries(/*excludeCurrentSelection*/ false);
    EXPECT_EQ(m_projectViewState->itemsBoundaries(), (std::set<muse::secs_t> { 2.0, 4.0, 6.0, 9.0 }));

    // [WHEN/THEN] Excluding the selection drops the selected clip's edges.
    m_projectViewState->updateItemsBoundaries(/*excludeCurrentSelection*/ true);
    EXPECT_EQ(m_projectViewState->itemsBoundaries(), (std::set<muse::secs_t> { 6.0, 9.0 }));
}

TEST_F(ProjectViewStateTests, UpdateItemsBoundaries_OmitsSpecifiedItemKey)
{
    using namespace trackedit;

    // [GIVEN] A clip being dragged and a neighbour it could snap to.
    const TrackId track1 = 1;
    const Clip dragged = makeClip(track1, 100, 1.0, 2.0);
    const Clip neighbour = makeClip(track1, 200, 4.0, 7.0);

    ON_CALL(*m_trackeditProjectMock, trackIdList())
    .WillByDefault(Return(TrackIdList { track1 }));
    ON_CALL(*m_trackeditProjectMock, clipList(track1))
    .WillByDefault([dragged, neighbour](const TrackId&) { return makeClipList({ dragged, neighbour }); });
    ON_CALL(*m_trackeditProjectMock, labelList(_))
    .WillByDefault([](const TrackId&) { return makeLabelList({}); });
    ON_CALL(*m_selectionControllerMock, selectedClips())
    .WillByDefault(Return(ClipKeyList {}));

    // [WHEN] Boundaries are rebuilt omitting the dragged clip.
    m_projectViewState->updateItemsBoundaries(/*excludeCurrentSelection*/ false, dragged.key);

    // [THEN] The dragged clip's own edges are not snap targets (no snapping to itself).
    EXPECT_EQ(m_projectViewState->itemsBoundaries(), (std::set<muse::secs_t> { 4.0, 7.0 }));
}

TEST_F(ProjectViewStateTests, UpdateItemsBoundaries_IncludesLabelBoundaries)
{
    using namespace trackedit;

    // [GIVEN] A track whose label edges should also be snap targets.
    const TrackId track1 = 1;

    ON_CALL(*m_trackeditProjectMock, trackIdList())
    .WillByDefault(Return(TrackIdList { track1 }));
    ON_CALL(*m_trackeditProjectMock, clipList(_))
    .WillByDefault([](const TrackId&) { return makeClipList({}); });
    ON_CALL(*m_trackeditProjectMock, labelList(track1))
    .WillByDefault([](const TrackId&) { return makeLabelList({ makeLabel(1, 300, 1.5, 3.5) }); });
    ON_CALL(*m_selectionControllerMock, selectedClips())
    .WillByDefault(Return(ClipKeyList {}));

    m_projectViewState->updateItemsBoundaries(/*excludeCurrentSelection*/ false);

    EXPECT_EQ(m_projectViewState->itemsBoundaries(), (std::set<muse::secs_t> { 1.5, 3.5 }));
}
} // namespace au::projectscene
