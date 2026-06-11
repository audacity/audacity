/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <set>

#include "projectscene/view/timeline/timelinecontext.h"
#include "projectscene/view/tracksitemsview/trackclipslistmodel.h"
#include "projectscene/view/tracksitemsview/viewtrackitem.h"
#include "projectscene/internal/projectviewstate.h"

#include "snaptestaccess.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/audiooutputmock.h"
#include "playback/tests/mocks/playbackmock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"

using namespace ::testing;

namespace au::projectscene {
//! Wires a real (uninitialised) TimelineContext to mocks so the snap/guideline
//! helpers can be exercised in isolation. With the default zoom (1.0) and
//! frame start (0.0), position and time are 1:1.
class SnapTestBase : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_project = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackeditProject = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();
        m_viewState = std::make_shared<ProjectViewState>(muse::modularity::globalCtx());
        m_playback = std::make_shared<NiceMock<playback::PlaybackMock> >();
        m_audioOutput = std::make_shared<NiceMock<playback::AudioOutputMock> >();

        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_project));
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_project, viewState())
        .WillByDefault(Return(m_viewState));
        ON_CALL(*m_playback, audioOutput())
        .WillByDefault(Return(m_audioOutput));
        ON_CALL(*m_audioOutput, sampleRate())
        .WillByDefault(Return(static_cast<uint64_t>(44100)));
        ON_CALL(*m_trackeditProject, timeSignature())
        .WillByDefault(Return(trackedit::TimeSignature { 120.0, 4, 4 }));

        m_context = new TimelineContext();
        SnapTestAccess::wireContext(m_context, m_globalContext, m_playback);
    }

    void TearDown() override
    {
        delete m_context;
    }

    void useGridSnapOff(const std::set<muse::secs_t>& boundaries)
    {
        m_viewState->setIsSnapEnabled(false);
        m_viewState->setItemsBoundaries(boundaries);
    }

    void useGridSnapOn(SnapType type)
    {
        m_viewState->setIsSnapEnabled(true);
        m_viewState->setSnapType(type);
    }

    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_project;
    std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_trackeditProject;
    std::shared_ptr<ProjectViewState> m_viewState;
    std::shared_ptr<NiceMock<playback::PlaybackMock> > m_playback;
    std::shared_ptr<NiceMock<playback::AudioOutputMock> > m_audioOutput;

    TimelineContext* m_context = nullptr;
};

// =====================================================================
// TimelineContext::findGuideline / isGuidelineValid
// =====================================================================

class TimelineContextFindGuidelineTests : public SnapTestBase
{
};

TEST_F(TimelineContextFindGuidelineTests, GridSnapOff_ExactBoundary_ReturnsBoundary)
{
    //! CASE With grid snap off, a time sitting exactly on an item boundary
    //! resolves to that boundary.
    useGridSnapOff({ 5.0, 10.0 });

    const double guideline = m_context->findGuideline(5.0);

    EXPECT_TRUE(m_context->isGuidelineValid(guideline));
    EXPECT_DOUBLE_EQ(guideline, 5.0);
}

TEST_F(TimelineContextFindGuidelineTests, GridSnapOff_WithinOneSampleOfBoundary_SnapsToBoundary)
{
    //! CASE The regression: a clip edge quantised to a sample lands a fraction
    //! of a sample away from a (non-sample-aligned) label boundary. It must
    //! still resolve to the boundary so the guideline draws on the edge.
    useGridSnapOff({ 10.0 });

    const double almostBoundary = 10.0 + (0.5 / 44100.0); // half a sample away

    const double guideline = m_context->findGuideline(almostBoundary);

    EXPECT_TRUE(m_context->isGuidelineValid(guideline));
    EXPECT_DOUBLE_EQ(guideline, 10.0);
}

TEST_F(TimelineContextFindGuidelineTests, GridSnapOff_FarFromBoundary_ReturnsInvalid)
{
    //! CASE A time more than a sample away from every boundary yields no guideline.
    useGridSnapOff({ 5.0, 10.0 });

    const double guideline = m_context->findGuideline(7.0);

    EXPECT_FALSE(m_context->isGuidelineValid(guideline));
    EXPECT_DOUBLE_EQ(guideline, TimelineContext::INVALID_GUIDELINE_TIME);
}

TEST_F(TimelineContextFindGuidelineTests, GridSnapOff_NoBoundaries_ReturnsInvalid)
{
    //! CASE No boundaries at all means nothing to snap to.
    useGridSnapOff({});

    EXPECT_FALSE(m_context->isGuidelineValid(m_context->findGuideline(3.0)));
}

TEST_F(TimelineContextFindGuidelineTests, GridSnapOn_OnGrid_ReturnsTime)
{
    //! CASE With grid snap on, a time already on the grid resolves to itself.
    useGridSnapOn(SnapType::Seconds);

    const double guideline = m_context->findGuideline(3.0);

    EXPECT_TRUE(m_context->isGuidelineValid(guideline));
    EXPECT_DOUBLE_EQ(guideline, 3.0);
}

TEST_F(TimelineContextFindGuidelineTests, GridSnapOn_OffGrid_ReturnsInvalid)
{
    //! CASE With grid snap on, a time away from any grid line yields no guideline.
    useGridSnapOn(SnapType::Seconds);

    EXPECT_FALSE(m_context->isGuidelineValid(m_context->findGuideline(3.4)));
}

TEST_F(TimelineContextFindGuidelineTests, IsGuidelineValid_DistinguishesSentinel)
{
    EXPECT_TRUE(m_context->isGuidelineValid(0.0));
    EXPECT_TRUE(m_context->isGuidelineValid(12.5));
    EXPECT_FALSE(m_context->isGuidelineValid(TimelineContext::INVALID_GUIDELINE_TIME));
}

// =====================================================================
// TrackItemsListModel::findGuideline / containsItem
// =====================================================================

namespace {
//! Minimal ViewTrackItem whose key and time can be set directly, avoiding the
//! configuration() dependency of the concrete clip/label items.
class TestTrackItem : public ViewTrackItem
{
public:
    TestTrackItem(const trackedit::TrackItemKey& key, double start, double end, QObject* parent)
        : ViewTrackItem(parent)
    {
        m_key = TrackItemKey(key);
        TrackItemTime t;
        t.startTime = start;
        t.endTime = end;
        m_time = t;
    }
};
}

class TrackItemsFindGuidelineTests : public SnapTestBase
{
protected:
    void SetUp() override
    {
        SnapTestBase::SetUp();
        m_model = new TrackClipsListModel();
        m_model->m_context = m_context;
    }

    void TearDown() override
    {
        delete m_model;
        SnapTestBase::TearDown();
    }

    void addItem(const trackedit::TrackItemKey& key, double start, double end)
    {
        m_model->m_items.append(new TestTrackItem(key, start, end, m_model));
    }

    TrackClipsListModel* m_model = nullptr;
};

TEST_F(TrackItemsFindGuidelineTests, ContainsItem_ReflectsMembership)
{
    addItem({ 1, 100 }, 5.0, 20.0);

    EXPECT_TRUE(m_model->containsItem(TrackItemKey({ 1, 100 })));
    EXPECT_FALSE(m_model->containsItem(TrackItemKey({ 1, 999 })));
}

TEST_F(TrackItemsFindGuidelineTests, FindGuideline_UnknownKey_ReturnsInvalid)
{
    useGridSnapOff({ 5.0 });
    // no items added

    const double guideline = m_model->findGuideline(TrackItemKey({ 1, 100 }), DirectionType::Direction::Auto);

    EXPECT_FALSE(m_context->isGuidelineValid(guideline));
    EXPECT_DOUBLE_EQ(guideline, TimelineContext::INVALID_GUIDELINE_TIME);
}

TEST_F(TrackItemsFindGuidelineTests, FindGuideline_Left_UsesStartEdgeOnly)
{
    //! CASE Left direction probes the item's start edge and ignores its end edge.
    addItem({ 1, 100 }, 5.0, 20.0);
    useGridSnapOff({ 5.0, 20.0 });

    EXPECT_DOUBLE_EQ(m_model->findGuideline(TrackItemKey({ 1, 100 }), DirectionType::Direction::Left), 5.0);

    // Start no longer on a boundary -> Left finds nothing even though the end still is.
    useGridSnapOff({ 20.0 });
    EXPECT_FALSE(m_context->isGuidelineValid(
                     m_model->findGuideline(TrackItemKey({ 1, 100 }), DirectionType::Direction::Left)));
}

TEST_F(TrackItemsFindGuidelineTests, FindGuideline_Right_UsesEndEdgeOnly)
{
    //! CASE Right direction probes the item's end edge and ignores its start edge.
    addItem({ 1, 100 }, 5.0, 20.0);
    useGridSnapOff({ 5.0, 20.0 });

    EXPECT_DOUBLE_EQ(m_model->findGuideline(TrackItemKey({ 1, 100 }), DirectionType::Direction::Right), 20.0);

    // End no longer on a boundary -> Right finds nothing even though the start still is.
    useGridSnapOff({ 5.0 });
    EXPECT_FALSE(m_context->isGuidelineValid(
                     m_model->findGuideline(TrackItemKey({ 1, 100 }), DirectionType::Direction::Right)));
}

TEST_F(TrackItemsFindGuidelineTests, FindGuideline_Auto_PrefersStartThenEnd)
{
    //! CASE Auto probes the start edge first, then falls back to the end edge.
    addItem({ 1, 100 }, 5.0, 20.0);

    useGridSnapOff({ 5.0, 20.0 });
    EXPECT_DOUBLE_EQ(m_model->findGuideline(TrackItemKey({ 1, 100 }), DirectionType::Direction::Auto), 5.0);

    useGridSnapOff({ 20.0 });
    EXPECT_DOUBLE_EQ(m_model->findGuideline(TrackItemKey({ 1, 100 }), DirectionType::Direction::Auto), 20.0);
}
}
