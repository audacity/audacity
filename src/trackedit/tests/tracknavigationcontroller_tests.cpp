/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "../internal/tracknavigationcontroller.h"

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "framework/ui/navigationcommands.h"
#include "mocks/commanddispatchermock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "mocks/navigationcontrollermock.h"
#include "mocks/selectioncontrollermock.h"
#include "mocks/trackeditinteractionmock.h"
#include "mocks/trackeditprojectmock.h"

using ::testing::NiceMock;
using ::testing::Return;
using ::testing::_;

using IActionsDispatcher = muse::actions::IActionsDispatcher;

namespace au::trackedit {
/*******************************************************************************
 * TRACK NAVIGATION CONTROLLER TESTS
 *
 * Verifies the Tab / Shift+Tab panel navigation of the track view:
 * - on a clip it steps to the adjacent clip of the same track
 * - on the edge clip (or a track without a focused clip) it hands over to the
 *   framework panel navigation (NEXT_PANEL_COMMAND / PREV_PANEL_COMMAND)
 ******************************************************************************/

class TrackNavigationControllerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_dispatcher = std::make_shared<NiceMock<muse::actions::ActionsDispatcherMock> >();
        m_commandDispatcher = std::make_shared<NiceMock<muse::rcommand::CommandDispatcherMock> >();
        m_navigationController = std::make_shared<NiceMock<muse::ui::NavigationControllerMock> >();
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_selectionController = std::make_shared<NiceMock<SelectionControllerMock> >();
        m_trackeditInteraction = std::make_shared<NiceMock<TrackeditInteractionMock> >();
        m_trackeditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();

        m_testCtx = std::make_shared<muse::modularity::Context>(999);
        m_controller = std::make_shared<TrackNavigationController>(m_testCtx);

        //! NOTE Inject mock dependencies directly via friend access
        m_controller->dispatcher.set(m_dispatcher);
        m_controller->commandDispatcher.set(m_commandDispatcher);
        m_controller->navigationController.set(m_navigationController);

        ON_CALL(*m_commandDispatcher, dispatch(_))
        .WillByDefault([](const muse::rcommand::Request& request) {
            return muse::async::make_promise<muse::rcommand::Response>([request](auto resolve) {
                return resolve(muse::rcommand::make_response(request, muse::make_ok()));
            });
        });
        m_controller->globalContext.set(m_globalContext);
        m_controller->selectionController.set(m_selectionController);
        m_controller->trackeditInteraction.set(m_trackeditInteraction);

        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProject));

        ON_CALL(*m_globalContext, currentTrackeditProjectChanged())
        .WillByDefault(Return(muse::async::Notification()));

        ON_CALL(*m_selectionController, tracksSelected())
        .WillByDefault(Return(muse::async::Channel<TrackIdList>()));

        //! NOTE Capture registered action callbacks so we can invoke them in tests
        ON_CALL(*m_dispatcher,
                reg(::testing::Matcher<muse::actions::Actionable*>(_),
                    ::testing::Matcher<const muse::actions::ActionCode&>(_),
                    ::testing::Matcher<const IActionsDispatcher::ActionCallBackWithNameAndData&>(_)))
        .WillByDefault([this](muse::actions::Actionable*, const muse::actions::ActionCode& code,
                              const IActionsDispatcher::ActionCallBackWithNameAndData& cb) {
            m_actionCallbacks[code] = cb;
        });
    }

    void TearDown() override
    {
        m_controller.reset();
        muse::modularity::removeIoC(m_testCtx);
    }

    void initController()
    {
        m_controller->init();
    }

    void invokeAction(const muse::actions::ActionCode& code)
    {
        auto it = m_actionCallbacks.find(code);
        ASSERT_NE(it, m_actionCallbacks.end()) << "Action not registered: " << code;
        it->second(code, muse::actions::ActionData());
    }

    static Clip makeClip(const TrackId& trackId, const TrackItemId& itemId, double startTime)
    {
        Clip clip;
        clip.key = { trackId, itemId };
        clip.startTime = startTime;
        return clip;
    }

    void setupTrackWithClips(const TrackId& trackId, const std::vector<Clip>& clips)
    {
        Track track;
        track.id = trackId;
        track.type = TrackType::Mono;

        ON_CALL(*m_trackeditProject, track(trackId))
        .WillByDefault(Return(track));
        ON_CALL(*m_trackeditProject, clipList(trackId))
        .WillByDefault([clips](const TrackId&) {
            muse::async::NotifyList<Clip> list;
            for (const Clip& clip : clips) {
                list.push_back(clip);
            }
            return list;
        });
    }

    struct TrackSpec
    {
        TrackId id = INVALID_TRACK;
        std::vector<Clip> clips;
    };

    //! NOTE Set up a project with several mono tracks, each with its own clips.
    //! Wires trackList(), per-track track()/clipList() and a clip(key) lookup so
    //! the start-time based navigation (above/below item) can be exercised.
    void setupTracks(const std::vector<TrackSpec>& specs)
    {
        std::vector<Track> trackList;
        std::vector<Clip> allClips;

        for (const TrackSpec& spec : specs) {
            setupTrackWithClips(spec.id, spec.clips);

            Track track;
            track.id = spec.id;
            track.type = TrackType::Mono;
            trackList.push_back(track);

            for (const Clip& clip : spec.clips) {
                allClips.push_back(clip);
            }
        }

        ON_CALL(*m_trackeditProject, trackList())
        .WillByDefault(Return(trackList));

        ON_CALL(*m_trackeditProject, clip(_))
        .WillByDefault([allClips](const ClipKey& key) {
            for (const Clip& clip : allClips) {
                if (clip.key.trackId == key.trackId && clip.key.itemId == key.itemId) {
                    return clip;
                }
            }
            return Clip {};
        });
    }

    //! NOTE Matcher for a framework panel-navigation command dispatch
    static auto isPanelCommand(const muse::rcommand::Command& command)
    {
        return ::testing::Truly([command](const muse::rcommand::Request& request) {
            return request.command == command;
        });
    }

    std::shared_ptr<muse::modularity::Context> m_testCtx;
    std::shared_ptr<TrackNavigationController> m_controller;
    std::shared_ptr<muse::actions::ActionsDispatcherMock> m_dispatcher;
    std::shared_ptr<muse::rcommand::CommandDispatcherMock> m_commandDispatcher;
    std::shared_ptr<muse::ui::NavigationControllerMock> m_navigationController;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<SelectionControllerMock> m_selectionController;
    std::shared_ptr<TrackeditInteractionMock> m_trackeditInteraction;
    std::shared_ptr<TrackeditProjectMock> m_trackeditProject;

    std::map<muse::actions::ActionCode, IActionsDispatcher::ActionCallBackWithNameAndData> m_actionCallbacks;
};

/**
 * Tab, while a clip is focused, steps to the next clip of the same track without
 * handing over to framework panel navigation.
 */
TEST_F(TrackNavigationControllerTests, TabOnClipStepsToNextClip)
{
    //! [GIVEN] A track with two clips, focus on the first clip
    setupTrackWithClips(1, { makeClip(1, 100, 0.0), makeClip(1, 200, 2.0) });

    initController();
    m_controller->setFocusedItem({ 1, 100 });

    //! [EXPECT] Framework panel navigation is NOT dispatched
    EXPECT_CALL(*m_commandDispatcher, dispatch(isPanelCommand(muse::ui::NEXT_PANEL_COMMAND))).Times(0);

    //! [WHEN] Tab is pressed
    invokeAction("track-view-next-panel");

    //! [THEN] Focus moves to the next clip
    EXPECT_EQ(m_controller->focusedItem(), (TrackItemKey { 1, 200 }));
}

/**
 * Tab, while the last clip of a track is focused, hands over to the framework
 * panel navigation and leaves the focus untouched.
 */
TEST_F(TrackNavigationControllerTests, TabOnLastClipHandsOverToNextPanel)
{
    //! [GIVEN] A track with two clips, focus on the last clip
    setupTrackWithClips(1, { makeClip(1, 100, 0.0), makeClip(1, 200, 2.0) });

    initController();
    m_controller->setFocusedItem({ 1, 200 });

    //! [EXPECT] Framework panel navigation is dispatched
    EXPECT_CALL(*m_commandDispatcher, dispatch(isPanelCommand(muse::ui::NEXT_PANEL_COMMAND))).Times(1);

    //! [WHEN] Tab is pressed
    invokeAction("track-view-next-panel");

    //! [THEN] Focus stays on the last clip
    EXPECT_EQ(m_controller->focusedItem(), (TrackItemKey { 1, 200 }));
}

/**
 * Tab, while a track (no clip) is focused, hands over to the framework panel
 * navigation.
 */
TEST_F(TrackNavigationControllerTests, TabNavigatesToNextPanelWhenNoItems)
{
    //! [GIVEN] One track with no clips, focus on the track (no item)
    setupTrackWithClips(1, {});

    initController();
    m_controller->setFocusedTrack(1);

    //! [EXPECT] Framework panel navigation is dispatched
    EXPECT_CALL(*m_commandDispatcher, dispatch(isPanelCommand(muse::ui::NEXT_PANEL_COMMAND))).Times(1);

    //! [WHEN] Tab is pressed
    invokeAction("track-view-next-panel");
}

/**
 * Shift+Tab, while a clip is focused, steps to the previous clip of the same
 * track without handing over to framework panel navigation.
 */
TEST_F(TrackNavigationControllerTests, ShiftTabOnClipStepsToPrevClip)
{
    //! [GIVEN] A track with two clips, focus on the last clip
    setupTrackWithClips(1, { makeClip(1, 100, 0.0), makeClip(1, 200, 2.0) });

    initController();
    m_controller->setFocusedItem({ 1, 200 });

    //! [EXPECT] Framework panel navigation is NOT dispatched
    EXPECT_CALL(*m_commandDispatcher, dispatch(isPanelCommand(muse::ui::PREV_PANEL_COMMAND))).Times(0);

    //! [WHEN] Shift+Tab is pressed
    invokeAction("track-view-prev-panel");

    //! [THEN] Focus moves to the previous clip
    EXPECT_EQ(m_controller->focusedItem(), (TrackItemKey { 1, 100 }));
}

/**
 * Shift+Tab, while the first clip of a track is focused, hands over to the
 * framework panel navigation and leaves the focus untouched.
 */
TEST_F(TrackNavigationControllerTests, ShiftTabOnFirstClipHandsOverToPrevPanel)
{
    //! [GIVEN] A track with two clips, focus on the first clip
    setupTrackWithClips(1, { makeClip(1, 100, 0.0), makeClip(1, 200, 2.0) });

    initController();
    m_controller->setFocusedItem({ 1, 100 });

    //! [EXPECT] Framework panel navigation is dispatched
    EXPECT_CALL(*m_commandDispatcher, dispatch(isPanelCommand(muse::ui::PREV_PANEL_COMMAND))).Times(1);

    //! [WHEN] Shift+Tab is pressed
    invokeAction("track-view-prev-panel");

    //! [THEN] Focus stays on the first clip
    EXPECT_EQ(m_controller->focusedItem(), (TrackItemKey { 1, 100 }));
}

/**
 * Down (track-view-below-item), while a track (no clip) is focused, moves the
 * focus to the next track.
 */
TEST_F(TrackNavigationControllerTests, DownFromTrackFocusesNextTrack)
{
    //! [GIVEN] Two tracks, focus on the first track (no item)
    setupTracks({ { 1, {} }, { 2, {} } });

    initController();
    m_controller->setFocusedTrack(1);

    //! [WHEN] Down is pressed
    invokeAction("track-view-below-item");

    //! [THEN] The next track is focused
    EXPECT_EQ(m_controller->focusedTrack(), 2);
}

/**
 * Up (track-view-above-item), while a track (no clip) is focused, moves the
 * focus to the previous track.
 */
TEST_F(TrackNavigationControllerTests, UpFromTrackFocusesPrevTrack)
{
    //! [GIVEN] Two tracks, focus on the second track (no item)
    setupTracks({ { 1, {} }, { 2, {} } });

    initController();
    m_controller->setFocusedTrack(2);

    //! [WHEN] Up is pressed
    invokeAction("track-view-above-item");

    //! [THEN] The previous track is focused
    EXPECT_EQ(m_controller->focusedTrack(), 1);
}

/**
 * Down (track-view-below-item), while a clip is focused, moves the focus to the
 * clip closest in start time on the next non-empty track.
 */
TEST_F(TrackNavigationControllerTests, DownFromClipFocusesClosestClipBelow)
{
    //! [GIVEN] Two tracks with clips, focus on the first clip of the first track
    setupTracks({
            { 1, { makeClip(1, 100, 0.0), makeClip(1, 200, 2.0) } },
            { 2, { makeClip(2, 300, 0.1), makeClip(2, 400, 2.5) } }
        });

    initController();
    m_controller->setFocusedItem({ 1, 100 }); //!< start time 0.0

    //! [WHEN] Down is pressed
    invokeAction("track-view-below-item");

    //! [THEN] The clip closest to 0.0 on track 2 is focused (clip 300 at 0.1)
    EXPECT_EQ(m_controller->focusedItem(), (TrackItemKey { 2, 300 }));
}

/**
 * Up (track-view-above-item), while a clip is focused, moves the focus to the
 * clip closest in start time on the previous non-empty track.
 */
TEST_F(TrackNavigationControllerTests, UpFromClipFocusesClosestClipAbove)
{
    //! [GIVEN] Two tracks with clips, focus on the last clip of the second track
    setupTracks({
            { 1, { makeClip(1, 100, 0.0), makeClip(1, 200, 2.0) } },
            { 2, { makeClip(2, 300, 0.1), makeClip(2, 400, 2.5) } }
        });

    initController();
    m_controller->setFocusedItem({ 2, 400 }); //!< start time 2.5

    //! [WHEN] Up is pressed
    invokeAction("track-view-above-item");

    //! [THEN] The clip closest to 2.5 on track 1 is focused (clip 200 at 2.0)
    EXPECT_EQ(m_controller->focusedItem(), (TrackItemKey { 1, 200 }));
}

/**
 * Shift+F10 (track-view-item-context-menu), while a track is focused, requests
 * the context menu for the focused track (no item).
 */
TEST_F(TrackNavigationControllerTests, ContextMenuRequestedForFocusedTrack)
{
    //! [GIVEN] One track, focus on the track (no item)
    setupTracks({ { 1, {} } });

    initController();
    m_controller->setFocusedTrack(1);

    //! [GIVEN] A listener on the context-menu request channel
    TrackItemKey requested { INVALID_TRACK, INVALID_TRACK_ITEM };
    bool called = false;
    muse::async::Channel<TrackItemKey> channel = m_controller->openContextMenuRequested();
    channel.onReceive(m_controller.get(), [&requested, &called](const TrackItemKey& key) {
        requested = key;
        called = true;
    });

    //! [WHEN] Shift+F10 is pressed
    invokeAction("track-view-item-context-menu");

    //! [THEN] The context menu is requested for the focused track
    EXPECT_TRUE(called);
    EXPECT_EQ(requested, (TrackItemKey { 1, INVALID_TRACK_ITEM }));
}

/**
 * Shift+F10 (track-view-item-context-menu) is a no-op when nothing is focused.
 */
TEST_F(TrackNavigationControllerTests, ContextMenuNotRequestedWithoutFocus)
{
    //! [GIVEN] A project with one track, but nothing is focused
    setupTracks({ { 1, {} } });

    initController();

    bool called = false;
    muse::async::Channel<TrackItemKey> channel = m_controller->openContextMenuRequested();
    channel.onReceive(m_controller.get(), [&called](const TrackItemKey&) {
        called = true;
    });

    //! [WHEN] Shift+F10 is pressed
    invokeAction("track-view-item-context-menu");

    //! [THEN] Nothing is requested
    EXPECT_FALSE(called);
}

/**
 * resetNavigation() drops the navigation highlight and clears the vertical-navigation
 * reference time, so the next Up/Down step recalculates it from the focused item instead
 * of a stale value. This is what the cancel/escape path relies on.
 */
TEST_F(TrackNavigationControllerTests, ResetNavigationRecomputesVerticalReference)
{
    //! [GIVEN] Three tracks; track 3 has a clip near t=1 and another near t=9
    setupTracks({
            { 1, { makeClip(1, 100, 0.0) } },
            { 2, { makeClip(2, 200, 0.0), makeClip(2, 210, 10.0) } },
            { 3, { makeClip(3, 300, 1.0), makeClip(3, 310, 9.0) } },
        });

    initController();

    //! [GIVEN] A vertical anchor established at t=0 by moving down from the first track
    m_controller->setFocusedItem({ 1, 100 });
    invokeAction("track-view-below-item");
    ASSERT_EQ(m_controller->focusedItem(), (TrackItemKey { 2, 200 }));

    //! [GIVEN] The focus is moved to an item at t=10 (the anchor is now stale at t=0)
    m_controller->setFocusedItem({ 2, 210 });

    //! [EXPECT] Resetting the navigation also drops the highlight
    EXPECT_CALL(*m_navigationController, setIsHighlight(false)).Times(1);

    //! [WHEN] The navigation is reset (as the cancel/escape path does)
    m_controller->resetNavigation();

    //! [AND] Down is pressed
    invokeAction("track-view-below-item");

    //! [THEN] The reference is recomputed from t=10, so the closest clip on track 3 is 310 (t=9),
    //! not the stale-anchor clip 300 (t=1)
    EXPECT_EQ(m_controller->focusedItem(), (TrackItemKey { 3, 310 }));
}
}
