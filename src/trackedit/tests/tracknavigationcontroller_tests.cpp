/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "../internal/tracknavigationcontroller.h"

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "framework/ui/navigationcommands.h"
#include "mocks/commanddispatchermock.h"
#include "context/tests/mocks/globalcontextmock.h"
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
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_selectionController = std::make_shared<NiceMock<SelectionControllerMock> >();
        m_trackeditInteraction = std::make_shared<NiceMock<TrackeditInteractionMock> >();
        m_trackeditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();

        m_testCtx = std::make_shared<muse::modularity::Context>(999);
        m_controller = std::make_shared<TrackNavigationController>(m_testCtx);

        //! NOTE Inject mock dependencies directly via friend access
        m_controller->dispatcher.set(m_dispatcher);
        m_controller->commandDispatcher.set(m_commandDispatcher);

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

    //! NOTE Matcher for a framework panel-navigation command dispatch
    static auto isPanelCommand(const muse::rcommand::Command& command)
    {
        return ::testing::Truly([command](const muse::rcommand::Request& request) {
            return request.query.uri() == command;
        });
    }

    std::shared_ptr<muse::modularity::Context> m_testCtx;
    std::shared_ptr<TrackNavigationController> m_controller;
    std::shared_ptr<muse::actions::ActionsDispatcherMock> m_dispatcher;
    std::shared_ptr<muse::rcommand::CommandDispatcherMock> m_commandDispatcher;
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
}
