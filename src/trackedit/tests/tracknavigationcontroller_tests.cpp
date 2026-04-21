/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "../internal/tracknavigationcontroller.h"

#include "actions/tests/mocks/actionsdispatchermock.h"
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
 * Verifies arrow key behavior based on navigation highlight state:
 * - When highlight is off (default): arrows move the playhead
 * - When highlight is on (after Tab): arrows navigate clips
 * - Escape dismisses highlight, returning arrows to playhead movement
 ******************************************************************************/

class TrackNavigationControllerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_dispatcher = std::make_shared<NiceMock<muse::actions::ActionsDispatcherMock> >();
        m_navigationController = std::make_shared<NiceMock<muse::ui::NavigationControllerMock> >();
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_selectionController = std::make_shared<NiceMock<SelectionControllerMock> >();
        m_trackeditInteraction = std::make_shared<NiceMock<TrackeditInteractionMock> >();
        m_trackeditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();

        m_testCtx = std::make_shared<muse::modularity::Context>(999);
        m_controller = std::make_shared<TrackNavigationController>(m_testCtx);

        //! NOTE Inject mock dependencies directly via friend access
        m_controller->dispatcher.set(m_dispatcher);
        m_controller->navigationController.set(m_navigationController);
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

    std::shared_ptr<muse::modularity::Context> m_testCtx;
    std::shared_ptr<TrackNavigationController> m_controller;
    std::shared_ptr<muse::actions::ActionsDispatcherMock> m_dispatcher;
    std::shared_ptr<muse::ui::NavigationControllerMock> m_navigationController;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<SelectionControllerMock> m_selectionController;
    std::shared_ptr<TrackeditInteractionMock> m_trackeditInteraction;
    std::shared_ptr<TrackeditProjectMock> m_trackeditProject;

    std::map<muse::actions::ActionCode, IActionsDispatcher::ActionCallBackWithNameAndData> m_actionCallbacks;
};

/**
 * Test 1: When project just opened (no highlight), right arrow should move playhead
 */
TEST_F(TrackNavigationControllerTests, RightArrowMovesPlayheadWhenNoHighlight)
{
    //! [GIVEN] Navigation highlight is off (default after opening project)
    ON_CALL(*m_navigationController, isHighlight())
    .WillByDefault(Return(false));

    //! [GIVEN] Controller is initialized
    initController();

    //! [EXPECT] Playhead move action is dispatched
    EXPECT_CALL(*m_dispatcher, dispatch(muse::actions::ActionCode("play-position-increase"))).Times(1);

    //! [WHEN] Right arrow is pressed (track-view-next-item is dispatched by shortcut system)
    invokeAction("track-view-next-item");
}

/**
 * Test 2: When project just opened (no highlight), left arrow should move playhead
 */
TEST_F(TrackNavigationControllerTests, LeftArrowMovesPlayheadWhenNoHighlight)
{
    //! [GIVEN] Navigation highlight is off
    ON_CALL(*m_navigationController, isHighlight())
    .WillByDefault(Return(false));

    //! [GIVEN] Controller is initialized
    initController();

    //! [EXPECT] Playhead move action is dispatched
    EXPECT_CALL(*m_dispatcher, dispatch(muse::actions::ActionCode("play-position-decrease"))).Times(1);

    //! [WHEN] Left arrow is pressed
    invokeAction("track-view-prev-item");
}

/**
 * Test 3: When navigation is highlighted, right arrow should navigate to next clip (not move playhead)
 */
TEST_F(TrackNavigationControllerTests, RightArrowNavigatesClipsWhenHighlighted)
{
    //! [GIVEN] Navigation highlight is on (user pressed Tab)
    ON_CALL(*m_navigationController, isHighlight())
    .WillByDefault(Return(true));

    //! [GIVEN] There is a track with two clips, focus is on the first clip
    Track track;
    track.id = 1;
    track.type = TrackType::Mono;

    Clip clip1;
    clip1.key = { 1, 100 };
    clip1.startTime = 0.0;
    Clip clip2;
    clip2.key = { 1, 200 };
    clip2.startTime = 2.0;

    ON_CALL(*m_trackeditProject, track(1))
    .WillByDefault(Return(track));
    ON_CALL(*m_trackeditProject, clipList(1))
    .WillByDefault([clip1, clip2](const TrackId&) {
        muse::async::NotifyList<Clip> list;
        list.push_back(clip1);
        list.push_back(clip2);
        return list;
    });

    //! [GIVEN] Controller is initialized with focus on the first clip
    initController();
    m_controller->setFocusedItem(clip1.key);

    //! [EXPECT] Playhead move is NOT dispatched
    EXPECT_CALL(*m_dispatcher, dispatch(muse::actions::ActionCode("play-position-increase"))).Times(0);

    //! [WHEN] Right arrow is pressed
    invokeAction("track-view-next-item");
}

/**
 * Test 4: When project just opened, Tab should trigger panel navigation
 */
TEST_F(TrackNavigationControllerTests, TabNavigatesToNextPanelWhenNoItems)
{
    //! [GIVEN] There is one track with no clips
    Track track;
    track.id = 1;
    track.type = TrackType::Mono;

    ON_CALL(*m_trackeditProject, trackList())
    .WillByDefault(Return(std::vector<Track>({ track })));
    ON_CALL(*m_trackeditProject, track(1))
    .WillByDefault(Return(track));
    ON_CALL(*m_trackeditProject, clipList(1))
    .WillByDefault(Return(muse::async::NotifyList<Clip> {}));

    //! [GIVEN] Controller is initialized with focus on the track (no item)
    initController();
    m_controller->setFocusedTrack(1);

    //! [EXPECT] Framework panel navigation is dispatched
    EXPECT_CALL(*m_dispatcher, dispatch(muse::actions::ActionCode("nav-next-panel"))).Times(1);

    //! [WHEN] Tab is pressed (track-view-next-panel is dispatched by shortcut system)
    invokeAction("track-view-next-panel");
}

/**
 * Test 5: Pressing Escape dismisses highlight, then arrow moves playhead
 */
TEST_F(TrackNavigationControllerTests, EscapeDismissesHighlightThenArrowMovesPlayhead)
{
    //! [GIVEN] Navigation highlight is initially on
    bool isHighlighted = true;
    ON_CALL(*m_navigationController, isHighlight())
    .WillByDefault([&isHighlighted]() { return isHighlighted; });

    //! [GIVEN] Controller is initialized
    initController();

    //! [EXPECT] Escape sets highlight to false
    EXPECT_CALL(*m_navigationController, setIsHighlight(false)).Times(1);

    //! [WHEN] Escape is pressed (nav-escape is dispatched)
    invokeAction("nav-escape");

    //! [THEN] Simulate highlight being turned off
    isHighlighted = false;

    //! [EXPECT] Now arrow dispatches playhead move
    EXPECT_CALL(*m_dispatcher, dispatch(muse::actions::ActionCode("play-position-increase"))).Times(1);

    //! [WHEN] Right arrow is pressed after Escape
    invokeAction("track-view-next-item");
}
}
