/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <QQmlEngine>
#include <QQmlContext>

#include "../view/tracknavigationmodel.h"

#include "global/modularity/ioc.h"
#include "global/async/channel.h"
#include "global/async/notification.h"
#include "ui/inavigation.h"
#include "ui/qml/Muse/Ui/navigationsection.h"
#include "ui/qml/Muse/Ui/navigationcontrol.h"

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "mocks/navigationcontrollermock.h"
#include "mocks/tracknavigationcontrollermock.h"
#include "mocks/trackeditprojectmock.h"

using ::testing::NiceMock;
using ::testing::Return;
using ::testing::ReturnRef;
using ::testing::SaveArg;
using ::testing::_;

namespace au::trackedit {
static constexpr const char* SECTION_NAME = "TrackViewSection";

class TrackNavigationModelTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_navigationController = std::make_shared<NiceMock<muse::ui::NavigationControllerMock> >();
        m_tracksNavigationController = std::make_shared<NiceMock<TrackNavigationControllerMock> >();
        m_dispatcher = std::make_shared<NiceMock<muse::actions::ActionsDispatcherMock> >();
        m_trackeditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();

        //! NOTE The context the model and its panels resolve their dependencies from
        m_testCtx = std::make_shared<muse::modularity::Context>(999);

        auto ioc = muse::modularity::ioc(m_testCtx);
        ioc->registerExportNoDelete<context::IGlobalContext>("utests", m_globalContext.get());
        ioc->registerExportNoDelete<muse::ui::INavigationController>("utests", m_navigationController.get());
        ioc->registerExportNoDelete<ITrackNavigationController>("utests", m_tracksNavigationController.get());
        ioc->registerExportNoDelete<muse::actions::IActionsDispatcher>("utests", m_dispatcher.get());

        //! NOTE Wire the test IoC context into the QML root context, so that objects
        //! created with the QObject constructor resolve their dependencies from it
        m_qmlIoc = new muse::QmlIoCContext(&m_engine);
        m_qmlIoc->ctx = m_testCtx;
        m_engine.rootContext()->setContextProperty("ioc_context", QVariant::fromValue(m_qmlIoc));

        //! NOTE The project channels the model subscribes to
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_globalContext, currentTrackeditProjectChanged())
        .WillByDefault(Return(m_projectChanged));

        ON_CALL(*m_trackeditProject, tracksChanged())
        .WillByDefault(Return(m_tracksChanged));
        ON_CALL(*m_trackeditProject, trackAdded())
        .WillByDefault(Return(m_trackAdded));
        ON_CALL(*m_trackeditProject, trackRemoved())
        .WillByDefault(Return(m_trackRemoved));
        ON_CALL(*m_trackeditProject, trackInserted())
        .WillByDefault(Return(m_trackInserted));
        ON_CALL(*m_trackeditProject, trackMoved())
        .WillByDefault(Return(m_trackMoved));

        //! NOTE The navigation controller channels the model subscribes to
        ON_CALL(*m_navigationController, navigationChanged())
        .WillByDefault(Return(m_navigationChanged));
        ON_CALL(*m_navigationController, isHighlight())
        .WillByDefault(Return(false));

        //! NOTE The tracks navigation controller channels the model subscribes to
        ON_CALL(*m_tracksNavigationController, focusedTrackChanged())
        .WillByDefault(Return(m_focusedTrackChanged));
        ON_CALL(*m_tracksNavigationController, focusedItemChanged())
        .WillByDefault(Return(m_focusedItemChanged));
        ON_CALL(*m_tracksNavigationController, focusedTrack())
        .WillByDefault(Return(INVALID_TRACK));

        //! NOTE The section is created with the test context directly
        m_section = new muse::ui::NavigationSection(m_testCtx);
        m_section->setName(SECTION_NAME);
        m_section->setOrder(1);
        m_section->componentComplete();

        m_model = new TrackNavigationModel();
        QQmlEngine::setContextForObject(m_model, m_engine.rootContext());
    }

    void TearDown() override
    {
        //! NOTE The controls reference the panels owned by the model, delete them first
        for (muse::ui::NavigationControl* control : m_controls) {
            delete control;
        }
        m_controls.clear();

        delete m_model;
        m_model = nullptr;

        delete m_section;
        m_section = nullptr;

        auto ioc = muse::modularity::ioc(m_testCtx);
        ioc->unregister<context::IGlobalContext>("utests");
        ioc->unregister<muse::ui::INavigationController>("utests");
        ioc->unregister<ITrackNavigationController>("utests");
        ioc->unregister<muse::actions::IActionsDispatcher>("utests");

        muse::modularity::removeIoC(m_testCtx);
    }

    //! NOTE Initialize the model and load the given tracks as the initial track list
    void loadWithTracks(const std::vector<Track>& tracks)
    {
        ON_CALL(*m_trackeditProject, trackList())
        .WillByDefault(Return(tracks));

        m_model->init(m_section);

        //! NOTE Triggers cleanup() + load(), which subscribes to the project and
        //! creates the panels for the initial track list
        m_projectChanged.notify();
    }

    static Track makeTrack(TrackId id, TrackType type = TrackType::Mono)
    {
        Track track;
        track.id = id;
        track.type = type;
        return track;
    }

    static QString trackPanelName(TrackId id) { return QString("Track %1 Panel").arg(id); }
    static QString headerPanelName(TrackId id) { return QString("Track %1 Header Panel").arg(id); }
    static QString itemsPanelName(TrackId id) { return QString("Track %1 Items Panel").arg(id); }

    //! NOTE Add an item control (a clip/label) to a panel, ordered by column, as QML does
    muse::ui::NavigationControl* addItemControl(muse::ui::NavigationPanel* panel, const QString& name, int column)
    {
        auto* control = new muse::ui::NavigationControl(m_testCtx);
        control->setName(name);
        control->setColumn(column);
        control->setEnabled(true);
        control->setPanel(panel);
        m_controls.push_back(control);
        return control;
    }

    //! NOTE Create a control that does not belong to the model's panels (e.g. the "Add track" button)
    muse::ui::NavigationControl* makeControl(const QString& name)
    {
        auto* control = new muse::ui::NavigationControl(m_testCtx);
        control->setName(name);
        control->setEnabled(true);
        m_controls.push_back(control);
        return control;
    }

    //! NOTE Keep track of the control the model sets as the default one of the navigation
    void trackDefaultNavigationControl()
    {
        EXPECT_CALL(*m_navigationController, setDefaultNavigationControl(_))
        .WillRepeatedly(SaveArg<0>(&m_defaultNavigationControl));
    }

    //! NOTE Simulate the navigation controller landing on a given panel/control
    void fireNavigationChanged(muse::ui::NavigationPanel* panel, muse::ui::NavigationControl* control)
    {
        ON_CALL(*m_navigationController, activePanel())
        .WillByDefault(Return(panel));
        ON_CALL(*m_navigationController, activeControl())
        .WillByDefault(Return(control));

        m_navigationChanged.notify();
    }

    //! NOTE Send the AboutActive event navigation system sends before it activates a panel, and return
    //! the control name the model asked to activate (empty when it leaves navigation system's default)
    static QString requestedControlName(muse::ui::NavigationPanel* panel)
    {
        auto event = muse::ui::INavigation::Event::make(muse::ui::INavigation::Event::AboutActive);
        panel->onEvent(event);
        return event->data.value("controlName").toString();
    }

    //! NOTE Deliver a navigation event (e.g. Escape) to a panel, as navigation system would
    static void sendPanelEvent(muse::ui::NavigationPanel* panel, muse::ui::INavigation::Event::Type type)
    {
        auto event = muse::ui::INavigation::Event::make(type);
        panel->onEvent(event);
    }

    QQmlEngine m_engine;
    muse::QmlIoCContext* m_qmlIoc = nullptr;

    std::shared_ptr<muse::modularity::Context> m_testCtx;
    muse::ui::NavigationSection* m_section = nullptr;
    TrackNavigationModel* m_model = nullptr;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<muse::ui::NavigationControllerMock> m_navigationController;
    std::shared_ptr<TrackNavigationControllerMock> m_tracksNavigationController;
    std::shared_ptr<muse::actions::ActionsDispatcherMock> m_dispatcher;
    std::shared_ptr<TrackeditProjectMock> m_trackeditProject;

    muse::async::Notification m_projectChanged;
    muse::async::Notification m_navigationChanged;
    muse::async::Channel<std::vector<Track> > m_tracksChanged;
    muse::async::Channel<Track> m_trackAdded;
    muse::async::Channel<Track> m_trackRemoved;
    muse::async::Channel<Track, int> m_trackInserted;
    muse::async::Channel<Track, int> m_trackMoved;
    muse::async::Channel<TrackId, bool> m_focusedTrackChanged;
    muse::async::Channel<TrackItemKey, bool> m_focusedItemChanged;

    std::vector<muse::ui::NavigationControl*> m_controls;

    muse::ui::INavigationControl* m_defaultNavigationControl = nullptr;
};

/**
 * Every track exposes three panels (track, header, clips/labels), the lists are
 * kept per track and the panel orders are laid out in blocks after the reserved
 * order 0 of the empty-project default panel.
 */
TEST_F(TrackNavigationModelTests, InitialLoadCreatesThreePanelsPerTrack)
{
    //! [GIVEN] A project with two tracks
    //! [WHEN] The model is loaded
    loadWithTracks({ makeTrack(10), makeTrack(20) });

    //! [THEN] There are three panel lists, one panel per track in each
    ASSERT_EQ(m_model->trackItemPanels().size(), 2);
    ASSERT_EQ(m_model->trackHeaderPanels().size(), 2);
    ASSERT_EQ(m_model->viewItemPanels().size(), 2);

    //! [AND] The panels are named after their tracks
    EXPECT_EQ(m_model->trackItemPanels().at(0)->name(), trackPanelName(10));
    EXPECT_EQ(m_model->trackHeaderPanels().at(0)->name(), headerPanelName(10));
    EXPECT_EQ(m_model->viewItemPanels().at(0)->name(), itemsPanelName(10));

    EXPECT_EQ(m_model->trackItemPanels().at(1)->name(), trackPanelName(20));

    //! [AND] The panel orders are laid out in blocks: base = 1 + 4 * pos
    EXPECT_EQ(m_model->trackItemPanels().at(0)->order(), 1);
    EXPECT_EQ(m_model->trackHeaderPanels().at(0)->order(), 2);
    EXPECT_EQ(m_model->viewItemPanels().at(0)->order(), 3);

    EXPECT_EQ(m_model->trackItemPanels().at(1)->order(), 5);
    EXPECT_EQ(m_model->trackHeaderPanels().at(1)->order(), 6);
    EXPECT_EQ(m_model->viewItemPanels().at(1)->order(), 7);
}

/**
 * Adding a track appends its three panels after the existing ones.
 */
TEST_F(TrackNavigationModelTests, TrackAddedAppendsPanels)
{
    //! [GIVEN] A project with one track
    loadWithTracks({ makeTrack(10) });
    ASSERT_EQ(m_model->trackItemPanels().size(), 1);

    //! [WHEN] A second track is added
    m_trackAdded.send(makeTrack(20));

    //! [THEN] Its panels are appended after the first track
    ASSERT_EQ(m_model->trackItemPanels().size(), 2);
    EXPECT_EQ(m_model->trackItemPanels().at(1)->name(), trackPanelName(20));
    EXPECT_EQ(m_model->trackItemPanels().at(1)->order(), 5);
    EXPECT_EQ(m_model->viewItemPanels().at(1)->order(), 7);
}

/**
 * Inserting a track places its panels at the position, the orders follow the
 * (new) track order rather than the insertion history.
 */
TEST_F(TrackNavigationModelTests, TrackInsertedPlacesPanelsAtPosition)
{
    //! [GIVEN] A project with two tracks
    loadWithTracks({ makeTrack(10), makeTrack(20) });

    //! [WHEN] A track is inserted between them
    m_trackInserted.send(makeTrack(15), 1);

    //! [THEN] The middle position holds the inserted track's panels
    ASSERT_EQ(m_model->trackItemPanels().size(), 3);
    EXPECT_EQ(m_model->trackItemPanels().at(0)->name(), trackPanelName(10));
    EXPECT_EQ(m_model->trackItemPanels().at(1)->name(), trackPanelName(15));
    EXPECT_EQ(m_model->trackItemPanels().at(2)->name(), trackPanelName(20));

    //! [AND] The orders are reassigned by position
    EXPECT_EQ(m_model->trackItemPanels().at(1)->order(), 5);
    EXPECT_EQ(m_model->trackItemPanels().at(2)->order(), 9);
}

/**
 * Removing a track drops its three panels and reorders the rest.
 */
TEST_F(TrackNavigationModelTests, TrackRemovedRemovesTrackPanels)
{
    //! [GIVEN] A project with three tracks
    loadWithTracks({ makeTrack(10), makeTrack(20), makeTrack(30) });

    //! [WHEN] The middle track is removed
    m_trackRemoved.send(makeTrack(20));

    //! [THEN] Only the remaining tracks keep their panels
    ASSERT_EQ(m_model->trackItemPanels().size(), 2);
    ASSERT_EQ(m_model->viewItemPanels().size(), 2);
    EXPECT_EQ(m_model->trackItemPanels().at(0)->name(), trackPanelName(10));
    EXPECT_EQ(m_model->trackItemPanels().at(1)->name(), trackPanelName(30));

    //! [AND] The orders are compacted by the new positions
    EXPECT_EQ(m_model->trackItemPanels().at(1)->order(), 5);
}

/**
 * Moving a track moves its panels and reorders all of them by the new positions.
 */
TEST_F(TrackNavigationModelTests, TrackMovedReordersPanels)
{
    //! [GIVEN] A project with three tracks
    loadWithTracks({ makeTrack(10), makeTrack(20), makeTrack(30) });

    //! [WHEN] The first track is moved to the last position
    m_trackMoved.send(makeTrack(10), 2);

    //! [THEN] The panel lists follow the new track order
    ASSERT_EQ(m_model->trackItemPanels().size(), 3);
    EXPECT_EQ(m_model->trackItemPanels().at(0)->name(), trackPanelName(20));
    EXPECT_EQ(m_model->trackItemPanels().at(1)->name(), trackPanelName(30));
    EXPECT_EQ(m_model->trackItemPanels().at(2)->name(), trackPanelName(10));

    //! [AND] The moved track takes the last order block
    EXPECT_EQ(m_model->trackItemPanels().at(2)->order(), 9);
}

/**
 * When the navigation lands on a clips/labels panel (e.g. via Tab or mouse), the
 * model reflects the active item back into the tracks navigation controller.
 */
TEST_F(TrackNavigationModelTests, NavigationOnItemsPanelFocusesItem)
{
    //! [GIVEN] A project with one track
    loadWithTracks({ makeTrack(10) });

    muse::ui::NavigationPanel* itemsPanel = m_model->viewItemPanels().at(0);

    //! [AND] An active clip control on the track's clips panel
    auto* clipControl = new muse::ui::NavigationControl(m_testCtx);
    clipControl->setName("200");
    clipControl->setPanel(itemsPanel);
    clipControl->setEnabled(true);

    ON_CALL(*m_navigationController, activePanel())
    .WillByDefault(Return(itemsPanel));
    ON_CALL(*m_navigationController, activeControl())
    .WillByDefault(Return(clipControl));

    //! [EXPECT] The focused item is set to that clip on that track
    EXPECT_CALL(*m_tracksNavigationController, setFocusedItem(TrackItemKey { 10, 200 }, _)).Times(1);

    //! [WHEN] The navigation changes
    m_navigationChanged.notify();

    delete clipControl;
}

/**
 * When the navigation lands on a track or header panel, the model focuses the
 * track with no item.
 */
TEST_F(TrackNavigationModelTests, NavigationOnTrackPanelFocusesTrackWithoutItem)
{
    //! [GIVEN] A project with one track
    loadWithTracks({ makeTrack(10) });

    muse::ui::NavigationPanel* trackPanel = m_model->trackItemPanels().at(0);

    ON_CALL(*m_navigationController, activePanel())
    .WillByDefault(Return(trackPanel));
    ON_CALL(*m_navigationController, activeControl())
    .WillByDefault(Return(nullptr));

    //! [EXPECT] The track is focused, without an item
    EXPECT_CALL(*m_tracksNavigationController, setFocusedItem(TrackItemKey { 10, INVALID_TRACK_ITEM }, _)).Times(1);

    //! [WHEN] The navigation changes
    m_navigationChanged.notify();
}

/**
 * Entering the clips/labels panel while navigating backwards (Shift+Tab, i.e. from a
 * higher panel) asks navigation system to activate the last item instead of its default first one.
 */
TEST_F(TrackNavigationModelTests, BackwardsIntoItemsPanelRequestsLastItem)
{
    //! [GIVEN] A project with two tracks, the first track's clips panel has two clips
    loadWithTracks({ makeTrack(10), makeTrack(20) });

    muse::ui::NavigationPanel* itemsPanel = m_model->viewItemPanels().at(0);
    addItemControl(itemsPanel, "100", 0);
    addItemControl(itemsPanel, "200", 4);

    //! [AND] The navigation currently sits on the second track (a higher panel order)
    fireNavigationChanged(m_model->trackItemPanels().at(1), nullptr);

    //! [WHEN] The navigation enters the first track's clips panel (Shift+Tab)
    //! [THEN] The model asks to activate the last clip
    EXPECT_EQ(requestedControlName(itemsPanel), QString("200"));
}

/**
 * Entering the clips/labels panel while navigating forwards (Tab, i.e. from a lower
 * panel) leaves muse's default, which activates the first item.
 */
TEST_F(TrackNavigationModelTests, ForwardsIntoItemsPanelKeepsFirstItem)
{
    //! [GIVEN] A project with one track whose clips panel has two clips
    loadWithTracks({ makeTrack(10) });

    muse::ui::NavigationPanel* itemsPanel = m_model->viewItemPanels().at(0);
    addItemControl(itemsPanel, "100", 0);
    addItemControl(itemsPanel, "200", 4);

    //! [AND] The navigation currently sits on the same track's header (a lower panel order)
    fireNavigationChanged(m_model->trackHeaderPanels().at(0), nullptr);

    //! [WHEN] The navigation enters the clips panel (Tab)
    //! [THEN] The model does not override the control, navigation system keeps its first-item default
    EXPECT_TRUE(requestedControlName(itemsPanel).isEmpty());
}

/**
 * Escape from a control of a track's header panel returns the navigation focus
 * to that track's container panel (with the highlight kept on).
 */
TEST_F(TrackNavigationModelTests, EscapeFromHeaderReturnsFocusToTrack)
{
    //! [GIVEN] A project with one track
    loadWithTracks({ makeTrack(10) });

    muse::ui::NavigationPanel* trackPanel = m_model->trackItemPanels().at(0);
    muse::ui::NavigationPanel* headerPanel = m_model->trackHeaderPanels().at(0);

    //! [AND] The track container panel has an enabled control to land on
    addItemControl(trackPanel, "track-10", 0);

    //! [EXPECT] The model keeps the highlight and activates the track container control
    EXPECT_CALL(*m_navigationController, setIsHighlight(true)).Times(1);
    EXPECT_CALL(*m_navigationController, requestActivateByName(
                    std::string(SECTION_NAME), trackPanelName(10).toStdString(), std::string("track-10"))).Times(1);

    //! [WHEN] Escape is pressed on the header panel
    sendPanelEvent(headerPanel, muse::ui::INavigation::Event::Escape);
}

/**
 * The control the navigation falls back to (Escape, navigation reset) is the first
 * track of the project; until its control is created by QML the "Add track" button is kept.
 */
TEST_F(TrackNavigationModelTests, DefaultNavigationControlIsFirstTrack)
{
    //! [GIVEN] The "Add track" button is set as the control to fall back to
    muse::ui::NavigationControl* addTrackControl = makeControl("AddTrack");
    m_model->setFallbackNavigationControl(addTrackControl);

    //! [AND] The default control set on the navigation controller is tracked
    trackDefaultNavigationControl();

    //! [WHEN] A project with two tracks is loaded
    loadWithTracks({ makeTrack(10), makeTrack(20) });

    //! [THEN] The tracks have no controls yet (QML creates them later), the button is kept
    EXPECT_EQ(m_defaultNavigationControl, addTrackControl);

    //! [WHEN] The tracks get their controls
    muse::ui::NavigationControl* firstTrackControl = addItemControl(m_model->trackItemPanels().at(0), "track-10", 0);
    addItemControl(m_model->trackItemPanels().at(1), "track-20", 0);

    //! [THEN] The first track becomes the default control
    EXPECT_EQ(m_defaultNavigationControl, firstTrackControl);
}

/**
 * A project without tracks falls back to the "Add track" button.
 */
TEST_F(TrackNavigationModelTests, DefaultNavigationControlIsAddTrackWhenNoTracks)
{
    //! [GIVEN] The "Add track" button is set as the control to fall back to
    muse::ui::NavigationControl* addTrackControl = makeControl("AddTrack");
    m_model->setFallbackNavigationControl(addTrackControl);

    trackDefaultNavigationControl();

    //! [WHEN] A project without tracks is loaded
    loadWithTracks({});

    //! [THEN] The default control is the "Add track" button
    EXPECT_EQ(m_defaultNavigationControl, addTrackControl);
}

/**
 * The default control follows the tracks list: removing the first track moves it to the
 * track that became first, removing the last one returns it to the "Add track" button.
 */
TEST_F(TrackNavigationModelTests, DefaultNavigationControlFollowsTracksList)
{
    //! [GIVEN] A project with two tracks, both with their controls created
    muse::ui::NavigationControl* addTrackControl = makeControl("AddTrack");
    m_model->setFallbackNavigationControl(addTrackControl);

    loadWithTracks({ makeTrack(10), makeTrack(20) });

    addItemControl(m_model->trackItemPanels().at(0), "track-10", 0);
    muse::ui::NavigationControl* secondTrackControl = addItemControl(m_model->trackItemPanels().at(1), "track-20", 0);

    trackDefaultNavigationControl();

    //! [WHEN] The first track is removed
    m_trackRemoved.send(makeTrack(10));

    //! [THEN] The default control is the control of the track that became first
    EXPECT_EQ(m_defaultNavigationControl, secondTrackControl);

    //! [WHEN] The last track is removed
    m_trackRemoved.send(makeTrack(20));

    //! [THEN] The default control is the "Add track" button again
    EXPECT_EQ(m_defaultNavigationControl, addTrackControl);
}
}
