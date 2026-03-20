/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <QEventLoop>
#include <QTimer>

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "global/tests/mocks/applicationmock.h"
#include "interactive/tests/mocks/interactivemock.h"

#include "mocks/appshellconfigurationmock.h"
#include "mocks/projectfilescontrollermock.h"
#include "mocks/multiwindowsprovidermock.h"

#include "../internal/applicationactioncontroller.h"

using ::testing::_;
using ::testing::Return;
using ::testing::NiceMock;
using ::testing::HasSubstr;

using namespace muse;
using namespace au::appshell;

namespace au::appshell {
class FactoryResetActionTests : public ::testing::Test
{
public:
    using ActionCB = muse::actions::IActionsDispatcher::ActionCallBackWithNameAndData;
    using WarningPromise = muse::async::Promise<IInteractive::Result>;

    void SetUp() override
    {
        m_controller = new ApplicationActionController(muse::modularity::globalCtx());

        m_dispatcher = std::make_shared<NiceMock<muse::actions::ActionsDispatcherMock> >();
        m_controller->dispatcher.set(m_dispatcher);

        ON_CALL(*m_dispatcher, reg(_, _, testing::An<const ActionCB&>()))
        .WillByDefault([this](muse::actions::Actionable*, const muse::actions::ActionCode& code,
                              const ActionCB& cb) {
            m_registeredActions[code] = cb;
        });

        m_interactive = std::make_shared<NiceMock<InteractiveMock> >();
        m_controller->interactive.set(m_interactive);

        ON_CALL(*m_interactive, buttonData(IInteractive::Button::Cancel))
        .WillByDefault(Return(IInteractive::ButtonData(IInteractive::Button::Cancel, "Cancel")));

        m_application = std::make_shared<NiceMock<ApplicationMock> >();
        m_controller->application.set(m_application);

        m_configuration = std::make_shared<NiceMock<AppShellConfigurationMock> >();
        m_controller->configuration.set(m_configuration);

        m_projectFilesController = std::make_shared<NiceMock<au::project::ProjectFilesControllerMock> >();
        m_controller->projectFilesController.set(m_projectFilesController);

        m_multiWindowsProvider = std::make_shared<NiceMock<muse::mi::MultiWindowsProviderMock> >();
        m_controller->multiwindowsProvider.set(m_multiWindowsProvider);

        setupWarningDialog();

        m_controller->init();
    }

    void TearDown() override
    {
        delete m_controller;
    }

    void setupWarningDialog()
    {
        EXPECT_CALL(*m_interactive, warning(
                        HasSubstr("revert to factory settings"),
                        _, _,
                        static_cast<int>(IInteractive::Button::Apply),
                        _, _))
        .WillOnce([this](const std::string&, const IInteractive::Text&,
                         const IInteractive::ButtonDatas&, int,
                         const IInteractive::Options&, const std::string&) -> WarningPromise {
            return WarningPromise([this](WarningPromise::Resolve resolve,
                                         WarningPromise::Reject) -> WarningPromise::Result {
                m_warningResolve = resolve;
                return WarningPromise::Result::unchecked();
            }, muse::async::PromiseType::AsyncByBody);
        });
    }

    void triggerRevertToFactory()
    {
        auto it = m_registeredActions.find("revert-factory");
        ASSERT_NE(it, m_registeredActions.end()) << "'revert-factory' action not registered";
        it->second("revert-factory", muse::actions::ActionData());
    }

    void confirmRevertDialog()
    {
        int btn = static_cast<int>(IInteractive::Button::Apply);
        IInteractive::Result result(btn);
        (void)m_warningResolve(result);
    }

    void cancelRevertDialog()
    {
        int btn = static_cast<int>(IInteractive::Button::Cancel);
        IInteractive::Result result(btn);
        (void)m_warningResolve(result);
    }

    //! Run the Qt event loop for the given duration so QTimer::singleShot can fire.
    void processEventsFor(int ms)
    {
        QEventLoop loop;
        QTimer::singleShot(ms, &loop, &QEventLoop::quit);
        loop.exec();
    }

protected:
    ApplicationActionController* m_controller = nullptr;

    std::shared_ptr<NiceMock<muse::actions::ActionsDispatcherMock> > m_dispatcher;
    std::shared_ptr<NiceMock<InteractiveMock> > m_interactive;
    std::shared_ptr<NiceMock<ApplicationMock> > m_application;
    std::shared_ptr<NiceMock<AppShellConfigurationMock> > m_configuration;
    std::shared_ptr<NiceMock<au::project::ProjectFilesControllerMock> > m_projectFilesController;
    std::shared_ptr<NiceMock<muse::mi::MultiWindowsProviderMock> > m_multiWindowsProvider;

    std::map<std::string, ActionCB> m_registeredActions;
    WarningPromise::Resolve m_warningResolve;
};

/**
 * @brief Cancel the factory reset confirmation dialog
 * @details User triggers factory reset but cancels the warning dialog.
 *          No side effects should occur.
 */
TEST_F(FactoryResetActionTests, CancelDialog_DoesNothing)
{
    //! [GIVEN] Single window
    ON_CALL(*m_multiWindowsProvider, windowCount())
    .WillByDefault(Return(size_t(1)));

    //! [THEN] No project close, no reset mode, no restart
    EXPECT_CALL(*m_projectFilesController, closeOpenedProject(_)).Times(0);
    EXPECT_CALL(*m_configuration, setFactoryResetMode(_)).Times(0);
    EXPECT_CALL(*m_application, restart()).Times(0);

    //! [WHEN] User triggers factory reset and cancels the dialog
    triggerRevertToFactory();
    cancelRevertDialog();
}

/**
 * @brief Single window, project closes successfully (clean or user saved)
 * @details After confirming, the project is closed and the app restarts
 *          with factory reset mode set.
 */
TEST_F(FactoryResetActionTests, SingleWindow_CloseSucceeds_Restarts)
{
    //! [GIVEN] Single window
    ON_CALL(*m_multiWindowsProvider, windowCount())
    .WillByDefault(Return(size_t(1)));

    //! [GIVEN] Project closes successfully (quitApp=false, not a quit)
    EXPECT_CALL(*m_projectFilesController, closeOpenedProject(false))
    .WillOnce(Return(true));

    //! [THEN] Factory reset mode is set to Full
    EXPECT_CALL(*m_configuration, setFactoryResetMode(FactoryResetMode::Full))
    .Times(1);

    //! [THEN] Application restarts
    EXPECT_CALL(*m_application, restart())
    .Times(1);

    //! [THEN] quitForAll is NOT called (single window)
    EXPECT_CALL(*m_multiWindowsProvider, quitForAll()).Times(0);

    //! [WHEN] User triggers and confirms factory reset
    triggerRevertToFactory();
    confirmRevertDialog();
}

/**
 * @brief Single window, user cancels the save dialog
 * @details The project has unsaved changes. User confirms factory reset
 *          but cancels the save dialog. No restart should occur.
 */
TEST_F(FactoryResetActionTests, SingleWindow_CloseCancelled_NoRestart)
{
    //! [GIVEN] Single window
    ON_CALL(*m_multiWindowsProvider, windowCount())
    .WillByDefault(Return(size_t(1)));

    //! [GIVEN] User cancels the save dialog (quitApp=false)
    EXPECT_CALL(*m_projectFilesController, closeOpenedProject(false))
    .WillOnce(Return(false));

    //! [THEN] No factory reset mode set, no restart
    EXPECT_CALL(*m_configuration, setFactoryResetMode(_)).Times(0);
    EXPECT_CALL(*m_application, restart()).Times(0);
    EXPECT_CALL(*m_multiWindowsProvider, quitForAll()).Times(0);

    //! [WHEN] User triggers and confirms factory reset
    triggerRevertToFactory();
    confirmRevertDialog();
}

/**
 * @brief Multiple windows, user cancels save dialog for current project
 * @details The current project has unsaved changes. User cancels the save
 *          dialog. No factory reset or quit should occur.
 */
TEST_F(FactoryResetActionTests, MultiWindow_CloseCancelled_NoAction)
{
    //! [GIVEN] Multiple windows open
    ON_CALL(*m_multiWindowsProvider, windowCount())
    .WillByDefault(Return(size_t(3)));

    //! [GIVEN] User cancels save dialog (quitApp=false)
    EXPECT_CALL(*m_projectFilesController, closeOpenedProject(false))
    .WillOnce(Return(false));

    //! [THEN] No factory reset mode, no quit, no restart
    EXPECT_CALL(*m_configuration, setFactoryResetMode(_)).Times(0);
    EXPECT_CALL(*m_multiWindowsProvider, quitForAll()).Times(0);
    EXPECT_CALL(*m_application, restart()).Times(0);

    //! [WHEN] User triggers and confirms factory reset
    triggerRevertToFactory();
    confirmRevertDialog();
}

/**
 * @brief Multiple windows: waits for other windows to close via QTimer polling,
 *        then restarts.
 * @details After quitForAll(), waitForOtherWindowsToCloseAndRestart() polls
 *          windowCount() every 200ms via QTimer::singleShot. When the count
 *          drops to 1, it calls restart(). This test exercises the actual
 *          timer-based wait by running the Qt event loop.
 */
TEST_F(FactoryResetActionTests, MultiWindow_WaitsForWindowsToClose_ThenRestarts)
{
    //! [GIVEN] windowCount sequence:
    //!   Call 1: in revertToFactorySettings → 2 (multi-window path)
    //!   Call 2: in waitFor... → 2 (still waiting, schedule QTimer)
    //!   Call 3: QTimer fires, in waitFor... → 1 (all closed, call restart)
    EXPECT_CALL(*m_multiWindowsProvider, windowCount())
    .WillOnce(Return(size_t(2)))
    .WillOnce(Return(size_t(2)))
    .WillOnce(Return(size_t(1)));

    //! [GIVEN] Current project closes successfully (quitApp=false, not a quit)
    EXPECT_CALL(*m_projectFilesController, closeOpenedProject(false))
    .WillOnce(Return(true));

    //! [THEN] Factory reset mode is set
    EXPECT_CALL(*m_configuration, setFactoryResetMode(FactoryResetMode::Full))
    .Times(1);

    //! [THEN] quitForAll is called to tell other windows to close
    EXPECT_CALL(*m_multiWindowsProvider, quitForAll())
    .Times(1);

    //! [THEN] restart is called after the timer fires and windowCount drops
    EXPECT_CALL(*m_application, restart())
    .Times(1);

    //! [WHEN] User triggers and confirms factory reset
    triggerRevertToFactory();
    confirmRevertDialog();

    //! [WHEN] Process Qt events so the QTimer::singleShot(200ms) fires
    processEventsFor(500);
}

/**
 * @brief Multiple windows: polls through several timer iterations before restart
 * @details Exercises multiple iterations of the 200ms polling loop before
 *          windowCount finally drops to 1.
 */
TEST_F(FactoryResetActionTests, MultiWindow_PollsMultipleTimesBeforeRestart)
{
    //! [GIVEN] windowCount stays > 1 for several polls, then drops
    //!   Call 1: in revertToFactorySettings → 3 (multi-window path)
    //!   Call 2: waitFor... → 3 (schedule timer)
    //!   Call 3: timer fires, waitFor... → 2 (schedule timer again)
    //!   Call 4: timer fires, waitFor... → 1 (done, restart)
    EXPECT_CALL(*m_multiWindowsProvider, windowCount())
    .WillOnce(Return(size_t(3)))
    .WillOnce(Return(size_t(3)))
    .WillOnce(Return(size_t(2)))
    .WillOnce(Return(size_t(1)));

    //! [GIVEN] Current project closes successfully (quitApp=false, not a quit)
    EXPECT_CALL(*m_projectFilesController, closeOpenedProject(false))
    .WillOnce(Return(true));

    //! [THEN] Factory reset mode is set
    EXPECT_CALL(*m_configuration, setFactoryResetMode(FactoryResetMode::Full))
    .Times(1);

    //! [THEN] quitForAll is called once
    EXPECT_CALL(*m_multiWindowsProvider, quitForAll())
    .Times(1);

    //! [THEN] restart is called exactly once after all windows close
    EXPECT_CALL(*m_application, restart())
    .Times(1);

    //! [WHEN] User triggers and confirms factory reset
    triggerRevertToFactory();
    confirmRevertDialog();

    //! [WHEN] Process Qt events for enough time to cover multiple 200ms polls
    processEventsFor(1000);
}
}
