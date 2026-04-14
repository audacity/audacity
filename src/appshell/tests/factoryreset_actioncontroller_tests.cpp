/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "global/tests/mocks/applicationmock.h"
#include "interactive/tests/mocks/interactivemock.h"

#include "mocks/appshellconfigurationmock.h"
#include "project/tests/mocks/projectfilescontrollermock.h"
#include "multiwindows/tests/mocks/multiwindowsprovidermock.h"

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
    using QuestionPromise = muse::async::Promise<IInteractive::Result>;

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
                        static_cast<int>(IInteractive::Button::Cancel),
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

    void setupRestartDialog()
    {
        EXPECT_CALL(*m_interactive, question(
                        HasSubstr("restart Audacity"),
                        _, _, _, _, _))
        .WillOnce([this](const std::string&, const IInteractive::Text&,
                         const IInteractive::ButtonDatas&, int,
                         const IInteractive::Options&, const std::string&) -> QuestionPromise {
            return QuestionPromise([this](QuestionPromise::Resolve resolve,
                                          QuestionPromise::Reject) -> QuestionPromise::Result {
                m_questionResolve = resolve;
                return QuestionPromise::Result::unchecked();
            }, muse::async::PromiseType::AsyncByBody);
        });
    }

    void triggerRevertToFactory()
    {
        auto it = m_registeredActions.find("revert-factory");
        ASSERT_NE(it, m_registeredActions.end()) << "'revert-factory' action not registered";
        it->second("revert-factory", muse::actions::ActionData());
    }

    void confirmWarningDialog()
    {
        int btn = static_cast<int>(IInteractive::Button::Apply);
        IInteractive::Result result(btn);
        (void)m_warningResolve(result);
    }

    void cancelWarningDialog()
    {
        int btn = static_cast<int>(IInteractive::Button::Cancel);
        IInteractive::Result result(btn);
        (void)m_warningResolve(result);
    }

    void confirmRestartDialog()
    {
        int btn = static_cast<int>(IInteractive::Button::Apply);
        IInteractive::Result result(btn);
        (void)m_questionResolve(result);
    }

    void cancelRestartDialog()
    {
        int btn = static_cast<int>(IInteractive::Button::Cancel);
        IInteractive::Result result(btn);
        (void)m_questionResolve(result);
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
    QuestionPromise::Resolve m_questionResolve;
};

/**
 * @brief Cancel the warning dialog — nothing should happen.
 */
TEST_F(FactoryResetActionTests, CancelWarningDialog_DoesNothing)
{
    //! [THEN] No reset, no restart
    EXPECT_CALL(*m_configuration, revertToFactorySettings(_, _, _)).Times(0);
    EXPECT_CALL(*m_application, restart()).Times(0);

    //! [WHEN] User triggers factory reset and cancels the warning
    triggerRevertToFactory();
    cancelWarningDialog();
}

/**
 * @brief Confirm the warning — settings are reset immediately.
 */
TEST_F(FactoryResetActionTests, ConfirmWarning_ResetsSettings)
{
    //! [GIVEN] Restart dialog will appear
    setupRestartDialog();

    //! [THEN] Settings are reset with keepDefault=false
    EXPECT_CALL(*m_configuration, revertToFactorySettings(false, false, false))
    .Times(1);

    //! [WHEN] User confirms the warning
    triggerRevertToFactory();
    confirmWarningDialog();

    //! [WHEN] User cancels the restart dialog
    cancelRestartDialog();
}

/**
 * @brief Confirm warning, cancel restart dialog — reset done but no restart.
 */
TEST_F(FactoryResetActionTests, ConfirmWarning_CancelRestart_NoRestart)
{
    //! [GIVEN] Restart dialog will appear
    setupRestartDialog();

    //! [THEN] Settings are reset
    EXPECT_CALL(*m_configuration, revertToFactorySettings(false, false, false))
    .Times(1);

    //! [THEN] No restart, no project close
    EXPECT_CALL(*m_projectFilesController, closeOpenedProject(_)).Times(0);
    EXPECT_CALL(*m_application, restart()).Times(0);

    //! [WHEN] User confirms warning, cancels restart
    triggerRevertToFactory();
    confirmWarningDialog();
    cancelRestartDialog();
}

/**
 * @brief Confirm both dialogs, single window — restart via application()->restart().
 */
TEST_F(FactoryResetActionTests, ConfirmRestart_SingleWindow_Restarts)
{
    //! [GIVEN] Restart dialog will appear
    setupRestartDialog();

    //! [GIVEN] Single window, project closes successfully
    ON_CALL(*m_multiWindowsProvider, windowCount())
    .WillByDefault(Return(size_t(1)));

    EXPECT_CALL(*m_projectFilesController, closeOpenedProject(false))
    .WillOnce(Return(true));

    //! [THEN] Settings are reset and app restarts
    EXPECT_CALL(*m_configuration, revertToFactorySettings(false, false, false))
    .Times(1);

    EXPECT_CALL(*m_application, restart())
    .Times(1);

    //! [WHEN] User confirms both dialogs
    triggerRevertToFactory();
    confirmWarningDialog();
    confirmRestartDialog();
}

/**
 * @brief Confirm both dialogs, multi-window — uses quitAllAndRestartLast().
 */
TEST_F(FactoryResetActionTests, ConfirmRestart_MultiWindow_QuitsAll)
{
    //! [GIVEN] Restart dialog will appear
    setupRestartDialog();

    //! [GIVEN] Multiple windows, project closes successfully
    ON_CALL(*m_multiWindowsProvider, windowCount())
    .WillByDefault(Return(size_t(3)));

    EXPECT_CALL(*m_projectFilesController, closeOpenedProject(false))
    .WillOnce(Return(true));

    //! [THEN] Settings are reset
    EXPECT_CALL(*m_configuration, revertToFactorySettings(false, false, false))
    .Times(1);

    //! [THEN] quitAllAndRestartLast is called (not application()->restart())
    EXPECT_CALL(*m_multiWindowsProvider, quitAllAndRestartLast())
    .Times(1);

    EXPECT_CALL(*m_application, restart()).Times(0);

    //! [WHEN] User confirms both dialogs
    triggerRevertToFactory();
    confirmWarningDialog();
    confirmRestartDialog();
}

/**
 * @brief Confirm both dialogs, but user rejects project close — no restart.
 */
TEST_F(FactoryResetActionTests, ConfirmRestart_ProjectCloseRejected_NoRestart)
{
    //! [GIVEN] Restart dialog will appear
    setupRestartDialog();

    //! [GIVEN] Project close is rejected by user
    EXPECT_CALL(*m_projectFilesController, closeOpenedProject(false))
    .WillOnce(Return(false));

    //! [THEN] Settings are still reset (happens before restart attempt)
    EXPECT_CALL(*m_configuration, revertToFactorySettings(false, false, false))
    .Times(1);

    //! [THEN] No restart occurs
    EXPECT_CALL(*m_application, restart()).Times(0);
    EXPECT_CALL(*m_multiWindowsProvider, quitAllAndRestartLast()).Times(0);

    //! [WHEN] User confirms both dialogs
    triggerRevertToFactory();
    confirmWarningDialog();
    confirmRestartDialog();
}
}
