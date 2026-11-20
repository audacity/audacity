/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <map>

#include <QTemporaryDir>

#include "global/async/processevents.h"
#include "global/modularity/ioc.h"
#include "global/iapplication.h"
#include "global/iglobalconfiguration.h"
#include "global/tests/mocks/applicationmock.h"
#include "global/tests/mocks/globalconfigurationmock.h"
#include "rcommand/tests/mocks/commanddispatchermock.h"
#include "shortcuts_v2/tests/mocks/commandshortcutsregistermock.h"

#include "macromanager/macromanagercommands.h"
#include "macromanager/internal/macroscommandscontroller.h"
#include "macromanager/internal/macrosrepository.h"

#include "mocks/macrorunnermock.h"

using namespace muse;
using namespace muse::rcommand;
using namespace au::macromanager;

using ::testing::_;
using ::testing::AtLeast;
using ::testing::NiceMock;
using ::testing::Return;

class MacroManager_MacrosCommandsControllerTests : public ::testing::Test
{
protected:
    static constexpr const char* MODULE = "macromanager_tests";

    void SetUp() override
    {
        ASSERT_TRUE(m_directory.isValid());

        m_globalConfiguration = std::make_shared<NiceMock<GlobalConfigurationMock> >();
        ON_CALL(*m_globalConfiguration, userAppDataPath()).WillByDefault(Return(io::path_t(m_directory.path())));
        ON_CALL(*m_globalConfiguration, appDataPath()).WillByDefault(Return(io::path_t(m_directory.path())));

        m_application = std::make_shared<NiceMock<ApplicationMock> >();
        ON_CALL(*m_application, fullVersion()).WillByDefault(Return(Version(4, 1, 0)));

        m_shortcutsRegister = std::make_shared<NiceMock<shortcuts::CommandShortcutsRegisterMock> >();

        auto ioc = modularity::globalIoc();
        ioc->unregister<IGlobalConfiguration>(MODULE);
        ioc->registerExport<IGlobalConfiguration>(MODULE, m_globalConfiguration);
        ioc->unregister<IApplication>(MODULE);
        ioc->registerExport<IApplication>(MODULE, m_application);
        ioc->unregister<shortcuts::ICommandShortcutsRegister>(MODULE);
        ioc->registerExport<shortcuts::ICommandShortcutsRegister>(MODULE, m_shortcutsRegister);

        m_repository = std::make_shared<MacrosRepository>();
        m_repository->init();
        ioc->unregister<IMacrosRepository>(MODULE);
        ioc->registerExport<IMacrosRepository>(MODULE, m_repository);

        m_dispatcher = std::make_shared<NiceMock<CommandDispatcherMock> >();
        ON_CALL(*m_dispatcher, unreg(_)).WillByDefault([this](Commandable*) {
            m_handlers.clear();
        });
        ON_CALL(*m_dispatcher, onRequest(_, _, ::testing::Matcher<const ICommandDispatcher::AsyncCallBack&>(_)))
        .WillByDefault([this](Commandable*, const Command& command, const ICommandDispatcher::AsyncCallBack& callback) {
            m_handlers[command] = callback;
        });

        m_runner = std::make_shared<NiceMock<MacroRunnerMock> >();
        ON_CALL(*m_runner, run(_)).WillByDefault([](const Macro&) {
            return async::make_promise<Ret>([](auto resolve) {
                return resolve(make_ok());
            });
        });

        modularity::ioc(m_context)->registerExport<ICommandDispatcher>(MODULE, m_dispatcher);
        modularity::ioc(m_context)->registerExport<IMacroRunner>(MODULE, m_runner);
    }

    void TearDown() override
    {
        m_controller.reset();
        modularity::removeIoC(m_context);

        auto ioc = modularity::globalIoc();
        ioc->unregister<IMacrosRepository>(MODULE);
        ioc->unregister<shortcuts::ICommandShortcutsRegister>(MODULE);
        ioc->unregister<IApplication>(MODULE);
        ioc->unregister<IGlobalConfiguration>(MODULE);
    }

    void createController()
    {
        m_controller = std::make_unique<MacrosCommandsController>(m_context);
        m_controller->init();
    }

    std::optional<Ret> dispatch(const Command& command)
    {
        const auto it = m_handlers.find(command);
        if (it == m_handlers.end()) {
            return std::nullopt;
        }

        std::optional<Ret> result;
        it->second(make_request(command, {}), [&result](const Response& response) {
            result = response.ret;
        });
        async::processMessages();
        return result;
    }

    QTemporaryDir m_directory;
    const modularity::ContextPtr m_context = std::make_shared<modularity::Context>(1);
    std::shared_ptr<GlobalConfigurationMock> m_globalConfiguration;
    std::shared_ptr<ApplicationMock> m_application;
    std::shared_ptr<shortcuts::CommandShortcutsRegisterMock> m_shortcutsRegister;
    std::shared_ptr<MacrosRepository> m_repository;
    std::shared_ptr<CommandDispatcherMock> m_dispatcher;
    std::shared_ptr<MacroRunnerMock> m_runner;
    std::map<Command, ICommandDispatcher::AsyncCallBack> m_handlers;
    std::unique_ptr<MacrosCommandsController> m_controller;
};

TEST_F(MacroManager_MacrosCommandsControllerTests, DispatchRunsTheMacroOfTheCommand)
{
    ASSERT_TRUE(m_repository->save(Macro { "Clean up", {} }));
    ASSERT_TRUE(m_repository->save(Macro { "Master", {} }));
    createController();

    EXPECT_CALL(*m_runner, run(::testing::Field(&Macro::name, "Master"))).Times(1);

    const std::optional<Ret> ret = dispatch(makeMacroRunCommand("Master"));

    ASSERT_TRUE(ret.has_value());
    EXPECT_TRUE(*ret) << ret->toString();
}

TEST_F(MacroManager_MacrosCommandsControllerTests, HandlersAndShortcutsFollowMacroChanges)
{
    createController();

    EXPECT_CALL(*m_shortcutsRegister, reload(_)).Times(AtLeast(1));
    ASSERT_TRUE(m_repository->save(Macro { "Clean up", {} }));
    EXPECT_TRUE(dispatch(makeMacroRunCommand("Clean up")).has_value());
    ::testing::Mock::VerifyAndClearExpectations(m_shortcutsRegister.get());

    ASSERT_TRUE(m_repository->rename("Clean up", "Master"));
    EXPECT_FALSE(dispatch(makeMacroRunCommand("Clean up")).has_value());
    EXPECT_TRUE(dispatch(makeMacroRunCommand("Master")).has_value());
}
