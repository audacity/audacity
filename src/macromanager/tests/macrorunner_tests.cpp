/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <QTemporaryDir>

#include "global/modularity/ioc.h"
#include "global/iapplication.h"
#include "global/iglobalconfiguration.h"
#include "global/tests/mocks/applicationmock.h"
#include "global/tests/mocks/globalconfigurationmock.h"

#include "macromanager/internal/macrorunner.h"
#include "macromanager/internal/macrosrepository.h"

#include "mocks/commandsregistermock.h"

using namespace muse;
using namespace muse::rcommand;
using namespace au::macromanager;

using ::testing::_;
using ::testing::HasSubstr;
using ::testing::NiceMock;
using ::testing::Return;
using ::testing::ReturnRef;
using ::testing::Truly;

namespace {
const Command KNOWN_COMMAND("command://test/known");
const Command UNKNOWN_COMMAND("command://test/unknown");

size_t occurrences(const std::string& text, const std::string& part)
{
    size_t count = 0;
    for (size_t pos = text.find(part); pos != std::string::npos; pos = text.find(part, pos + part.size())) {
        ++count;
    }
    return count;
}
}

class MacroManager_MacroRunnerTests : public ::testing::Test
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

        m_knownInfo.command = KNOWN_COMMAND;
        m_commandsRegister = std::make_shared<NiceMock<CommandsRegisterMock> >();
        ON_CALL(*m_commandsRegister, commandInfo(_)).WillByDefault(ReturnRef(m_unknownInfo));
        ON_CALL(*m_commandsRegister, commandInfo(Truly([](const Command& command) {
            return command.toString() == KNOWN_COMMAND.toString();
        }))).WillByDefault(ReturnRef(m_knownInfo));

        auto ioc = modularity::globalIoc();
        ioc->unregister<IGlobalConfiguration>(MODULE);
        ioc->registerExport<IGlobalConfiguration>(MODULE, m_globalConfiguration);
        ioc->unregister<IApplication>(MODULE);
        ioc->registerExport<IApplication>(MODULE, m_application);
        ioc->unregister<ICommandsRegister>(MODULE);
        ioc->registerExport<ICommandsRegister>(MODULE, m_commandsRegister);

        m_repository = std::make_shared<MacrosRepository>();
        m_repository->init();
        ioc->unregister<IMacrosRepository>(MODULE);
        ioc->registerExport<IMacrosRepository>(MODULE, m_repository);

        m_runner = std::make_unique<MacroRunner>(modularity::ContextPtr());
    }

    void TearDown() override
    {
        m_runner.reset();

        auto ioc = modularity::globalIoc();
        ioc->unregister<IMacrosRepository>(MODULE);
        ioc->unregister<ICommandsRegister>(MODULE);
        ioc->unregister<IApplication>(MODULE);
        ioc->unregister<IGlobalConfiguration>(MODULE);
    }

    static MacroItem step(const Command& command)
    {
        return MacroStep { command, {} };
    }

    static MacroItem reference(const std::string& name)
    {
        return MacroReference { name };
    }

    QTemporaryDir m_directory;
    CommandInfo m_knownInfo;
    CommandInfo m_unknownInfo;
    std::shared_ptr<GlobalConfigurationMock> m_globalConfiguration;
    std::shared_ptr<ApplicationMock> m_application;
    std::shared_ptr<CommandsRegisterMock> m_commandsRegister;
    std::shared_ptr<MacrosRepository> m_repository;
    std::unique_ptr<MacroRunner> m_runner;
};

TEST_F(MacroManager_MacroRunnerTests, ValidateSucceedsWhenAllStepsExist)
{
    ASSERT_TRUE(m_repository->save(Macro { "Child", { step(KNOWN_COMMAND) } }));

    const Ret ret = m_runner->validate(Macro { "Main", { step(KNOWN_COMMAND), reference("Child") } });

    EXPECT_TRUE(ret) << ret.toString();
}

TEST_F(MacroManager_MacroRunnerTests, ValidateNamesMissingCommandAndItsStepNumber)
{
    const Ret ret = m_runner->validate(Macro { "Main", { step(KNOWN_COMMAND), step(UNKNOWN_COMMAND) } });

    ASSERT_FALSE(ret);
    EXPECT_THAT(ret.text(), HasSubstr("Step 2 of “Main”: command://test/unknown"));
    EXPECT_THAT(ret.text(), ::testing::Not(HasSubstr("command://test/known")));
}

TEST_F(MacroManager_MacroRunnerTests, ValidateNamesMissingMacroReference)
{
    const Ret ret = m_runner->validate(Macro { "Main", { step(KNOWN_COMMAND), reference("Gone") } });

    ASSERT_FALSE(ret);
    EXPECT_THAT(ret.text(), HasSubstr("Step 2 of “Main”: macro “Gone”"));
}

TEST_F(MacroManager_MacroRunnerTests, ValidateNamesMissingStepsInReferencedMacros)
{
    ASSERT_TRUE(m_repository->save(Macro { "Child", { step(KNOWN_COMMAND), step(UNKNOWN_COMMAND) } }));

    const Ret ret = m_runner->validate(Macro { "Main", { reference("Child") } });

    ASSERT_FALSE(ret);
    EXPECT_THAT(ret.text(), HasSubstr("Step 2 of “Child”: command://test/unknown"));
}

TEST_F(MacroManager_MacroRunnerTests, ValidateListsEveryMissingStep)
{
    const Ret ret = m_runner->validate(Macro { "Main", { step(UNKNOWN_COMMAND), reference("Gone") } });

    ASSERT_FALSE(ret);
    EXPECT_THAT(ret.text(), HasSubstr("Step 1 of “Main”: command://test/unknown"));
    EXPECT_THAT(ret.text(), HasSubstr("Step 2 of “Main”: macro “Gone”"));
}

TEST_F(MacroManager_MacroRunnerTests, ValidateListsStepOfMacroReferencedTwiceOnce)
{
    ASSERT_TRUE(m_repository->save(Macro { "Child", { step(UNKNOWN_COMMAND) } }));

    const Ret ret = m_runner->validate(Macro { "Main", { reference("Child"), reference("Child") } });

    ASSERT_FALSE(ret);
    EXPECT_EQ(occurrences(ret.text(), "Step 1 of “Child”"), 1u);
}

TEST_F(MacroManager_MacroRunnerTests, ValidateReportsCycleBeforeMissingSteps)
{
    const Ret ret = m_runner->validate(Macro { "Main", { step(UNKNOWN_COMMAND), reference("Main") } });

    ASSERT_FALSE(ret);
    EXPECT_EQ(Ret::Code(ret.code()), Ret::Code::BadArgs);
    EXPECT_THAT(ret.text(), ::testing::Not(HasSubstr("command://test/unknown")));
}
