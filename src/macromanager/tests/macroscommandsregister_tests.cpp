/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <set>

#include <QTemporaryDir>

#include "global/modularity/ioc.h"
#include "global/iapplication.h"
#include "global/iglobalconfiguration.h"
#include "global/tests/mocks/applicationmock.h"
#include "global/tests/mocks/globalconfigurationmock.h"

#include "macromanager/macromanagercommands.h"
#include "macromanager/internal/macroscommandsregister.h"
#include "macromanager/internal/macrosrepository.h"

using namespace muse;
using namespace muse::rcommand;
using namespace au::macromanager;

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::NiceMock;
using ::testing::Return;

class MacroManager_MacrosCommandsRegisterTests : public ::testing::Test
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

        auto ioc = modularity::globalIoc();
        ioc->unregister<IGlobalConfiguration>(MODULE);
        ioc->registerExport<IGlobalConfiguration>(MODULE, m_globalConfiguration);
        ioc->unregister<IApplication>(MODULE);
        ioc->registerExport<IApplication>(MODULE, m_application);

        m_repository = std::make_shared<MacrosRepository>();
        m_repository->init();
        ioc->unregister<IMacrosRepository>(MODULE);
        ioc->registerExport<IMacrosRepository>(MODULE, m_repository);

        m_register = std::make_shared<MacrosCommandsRegister>();
        m_register->init();
    }

    void TearDown() override
    {
        m_register.reset();

        auto ioc = modularity::globalIoc();
        ioc->unregister<IMacrosRepository>(MODULE);
        ioc->unregister<IApplication>(MODULE);
        ioc->unregister<IGlobalConfiguration>(MODULE);
    }

    QTemporaryDir m_directory;
    std::shared_ptr<GlobalConfigurationMock> m_globalConfiguration;
    std::shared_ptr<ApplicationMock> m_application;
    std::shared_ptr<MacrosRepository> m_repository;
    std::shared_ptr<MacrosCommandsRegister> m_register;
};

TEST_F(MacroManager_MacrosCommandsRegisterTests, CommandsFollowSaveRenameAndRemove)
{
    ASSERT_TRUE(m_repository->save(Macro { "Clean up", {} }));
    EXPECT_THAT(m_register->commandList(), ElementsAre(makeMacroRunCommand("Clean up")));

    ASSERT_TRUE(m_repository->rename("Clean up", "Master"));
    EXPECT_THAT(m_register->commandList(), ElementsAre(makeMacroRunCommand("Master")));

    ASSERT_TRUE(m_repository->remove("Master"));
    EXPECT_THAT(m_register->commandList(), IsEmpty());
}

TEST_F(MacroManager_MacrosCommandsRegisterTests, NamesWithUriDelimitersGiveDistinctSingleSegmentCommands)
{
    const std::vector<std::string> names { "Fade/Out", "Fade?Out", "Fade Out", "Fade_Out", "Fade%2FOut" };
    for (const std::string& name : names) {
        ASSERT_TRUE(m_repository->save(Macro { name, {} }));
    }

    std::set<std::string> commands;
    for (const Command& command : m_register->commandList()) {
        EXPECT_EQ(command.pathSegments().size(), 3) << command.toString();
        commands.insert(command.toString());
    }

    EXPECT_EQ(commands.size(), names.size());
}

TEST_F(MacroManager_MacrosCommandsRegisterTests, TitleShowsAmpersandOfMacroName)
{
    ASSERT_TRUE(m_repository->save(Macro { "Fade & Normalize", {} }));

    const std::vector<CommandInfo>& infos = m_register->commandInfoList();

    ASSERT_EQ(infos.size(), 1);
    EXPECT_EQ(infos.front().title.qTranslatedWithoutMnemonic(), "Fade & Normalize");
}
