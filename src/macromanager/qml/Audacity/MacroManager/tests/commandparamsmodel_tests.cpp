/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "global/modularity/ioc.h"

#include "../commandparamsmodel.h"

#include "macromanager/tests/mocks/commandsregistermock.h"

using namespace muse;
using namespace muse::rcommand;
using namespace au::macromanager;

using ::testing::_;
using ::testing::NiceMock;
using ::testing::ReturnRef;

namespace {
const Command ENUM_COMMAND("command://test/enum");
const std::string ENUM_KEY("relativeTo");
}

class MacroManager_CommandParamsModelTests : public ::testing::Test
{
protected:
    static constexpr const char* MODULE = "macromanager_qml_tests";

    void SetUp() override
    {
        Arg arg(DataType::Enum, std::string("Reference point"));
        arg.allowedValues = ValList { Val("ProjectStart"), Val("Project"), Val("SelectionEnd") };

        m_info.command = ENUM_COMMAND;
        m_info.inputSchema = InputSchema({ { ENUM_KEY, arg } });

        m_commandsRegister = std::make_shared<NiceMock<CommandsRegisterMock> >();
        ON_CALL(*m_commandsRegister, commandInfo(_)).WillByDefault(ReturnRef(m_info));

        auto ioc = modularity::globalIoc();
        ioc->unregister<ICommandsRegister>(MODULE);
        ioc->registerExport<ICommandsRegister>(MODULE, m_commandsRegister);
    }

    void TearDown() override
    {
        modularity::globalIoc()->unregister<ICommandsRegister>(MODULE);
    }

    QVariant roleValue(const QByteArray& roleName) const
    {
        const int role = m_model.roleNames().key(roleName);
        return m_model.data(m_model.index(0), role);
    }

    void load(const QVariantMap& params)
    {
        m_model.load(QString::fromStdString(ENUM_COMMAND.toString()), params);
    }

    CommandInfo m_info;
    std::shared_ptr<CommandsRegisterMock> m_commandsRegister;
    CommandParamsModel m_model;
};

TEST_F(MacroManager_CommandParamsModelTests, EnumParamOffersTheAllowedValues)
{
    load({});

    ASSERT_EQ(m_model.rowCount(), 1);
    EXPECT_EQ(roleValue("type").toInt(), static_cast<int>(CommandParamsModel::ParamType::Enum));

    const QVariantList options = roleValue("options").toList();
    ASSERT_EQ(options.size(), 3);
    EXPECT_EQ(options.at(0).toMap().value("value").toString(), "ProjectStart");
    EXPECT_EQ(options.at(0).toMap().value("text").toString(), "ProjectStart");
    EXPECT_EQ(options.at(2).toMap().value("value").toString(), "SelectionEnd");
}

TEST_F(MacroManager_CommandParamsModelTests, EnumParamDefaultsToTheFirstAllowedValue)
{
    load({});

    EXPECT_EQ(roleValue("value").toString(), "ProjectStart");
    EXPECT_EQ(m_model.params().value(QString::fromStdString(ENUM_KEY)).toString(), "ProjectStart");
}

TEST_F(MacroManager_CommandParamsModelTests, EnumParamKeepsAStoredAllowedValue)
{
    load({ { QString::fromStdString(ENUM_KEY), "SelectionEnd" } });

    EXPECT_EQ(roleValue("value").toString(), "SelectionEnd");
}

TEST_F(MacroManager_CommandParamsModelTests, EnumParamReplacesAStoredValueThatIsNotAllowed)
{
    load({ { QString::fromStdString(ENUM_KEY), "Unknown" } });

    EXPECT_EQ(roleValue("value").toString(), "ProjectStart");
}

TEST_F(MacroManager_CommandParamsModelTests, EnumParamSavesTheSelectedValue)
{
    load({});

    m_model.setValue(0, "Project");

    EXPECT_EQ(m_model.params().value(QString::fromStdString(ENUM_KEY)).toString(), "Project");
}
