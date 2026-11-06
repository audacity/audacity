/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "framework/rcommand/icommandsregister.h"

namespace au::macromanager {
class CommandsRegisterMock : public muse::rcommand::ICommandsRegister
{
public:
    MOCK_METHOD(void, reg, (const muse::rcommand::IModuleCommandsRegisterPtr& module), (override));
    MOCK_METHOD(void, unreg, (const muse::rcommand::IModuleCommandsRegisterPtr& module), (override));
    MOCK_METHOD(muse::rcommand::IModuleCommandsRegisterPtr, moduleRegister, (const std::string& moduleName), (const, override));
    MOCK_METHOD(std::vector<muse::rcommand::CommandInfo>, commandInfoList, (), (const, override));
    MOCK_METHOD(const muse::rcommand::CommandInfo&, commandInfo, (const muse::rcommand::Command& command), (const, override));
    MOCK_METHOD(muse::async::Notification, commandInfoListChanged, (), (const, override));
    MOCK_METHOD(const std::string&, commandModuleName, (const muse::rcommand::Command& command), (const, override));
};
}
