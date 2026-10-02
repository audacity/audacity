/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/imodulecommandsregister.h"

#include "../ieffectsprovider.h"

namespace au::effects {
class EffectsCommandsRegister : public muse::rcommand::IModuleCommandsRegister, public muse::async::Asyncable
{
    muse::GlobalInject<IEffectsProvider> effectsProvider;

public:
    EffectsCommandsRegister() = default;

    void init();

    std::string moduleName() const override;

    const std::vector<muse::rcommand::Command>& commandList() const override;
    const std::vector<muse::rcommand::CommandInfo>& commandInfoList() const override;
    muse::async::Notification commandListChanged() const override;

private:
    void reload();

    std::vector<muse::rcommand::CommandInfo> m_commandInfos;
    mutable std::vector<muse::rcommand::Command> m_commands;
    muse::async::Notification m_commandListChanged;
};
}
