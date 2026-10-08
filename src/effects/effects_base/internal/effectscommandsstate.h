/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/icommandsregister.h"
#include "framework/rcommand/imodulecommandsstate.h"

#include "context/iglobalcontext.h"
#include "spectrogram/ifrequencyselectioncontroller.h"
#include "spectrogram/ispectraleffectsregister.h"
#include "../ieffectexecutionscenario.h"

namespace au::effects {
class EffectsCommandsState : public muse::rcommand::IModuleCommandsState, public muse::Contextable, public muse::async::Asyncable
{
    muse::GlobalInject<muse::rcommand::ICommandsRegister> commandsRegister;
    muse::GlobalInject<spectrogram::ISpectralEffectsRegister> spectralEffectsRegister;
    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<IEffectExecutionScenario> effectExecutionScenario{ this };
    muse::ContextInject<spectrogram::IFrequencySelectionController> frequencySelectionController{ this };

public:
    EffectsCommandsState(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    std::string moduleName() const override;

    void init() override;
    void deinit() override;

    muse::rcommand::CommandState commandState(const muse::rcommand::Command& command) const override;
    muse::async::Channel<muse::rcommand::Command, muse::rcommand::CommandState> commandStateChanged() const override;

private:
    void updateCommandStates(const std::vector<muse::rcommand::Command>& commands = {});
    muse::rcommand::CommandState effectCommandState(const EffectId& effectId) const;
    bool isSpectralEffect(const EffectId& effectId) const;

    muse::rcommand::IModuleCommandsRegisterPtr m_moduleRegister;
    std::map<muse::rcommand::Command, muse::rcommand::CommandState> m_commandStates;
    muse::async::Channel<muse::rcommand::Command, muse::rcommand::CommandState> m_commandStateChanged;
};
}
