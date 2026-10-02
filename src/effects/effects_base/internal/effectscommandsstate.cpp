/*
* Audacity: A Digital Audio Editor
*/
#include "effectscommandsstate.h"

#include "framework/global/log.h"

#include "../effectscommands.h"

using namespace au::effects;
using namespace muse;
using namespace muse::rcommand;

std::string EffectsCommandsState::moduleName() const
{
    return "effects";
}

void EffectsCommandsState::init()
{
    m_moduleRegister = commandsRegister()->moduleRegister(moduleName());
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    m_moduleRegister->commandListChanged().onNotify(this, [this]() {
        updateCommandStates();
    });

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        updateCommandStates();
    });

    effectExecutionScenario()->lastProcessorIsNowAvailable().onNotify(this, [this]() {
        updateCommandStates({ EFFECTS_REPEAT_LAST_EFFECT_COMMAND });
    });

    frequencySelectionController()->frequencySelectionChanged().onReceive(this, [this](bool complete) {
        if (complete) {
            updateCommandStates();
        }
    });

    updateCommandStates();
}

void EffectsCommandsState::deinit()
{
    if (m_moduleRegister) {
        m_moduleRegister->commandListChanged().disconnect(this);
    }
    globalContext()->currentProjectChanged().disconnect(this);
    effectExecutionScenario()->lastProcessorIsNowAvailable().disconnect(this);
    frequencySelectionController()->frequencySelectionChanged().disconnect(this);
}

void EffectsCommandsState::updateCommandStates(const std::vector<Command>& commands)
{
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    const auto& commandList = commands.empty() ? m_moduleRegister->commandList() : commands;

    for (const Command& command : commandList) {
        const CommandState newState = commandState(command);
        if (m_commandStates[command] != newState) {
            m_commandStates[command] = newState;
            m_commandStateChanged.send(command, newState);
        }
    }
}

bool EffectsCommandsState::isSpectralEffect(const EffectId& effectId) const
{
    const spectrogram::SpectralEffectList spectralEffects = spectralEffectsRegister()->spectralEffects();
    return std::any_of(spectralEffects.begin(), spectralEffects.end(), [&effectId](const spectrogram::SpectralEffect& spectralEffect) {
        return effectIdFromAction(muse::actions::ActionQuery(spectralEffect.action)) == effectId;
    });
}

CommandState EffectsCommandsState::commandState(const Command& command) const
{
    if (command == EFFECTS_REPEAT_LAST_EFFECT_COMMAND) {
        return CommandState(effectExecutionScenario()->lastProcessorIsAvailable(), false);
    }

    const EffectId effectId = effectIdFromCommand(command);
    if (!effectId.empty()) {
        return effectCommandState(effectId);
    }

    return CommandState(true, false);
}

CommandState EffectsCommandsState::effectCommandState(const EffectId& effectId) const
{
    if (globalContext()->currentProject() == nullptr) {
        return CommandState(false, false);
    }

    if (isSpectralEffect(effectId)) {
        const spectrogram::FrequencySelection selection = frequencySelectionController()->frequencySelection();
        return CommandState(frequencySelectionController()->showsSpectrogram(selection.trackId) && selection.isValid(), false);
    }

    return CommandState(true, false);
}

async::Channel<Command, CommandState> EffectsCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
