/*
* Audacity: A Digital Audio Editor
*/
#include "projectcommandsstate.h"

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "../projectcommands.h"

using namespace au::project;
using namespace muse;
using namespace muse::rcommand;

namespace {
const std::vector<Command> commandsAllowedWithoutProject {
    PROJECT_NEW_COMMAND,
    PROJECT_OPEN_COMMAND,
    PROJECT_OPEN_CLOUD_COMMAND,
    PROJECT_CLEAR_RECENT_COMMAND,
    PROJECT_IMPORT_STARTUP_MEDIA_COMMAND,
    PROJECT_OPEN_CLOUD_AUDIO_FILE_COMMAND,
    PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_FOR_PROJECT_COMMAND,
};

const std::vector<Command> commandsDisabledDuringRecording {
    PROJECT_CLOSE_COMMAND,
    PROJECT_IMPORT_COMMAND,
    PROJECT_SAVE_COMMAND,
    PROJECT_SAVE_TO_CLOUD_COMMAND,
    PROJECT_SAVE_AS_COMMAND,
    PROJECT_EXPORT_AUDIO_COMMAND,
    PROJECT_EXPORT_LABELS_COMMAND,
    PROJECT_EXPORT_MIDI_COMMAND,
    PROJECT_SHARE_AUDIO_COMMAND,
    PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_COMMAND,
    PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_FOR_PROJECT_COMMAND,
};

const std::vector<Command> cloudProjectCommands {
    PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_COMMAND,
};

const std::vector<Command> audioContentCommands {
    PROJECT_SHARE_AUDIO_COMMAND,
    PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_COMMAND,
    PROJECT_EXPORT_AUDIO_COMMAND,
};

const std::vector<Command> labelsCommands {
    PROJECT_EXPORT_LABELS_COMMAND,
};
}

std::string ProjectCommandsState::moduleName() const
{
    return "project";
}

void ProjectCommandsState::init()
{
    m_moduleRegister = commandsRegister()->moduleRegister(moduleName());
    IF_ASSERT_FAILED(m_moduleRegister) {
        return;
    }

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        listenProjectChanges();
        updateCommandStates();
    });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        updateCommandStates(commandsDisabledDuringRecording);
    });

    listenProjectChanges();
    updateCommandStates();
}

void ProjectCommandsState::deinit()
{
    globalContext()->currentTrackeditProjectChanged().disconnect(this);
    recordController()->isRecordingChanged().disconnect(this);

    if (const trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject()) {
        prj->hasAudioContent().ch.disconnect(this);
        prj->hasLabels().ch.disconnect(this);
    }

    if (const IAudacityProjectPtr prj = globalContext()->currentProject()) {
        prj->isCloudProjectChanged().disconnect(this);
    }
}

void ProjectCommandsState::listenProjectChanges()
{
    if (const trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject()) {
        prj->hasAudioContent().ch.onReceive(this, [this](bool) {
            updateCommandStates(audioContentCommands);
        }, muse::async::Asyncable::Mode::SetReplace);

        prj->hasLabels().ch.onReceive(this, [this](bool) {
            updateCommandStates(labelsCommands);
        }, muse::async::Asyncable::Mode::SetReplace);
    }

    if (const IAudacityProjectPtr prj = globalContext()->currentProject()) {
        prj->isCloudProjectChanged().onNotify(this, [this]() {
            updateCommandStates(cloudProjectCommands);
        }, muse::async::Asyncable::Mode::SetReplace);
    }
}

void ProjectCommandsState::updateCommandStates(const std::vector<Command>& commands)
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

CommandState ProjectCommandsState::commandState(const Command& command) const
{
    const IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return CommandState(muse::contains(commandsAllowedWithoutProject, command));
    }

    if (muse::contains(cloudProjectCommands, command) && !project->isCloudProject()) {
        return CommandState(false);
    }

    const trackedit::ITrackeditProjectPtr trackeditProject = globalContext()->currentTrackeditProject();
    if (muse::contains(audioContentCommands, command) && (!trackeditProject || !trackeditProject->hasAudioContent().val)) {
        return CommandState(false);
    }

    if (muse::contains(labelsCommands, command) && (!trackeditProject || !trackeditProject->hasLabels().val)) {
        return CommandState(false);
    }

    if (muse::contains(commandsDisabledDuringRecording, command) && recordController()->isRecording()) {
        return CommandState(false);
    }

    return CommandState(true);
}

async::Channel<Command, CommandState> ProjectCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
