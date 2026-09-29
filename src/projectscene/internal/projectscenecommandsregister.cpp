/*
* Audacity: A Digital Audio Editor
*/
#include "projectscenecommandsregister.h"

#include "framework/ui/view/iconcodes.h"
#include "framework/global/types/translatablestring.h"

#include "../projectscenecommands.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::rcommand;
using namespace muse::ui;

namespace {
const std::vector<CommandInfo> s_commandInfos = {
    CommandInfo{
        PROJECTSCENE_MINUTES_SECONDS_RULER_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Minutes && seconds"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Minutes && seconds"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PROJECTSCENE_BEATS_MEASURES_RULER_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Beats && measures"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Beats && measures"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Show vertical rulers"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Show vertical rulers"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_RMS_IN_WAVEFORM_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Show RMS in waveform"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Show RMS in waveform"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_CLIPPING_IN_WAVEFORM_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Show clipping in waveform"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Show clipping in waveform"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Update display while playing"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Update display while playing"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_PINNED_PLAY_HEAD_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Pinned playhead"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Pinned playhead"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_PLAYBACK_ON_RULER_CLICK_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Click ruler to start playback"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Click ruler to start playback"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Half-wave"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Half-wave"),
        InputSchema({
                { "trackId", Arg(DataType::Integer, u"Track identifier") },
            }),
        Decoration(IconCode::Code::WAVEFORM_HALFWAVE, rcommand::Checkable::Yes)
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_CLIP_GAIN_AUTOMATION_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Clip gain"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Clip gain"),
        InputSchema(),
        Decoration(IconCode::Code::AUTOMATION)
    },
    CommandInfo{
        PROJECTSCENE_CLIP_PITCH_AND_SPEED_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Pitch and speed"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Pitch and speed"),
        InputSchema({
                { "trackId", Arg(DataType::Integer, u"Track identifier") },
                { "clipId", Arg(DataType::Integer, u"Clip identifier") },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_OPEN_LABEL_EDITOR_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Show label editor"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Show label editor"),
        InputSchema(),
        Decoration()
    },
};
}

std::string ProjectSceneCommandsRegister::moduleName() const
{
    return "projectscene";
}

const std::vector<Command>& ProjectSceneCommandsRegister::commandList() const
{
    static std::vector<Command> commands;
    if (commands.empty()) {
        commands.reserve(s_commandInfos.size());
        for (const auto& info : s_commandInfos) {
            commands.push_back(info.command);
        }
    }
    return commands;
}

const std::vector<CommandInfo>& ProjectSceneCommandsRegister::commandInfoList() const
{
    return s_commandInfos;
}
