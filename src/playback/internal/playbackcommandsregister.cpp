/*
* Audacity: A Digital Audio Editor
*/
#include "playbackcommandsregister.h"

#include "framework/ui/view/iconcodes.h"
#include "framework/global/types/translatablestring.h"

#include "../playbackcommands.h"

using namespace au::playback;
using namespace muse;
using namespace muse::rcommand;
using namespace muse::ui;

namespace {
const std::vector<CommandInfo> s_commandInfos = {
    CommandInfo{
        PLAYBACK_TOGGLE_PLAY_PAUSE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Play/Pause"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Play/Pause"),
        InputSchema(),
        Decoration(IconCode::Code::PLAY_FILL)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_PLAY_STOP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Play/Stop"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Play/Stop"),
        InputSchema(),
        Decoration(IconCode::Code::PLAY_FILL)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_PLAY_STOP_AND_SET_CURSOR_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Play/Stop and set cursor"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Play/Stop and set cursor"),
        InputSchema(),
        Decoration(IconCode::Code::PLAY_FILL)
    },
    CommandInfo{
        PLAYBACK_PLAY_SELECTION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Play selection"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Play selection"),
        InputSchema(),
        Decoration(IconCode::Code::PLAY_FILL)
    },
    CommandInfo{
        PLAYBACK_PLAY_TRACKS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Play tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Play the given tracks"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_PAUSE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Pause"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Pause"),
        InputSchema(),
        Decoration(IconCode::Code::PAUSE_FILL)
    },
    CommandInfo{
        PLAYBACK_STOP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Stop"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Stop playback"),
        InputSchema(),
        Decoration(IconCode::Code::STOP_FILL)
    },
    CommandInfo{
        PLAYBACK_REWIND_START_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Rewind to start"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Rewind to start"),
        InputSchema(),
        Decoration(IconCode::Code::REWIND_START_FILL)
    },
    CommandInfo{
        PLAYBACK_REWIND_END_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Rewind to end"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Rewind to end"),
        InputSchema(),
        Decoration(IconCode::Code::REWIND_END_FILL)
    },
    CommandInfo{
        PLAYBACK_SEEK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Seek"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move the playhead to the given time"),
        InputSchema({
                { PLAYBACK_SEEK_TIME_PARAM, Arg(DataType::Float, u"Time in seconds") },
                { PLAYBACK_SEEK_TRIGGER_PLAY_PARAM, Arg(DataType::Boolean, u"Start playback from the new position") },
            }),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_CHANGE_PLAY_REGION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Change play region"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Change the playback region"),
        InputSchema({
                { PLAYBACK_CHANGE_PLAY_REGION_START_PARAM, Arg(DataType::Float, u"Region start in seconds") },
                { PLAYBACK_CHANGE_PLAY_REGION_END_PARAM, Arg(DataType::Float, u"Region end in seconds") },
            }),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_CHANGE_AUDIO_API_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Change audio host"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Change audio host"),
        InputSchema({
                { PLAYBACK_CHANGE_AUDIO_API_INDEX_PARAM, Arg(DataType::Integer, u"Index in the list of available audio hosts") },
            }),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_CHANGE_PLAYBACK_DEVICE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Change playback device"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Change playback device"),
        InputSchema({
                { PLAYBACK_CHANGE_PLAYBACK_DEVICE_INDEX_PARAM, Arg(DataType::Integer, u"Index in the list of available output devices") },
                { PLAYBACK_CHANGE_PLAYBACK_DEVICE_IS_DEFAULT_PARAM, Arg(DataType::Boolean, u"Use the system default device") },
            }),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_CHANGE_RECORDING_DEVICE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Change recording device"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Change recording device"),
        InputSchema({
                { PLAYBACK_CHANGE_RECORDING_DEVICE_INDEX_PARAM, Arg(DataType::Integer, u"Index in the list of available input devices") },
                { PLAYBACK_CHANGE_RECORDING_DEVICE_IS_DEFAULT_PARAM, Arg(DataType::Boolean, u"Use the system default device") },
            }),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_CHANGE_INPUT_CHANNELS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Change input channels"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Change input channels"),
        InputSchema({
                { "input-channels_index", Arg(DataType::Integer, u"Number of input channels") },
            }),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_RESCAN_DEVICES_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Rescan audio devices"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Rescan audio devices"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_TOGGLE_PLAY_REPEATS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Play repeats"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Play repeats"),
        InputSchema(),
        Decoration(IconCode::Code::PLAY_REPEATS, rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_AUTOMATIC_PAN_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Pan automatically"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Pan automatically during playback"),
        InputSchema(),
        Decoration(IconCode::Code::PAN_SCORE, rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_LOOP_REGION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Loop playback"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Toggle ‘Loop playback’"),
        InputSchema(),
        Decoration(IconCode::Code::LOOP, rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_CLEAR_LOOP_REGION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Clear loop region"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Clear loop region"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_SET_LOOP_REGION_TO_SELECTION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Set loop region to selection"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Set loop region to selection"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_SET_SELECTION_TO_LOOP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Set selection to loop"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Set selection to loop"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_SET_LOOP_REGION_IN_OUT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Set loop region in out"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Set loop region in out"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Creating a loop also selects audio"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Creating a loop also selects audio"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_MUTE_FOCUSED_TRACK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Mute/unmute focused track"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Mute/unmute focused track"),
        InputSchema(),
        Decoration(IconCode::Code::MUTE)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_SOLO_FOCUSED_TRACK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Solo/unsolo focused track"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Solo/unsolo focused track"),
        InputSchema(),
        Decoration(IconCode::Code::SOLO)
    },
    CommandInfo{
        PLAYBACK_MUTE_ALL_TRACKS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Mute all tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Mute all tracks"),
        InputSchema(),
        Decoration(IconCode::Code::MUTE)
    },
    CommandInfo{
        PLAYBACK_UNMUTE_ALL_TRACKS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Unmute all tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Unmute all tracks"),
        InputSchema(),
        Decoration(IconCode::Code::MUTE)
    },
    CommandInfo{
        PLAYBACK_MUTE_SELECTED_TRACKS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Mute selected tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Mute selected tracks"),
        InputSchema(),
        Decoration(IconCode::Code::MUTE)
    },
    CommandInfo{
        PLAYBACK_UNMUTE_SELECTED_TRACKS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Unmute selected tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Unmute selected tracks"),
        InputSchema(),
        Decoration(IconCode::Code::MUTE)
    },
    CommandInfo{
        PLAYBACK_LEVEL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Playback level"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Set playback level"),
        InputSchema(),
        Decoration(IconCode::Code::AUDIO)
    },
    CommandInfo{
        PLAYBACK_METRONOME_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Metronome"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Toggle metronome playback"),
        InputSchema(),
        Decoration(IconCode::Code::METRONOME, rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_TIME_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Timecode"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Set playback time"),
        InputSchema(),
        Decoration(IconCode::Code::CLOCK)
    },
    CommandInfo{
        PLAYBACK_BPM_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Tempo"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Set playback tempo"),
        InputSchema(),
        Decoration(IconCode::Code::BPM)
    },
    CommandInfo{
        PLAYBACK_TIME_SIGNATURE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Time signature"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Set playback time signature"),
        InputSchema(),
        Decoration(IconCode::Code::TIME_SIGNATURE)
    },
};
}

std::string PlaybackCommandsRegister::moduleName() const
{
    return "playback";
}

const std::vector<Command>& PlaybackCommandsRegister::commandList() const
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

const std::vector<CommandInfo>& PlaybackCommandsRegister::commandInfoList() const
{
    return s_commandInfos;
}
