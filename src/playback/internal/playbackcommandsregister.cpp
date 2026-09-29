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
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Play/Pause"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Play/Pause"),
        InputSchema(),
        Decoration(IconCode::Code::PLAY_FILL)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_PLAY_STOP_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Play/Stop"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Play/Stop"),
        InputSchema(),
        Decoration(IconCode::Code::PLAY_FILL)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_PLAY_STOP_AND_SET_CURSOR_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Play/Stop and set cursor"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Play/Stop and set cursor"),
        InputSchema(),
        Decoration(IconCode::Code::PLAY_FILL)
    },
    CommandInfo{
        PLAYBACK_PLAY_SELECTION_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Play selection"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Play selection"),
        InputSchema(),
        Decoration(IconCode::Code::PLAY_FILL)
    },
    CommandInfo{
        PLAYBACK_PLAY_TRACKS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Play tracks"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Play the given tracks"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_PAUSE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Pause"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Pause"),
        InputSchema(),
        Decoration(IconCode::Code::PAUSE_FILL)
    },
    CommandInfo{
        PLAYBACK_STOP_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Stop"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Stop playback"),
        InputSchema(),
        Decoration(IconCode::Code::STOP_FILL)
    },
    CommandInfo{
        PLAYBACK_REWIND_START_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Rewind to start"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Rewind to start"),
        InputSchema(),
        Decoration(IconCode::Code::REWIND_START_FILL)
    },
    CommandInfo{
        PLAYBACK_REWIND_END_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Rewind to end"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Rewind to end"),
        InputSchema(),
        Decoration(IconCode::Code::REWIND_END_FILL)
    },
    CommandInfo{
        PLAYBACK_SEEK_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Seek"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Move the playhead to the given time"),
        InputSchema({
                { "seekTime", Arg(DataType::Float, u"Time in seconds") },
                { "triggerPlay", Arg(DataType::Boolean, u"Start playback from the new position") },
            }),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_CHANGE_PLAY_REGION_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Change play region"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Change the playback region"),
        InputSchema({
                { "start", Arg(DataType::Float, u"Region start in seconds") },
                { "end", Arg(DataType::Float, u"Region end in seconds") },
            }),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_CHANGE_AUDIO_API_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Change audio host"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Change audio host"),
        InputSchema({
                { "api_index", Arg(DataType::Integer, u"Index in the list of available audio hosts") },
            }),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_CHANGE_PLAYBACK_DEVICE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Change playback device"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Change playback device"),
        InputSchema({
                { "device_index", Arg(DataType::Integer, u"Index in the list of available output devices") },
                { "is_default_device", Arg(DataType::Boolean, u"Use the system default device") },
            }),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_CHANGE_RECORDING_DEVICE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Change recording device"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Change recording device"),
        InputSchema({
                { "device_index", Arg(DataType::Integer, u"Index in the list of available input devices") },
                { "is_default_device", Arg(DataType::Boolean, u"Use the system default device") },
            }),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_CHANGE_INPUT_CHANNELS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Change input channels"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Change input channels"),
        InputSchema({
                { "input-channels_index", Arg(DataType::Integer, u"Number of input channels") },
            }),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_RESCAN_DEVICES_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Rescan audio devices"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Rescan audio devices"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_TOGGLE_PLAY_REPEATS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Play repeats"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Play repeats"),
        InputSchema(),
        Decoration(IconCode::Code::PLAY_REPEATS, rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_AUTOMATIC_PAN_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Pan automatically"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Pan automatically during playback"),
        InputSchema(),
        Decoration(IconCode::Code::PAN_SCORE, rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_LOOP_REGION_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Loop playback"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Toggle ‘Loop playback’"),
        InputSchema(),
        Decoration(IconCode::Code::LOOP, rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_CLEAR_LOOP_REGION_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Clear loop region"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Clear loop region"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_SET_LOOP_REGION_TO_SELECTION_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Set loop region to selection"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Set loop region to selection"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_SET_SELECTION_TO_LOOP_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Set selection to loop"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Set selection to loop"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_SET_LOOP_REGION_IN_OUT_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Set loop region in out"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Set loop region in out"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PLAYBACK_TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Creating a loop also selects audio"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Creating a loop also selects audio"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_MUTE_FOCUSED_TRACK_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Mute/unmute focused track"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Mute/unmute focused track"),
        InputSchema(),
        Decoration(IconCode::Code::MUTE)
    },
    CommandInfo{
        PLAYBACK_TOGGLE_SOLO_FOCUSED_TRACK_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Solo/unsolo focused track"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Solo/unsolo focused track"),
        InputSchema(),
        Decoration(IconCode::Code::SOLO)
    },
    CommandInfo{
        PLAYBACK_MUTE_ALL_TRACKS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Mute all tracks"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Mute all tracks"),
        InputSchema(),
        Decoration(IconCode::Code::MUTE)
    },
    CommandInfo{
        PLAYBACK_UNMUTE_ALL_TRACKS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Unmute all tracks"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Unmute all tracks"),
        InputSchema(),
        Decoration(IconCode::Code::MUTE)
    },
    CommandInfo{
        PLAYBACK_MUTE_SELECTED_TRACKS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Mute selected tracks"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Mute selected tracks"),
        InputSchema(),
        Decoration(IconCode::Code::MUTE)
    },
    CommandInfo{
        PLAYBACK_UNMUTE_SELECTED_TRACKS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Unmute selected tracks"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Unmute selected tracks"),
        InputSchema(),
        Decoration(IconCode::Code::MUTE)
    },
    CommandInfo{
        PLAYBACK_LEVEL_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Playback level"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Set playback level"),
        InputSchema(),
        Decoration(IconCode::Code::AUDIO)
    },
    CommandInfo{
        PLAYBACK_METRONOME_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Metronome"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Toggle metronome playback"),
        InputSchema(),
        Decoration(IconCode::Code::METRONOME, rcommand::Checkable::Yes)
    },
    CommandInfo{
        PLAYBACK_TIME_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Timecode"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Set playback time"),
        InputSchema(),
        Decoration(IconCode::Code::CLOCK)
    },
    CommandInfo{
        PLAYBACK_BPM_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Tempo"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Set playback tempo"),
        InputSchema(),
        Decoration(IconCode::Code::BPM)
    },
    CommandInfo{
        PLAYBACK_TIME_SIGNATURE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Time signature"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Set playback time signature"),
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
