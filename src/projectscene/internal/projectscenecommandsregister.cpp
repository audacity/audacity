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
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Minutes && seconds"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Minutes && seconds"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_BEATS_MEASURES_RULER_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Beats && measures"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Beats && measures"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Show vertical rulers"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show vertical rulers"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_RMS_IN_WAVEFORM_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Show RMS in waveform"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show RMS in waveform"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_CLIPPING_IN_WAVEFORM_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Show clipping in waveform"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show clipping in waveform"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Update display while playing"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Update display while playing"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_PINNED_PLAY_HEAD_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Pinned playhead"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Pinned playhead"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_PLAYBACK_ON_RULER_CLICK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Click ruler to start playback"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Click ruler to start playback"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Half-wave"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Half-wave"),
        InputSchema({
                { PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_TRACK_ID_PARAM, Arg(DataType::Int, u"Track identifier") },
            }),
        Decoration(IconCode::Code::WAVEFORM_HALFWAVE, rcommand::Checkable::Yes),
        Availability::Internal | Availability::API
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_CLIP_GAIN_AUTOMATION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Clip gain"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Clip gain"),
        InputSchema(),
        Decoration(IconCode::Code::AUTOMATION),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_CLIP_PITCH_AND_SPEED_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Pitch and speed"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Pitch and speed"),
        InputSchema({
                { PROJECTSCENE_CLIP_PITCH_AND_SPEED_TRACK_ID_PARAM, Arg(DataType::Int, u"Track identifier") },
                { PROJECTSCENE_CLIP_PITCH_AND_SPEED_CLIP_ID_PARAM, Arg(DataType::Int, u"Clip identifier") },
            }),
        Decoration(),
        Availability::Internal
    },
    CommandInfo{
        PROJECTSCENE_OPEN_LABEL_EDITOR_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Show label editor"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show label editor"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        PROJECTSCENE_ZOOM_IN_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Zoom in"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Zoom in"),
        InputSchema(),
        Decoration(IconCode::Code::ZOOM_IN),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_ZOOM_OUT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Zoom out"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Zoom out"),
        InputSchema(),
        Decoration(IconCode::Code::ZOOM_OUT),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_ZOOM_DEFAULT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Zoom default"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Zoom default"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_ZOOM_TO_SELECTION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Zoom to selection"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Zoom to selection"),
        InputSchema(),
        Decoration(IconCode::Code::FIT_SELECTION),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_ZOOM_TO_FIT_PROJECT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Zoom to fit project"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Zoom to fit project"),
        InputSchema(),
        Decoration(IconCode::Code::FIT_PROJECT),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_ZOOM_TOGGLE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Zoom toggle"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Zoom toggle"),
        InputSchema(),
        Decoration(IconCode::Code::ZOOM_TOGGLE),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_INCREASE_TRACK_HEIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Increase track height"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Make the focused or selected tracks taller"),
        InputSchema({
                { PROJECTSCENE_TRACK_HEIGHT_TRACK_ID_PARAM, Arg(DataType::Int64, "Track identifier", Presence::Optional) },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_DECREASE_TRACK_HEIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Decrease track height"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Make the focused or selected tracks shorter"),
        InputSchema({
                { PROJECTSCENE_TRACK_HEIGHT_TRACK_ID_PARAM, Arg(DataType::Int64, "Track identifier", Presence::Optional) },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_INCREASE_ALL_TRACK_HEIGHTS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Increase all track heights"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Make all tracks taller"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_DECREASE_ALL_TRACK_HEIGHTS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Decrease all track heights"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Make all tracks shorter"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_COLLAPSE_TRACKS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Collapse tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Collapse the focused or selected tracks"),
        InputSchema({
                { PROJECTSCENE_TRACK_HEIGHT_TRACK_ID_PARAM, Arg(DataType::Int64, "Track identifier", Presence::Optional) },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_EXPAND_TRACKS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Expand tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Expand the focused or selected tracks back to their height before collapsing"),
        InputSchema({
                { PROJECTSCENE_TRACK_HEIGHT_TRACK_ID_PARAM, Arg(DataType::Int64, "Track identifier", Presence::Optional) },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_COLLAPSE_ALL_TRACKS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Collapse all tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Collapse all tracks"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_EXPAND_ALL_TRACKS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Expand collapsed tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Expand all collapsed tracks back to their height before collapsing"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_FIT_TRACKS_TO_HEIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Fit tracks to height"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Fit the focused or selected tracks to the height of the view and collapse the others"),
        InputSchema({
                { PROJECTSCENE_TRACK_HEIGHT_TRACK_ID_PARAM, Arg(DataType::Int64, "Track identifier", Presence::Optional) },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_FIT_ALL_TRACKS_TO_HEIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Fit all tracks to height"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Share the height of the view between all tracks"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_RESET_TRACK_HEIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Reset track height"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Reset the focused or selected tracks to their default height"),
        InputSchema({
                { PROJECTSCENE_TRACK_HEIGHT_TRACK_ID_PARAM, Arg(DataType::Int64, "Track identifier", Presence::Optional) },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_RESET_ALL_TRACK_HEIGHTS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Reset all track heights"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Reset all tracks to their default height"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECTSCENE_TIMELINE_PAGE_LEFT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Scroll timeline left one page"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Scroll the timeline left by one screen width"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_TIMELINE_PAGE_RIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Scroll timeline right one page"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Scroll the timeline right by one screen width"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_CENTER_VIEW_ON_PLAYHEAD_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Center view on playhead"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Center view on playhead"),
        InputSchema({
                { PROJECTSCENE_CENTER_VIEW_ON_PLAYHEAD_ONLY_IF_NOT_VISIBLE_PARAM,
                  Arg(DataType::Bool, u"Skip when the playhead is already visible") },
            }),
        Decoration(),
        Availability::Internal | Availability::API
    },
    CommandInfo{
        PROJECTSCENE_TIMELINE_CONTEXT_MENU_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Timeline context menu"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open the timeline context menu"),
        InputSchema(),
        Decoration(),
        Availability::Internal
    },
    CommandInfo{
        PROJECTSCENE_PLAY_POSITION_DECREASE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move playhead left"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move playhead left"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_PLAY_POSITION_INCREASE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move playhead right"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move playhead right"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_SELECTION_EXTEND_LEFT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Extend selection left"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Extend selection left"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_SELECTION_EXTEND_RIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Extend selection right"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Extend selection right"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_SELECTION_CONTRACT_LEFT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Contract selection from left"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Contract selection from left"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_SELECTION_CONTRACT_RIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Contract selection from right"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Contract selection from right"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_CURSOR_TO_SELECTION_START_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move playhead to selection start"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move playhead to selection start"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_CURSOR_TO_SELECTION_END_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move playhead to selection end"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move playhead to selection end"),
        InputSchema(),
        Decoration(),
        Availability::All
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_EFFECTS_PANEL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Show effects panel"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show effects panel"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::UI
    },
    CommandInfo{
        PROJECTSCENE_AUDIO_SETUP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Audio setup"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open audio setup context menu"),
        InputSchema(),
        Decoration(IconCode::Code::CONFIGURE),
        Availability::UI
    },
    CommandInfo{
        PROJECTSCENE_TOGGLE_SPLIT_TOOL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Split tool"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Split tool"),
        InputSchema(),
        Decoration(IconCode::Code::SPLIT_TOOL),
        Availability::UI
    },
    CommandInfo{
        PROJECTSCENE_REALTIME_EFFECT_MOVE_UP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move realtime effect up"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move realtime effect up"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        PROJECTSCENE_REALTIME_EFFECT_MOVE_DOWN_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move realtime effect down"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move realtime effect down"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        PROJECTSCENE_SNAP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Snapping"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Snapping"),
        InputSchema(),
        Decoration(IconCode::Code::MAGNET),
        Availability::Internal
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
