/*
* Audacity: A Digital Audio Editor
*/
#include "trackeditcommandsregister.h"

#include "framework/ui/view/iconcodes.h"
#include "framework/global/types/translatablestring.h"

#include "../trackeditcommands.h"

using namespace au::trackedit;
using namespace muse;
using namespace muse::rcommand;
using namespace muse::ui;

namespace {
const std::vector<CommandInfo> commandInfos = {
    CommandInfo{
        TRACKEDIT_UNDO_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Undo"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Undo"),
        InputSchema(),
        Decoration(IconCode::Code::UNDO)
    },
    CommandInfo{
        TRACKEDIT_REDO_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Redo"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Redo"),
        InputSchema(),
        Decoration(IconCode::Code::REDO)
    },
    CommandInfo{
        TRACKEDIT_CANCEL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Cancel"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Cancel the current interaction or clear the selection"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_PASTE_INSERT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Paste (pushes clips on selected track)"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Paste (pushes clips on selected track)"),
        InputSchema(),
        Decoration(IconCode::Code::PASTE)
    },
    CommandInfo{
        TRACKEDIT_PASTE_OVERLAP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Paste (overlaps other clips)"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Paste (overlaps other clips)"),
        InputSchema(),
        Decoration(IconCode::Code::PASTE)
    },
    CommandInfo{
        TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Paste (preserves synchronization on all tracks)"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Paste (preserves synchronization on all tracks)"),
        InputSchema(),
        Decoration(IconCode::Code::PASTE)
    },
    CommandInfo{
        TRACKEDIT_CLIP_CUT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Cut clip"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Cut the given clip into the clipboard"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
                { TRACKEDIT_CLIP_ID_PARAM, Arg(DataType::Integer, u"Clip id") },
            }),
        Decoration(IconCode::Code::CUT)
    },
    CommandInfo{
        TRACKEDIT_CLIP_COPY_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Copy clip"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Copy the given clip into the clipboard"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
                { TRACKEDIT_CLIP_ID_PARAM, Arg(DataType::Integer, u"Clip id") },
            }),
        Decoration(IconCode::Code::COPY)
    },
    CommandInfo{
        TRACKEDIT_CLIP_DELETE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Delete clip"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Delete the given clip"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
                { TRACKEDIT_CLIP_ID_PARAM, Arg(DataType::Integer, u"Clip id") },
            }),
        Decoration(IconCode::Code::DELETE_TANK)
    },
    CommandInfo{
        TRACKEDIT_CLIP_SPLIT_CUT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Split cut clip"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Cut the given clip into the clipboard and leave a gap"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
                { TRACKEDIT_CLIP_ID_PARAM, Arg(DataType::Integer, u"Clip id") },
            }),
        Decoration(IconCode::Code::CUT)
    },
    CommandInfo{
        TRACKEDIT_CLIP_SPLIT_DELETE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Split delete clip"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Delete the given clip and leave a gap"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
                { TRACKEDIT_CLIP_ID_PARAM, Arg(DataType::Integer, u"Clip id") },
            }),
        Decoration(IconCode::Code::DELETE_TANK)
    },
    CommandInfo{
        TRACKEDIT_CLIP_PITCH_SPEED_OPEN_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Open pitch and speed dialog"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open pitch and speed dialog"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_CLIP_RENDER_PITCH_SPEED_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Render pitch and speed"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Render pitch and speed"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
                { TRACKEDIT_CLIP_ID_PARAM, Arg(DataType::Integer, u"Clip id") },
            }),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_CLIP_RESET_PITCH_SPEED_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Reset pitch and speed"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Reset pitch and speed"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
                { TRACKEDIT_CLIP_ID_PARAM, Arg(DataType::Integer, u"Clip id") },
            }),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_STRETCH_CLIP_TO_MATCH_TEMPO_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Stretch with tempo changes"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Stretch with tempo changes"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
                { TRACKEDIT_CLIP_ID_PARAM, Arg(DataType::Integer, u"Clip id") },
            }),
        Decoration(Checkable::Yes)
    },
    CommandInfo{
        TRACKEDIT_TRACK_SPLIT_AT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Split tracks at"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Split the given tracks at the given positions"),
        InputSchema({
                { TRACKEDIT_TRACK_IDS_PARAM, Arg(DataType::Array, u"Track ids") },
                { TRACKEDIT_PIVOTS_PARAM, Arg(DataType::Array, u"Split positions in seconds") },
            }),
        Decoration(IconCode::Code::SPLIT_TOOL)
    },
    CommandInfo{
        TRACKEDIT_NEW_MONO_TRACK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "New mono track"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "New mono track"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_NEW_STEREO_TRACK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "New stereo track"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "New stereo track"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_NEW_LABEL_TRACK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "New label track"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "New label track"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_DUPLICATE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Duplicate"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Duplicate track"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_MOVE_UP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move track up"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move track up"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_MOVE_DOWN_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move track down"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move track down"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_MOVE_TOP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move track to top"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move track to top"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_MOVE_BOTTOM_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move track to bottom"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move track to bottom"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_SWAP_CHANNELS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Swap stereo channels"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Swap stereo channels"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_SPLIT_STEREO_TO_LR_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Split stereo to L/R mono"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Split stereo to L/R mono"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_SPLIT_STEREO_TO_CENTER_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Split stereo to center mono"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Split stereo to center mono"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_CHANGE_RATE_CUSTOM_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Other…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Set a custom track sample rate"),
        InputSchema(),
        Decoration(Checkable::Yes)
    },
    CommandInfo{
        TRACKEDIT_TRACK_MAKE_STEREO_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Make stereo track"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Make stereo track"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_RESAMPLE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Resample track…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Resample track…"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRIM_AUDIO_OUTSIDE_SELECTION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Trim"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Trim audio outside selection"),
        InputSchema(),
        Decoration(IconCode::Code::TRIM_AUDIO_OUTSIDE_SELECTION)
    },
    CommandInfo{
        TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Silence"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Silence audio selection"),
        InputSchema(),
        Decoration(IconCode::Code::SILENCE_AUDIO_SELECTION)
    },
    CommandInfo{
        TRACKEDIT_GROUP_CLIPS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Group clips"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Group clips"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_UNGROUP_CLIPS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Ungroup clips"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Ungroup clips"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_SELECT_ALL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Select all"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Select all"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_CLEAR_SELECTION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Clear selection"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Clear selection"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_SELECT_ALL_TRACKS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Select all tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Select all tracks"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_SELECT_LEFT_OF_PLAYBACK_POSITION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Left of playback position"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Select from a chosen time to the playback position"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_SELECT_RIGHT_OF_PLAYBACK_POSITION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Right of playback position"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Select from the playback position to a chosen time"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_SELECT_TRACK_START_TO_CURSOR_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Track start to cursor"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Select from the track start to the cursor"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_SELECT_CURSOR_TO_TRACK_END_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Cursor to track end"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Select from the cursor to the track end"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_SELECT_TRACK_START_TO_END_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Track start to end"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Select from the track start to the track end"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_SET_SELECTION_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Set selection"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Set the time selection"),
        InputSchema({
                { TRACKEDIT_START_PARAM, Arg(DataType::Float, u"Selection start in seconds") },
                { TRACKEDIT_END_PARAM, Arg(DataType::Float, u"Selection end in seconds") },
            }),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_SELECT_TRACK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Select track"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Select the track at the given index"),
        InputSchema({
                { TRACKEDIT_TRACK_INDEX_PARAM, Arg(DataType::Integer, u"Track index") },
            }),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_ZERO_CROSS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "At zero crossings"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move the cursor to the nearest zero crossing"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_CLIP_CHANGE_COLOR_AUTO_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Same as track color"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Use the track color for the selected clips"),
        InputSchema(),
        Decoration(Checkable::Yes)
    },
    CommandInfo{
        TRACKEDIT_CLIP_CHANGE_COLOR_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Change clip color"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Change clip color"),
        InputSchema({
                { TRACKEDIT_COLOR_INDEX_PARAM, Arg(DataType::Integer, u"Color index") },
            }),
        Decoration(Checkable::Yes)
    },
    CommandInfo{
        TRACKEDIT_TRACK_CHANGE_COLOR_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Change track color"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Change track color"),
        InputSchema({
                { TRACKEDIT_COLOR_INDEX_PARAM, Arg(DataType::Integer, u"Color index") },
            }),
        Decoration(Checkable::Yes)
    },
    CommandInfo{
        TRACKEDIT_TRACK_CHANGE_FORMAT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Change track format"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Change track format"),
        InputSchema({
                { TRACKEDIT_FORMAT_PARAM, Arg(DataType::Integer, u"Track format") },
            }),
        Decoration(Checkable::Yes)
    },
    CommandInfo{
        TRACKEDIT_TRACK_CHANGE_RATE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Change track sample rate"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Change track sample rate"),
        InputSchema({
                { TRACKEDIT_RATE_PARAM, Arg(DataType::Integer, u"Sample rate in Hz") },
            }),
        Decoration(Checkable::Yes)
    },
    CommandInfo{
        TRACKEDIT_GLOBAL_VIEW_SPECTROGRAM_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Toggle spectral view"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Toggle spectral view"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_WAVEFORM_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Waveform"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Waveform"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
            }),
        Decoration(IconCode::Code::WAVEFORM, Checkable::Yes)
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_SPECTROGRAM_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Spectrogram"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Spectrogram"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
            }),
        Decoration(IconCode::Code::SPECTROGRAM, Checkable::Yes)
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_MULTI_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Multi-view"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Multi-view"),
        InputSchema({
                { TRACKEDIT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Track id") },
            }),
        Decoration(IconCode::Code::WAVEFORM_MULTIVIEW, Checkable::Yes)
    },
    CommandInfo{
        TRACKEDIT_LABEL_ADD_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Add label"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Add label"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_RENAME_ITEM_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Rename item (clip/label)"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Rename item (clip/label)"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_ITEM_MOVE_LEFT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move item left"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move item left"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_ITEM_MOVE_RIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move item right"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move item right"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_ITEM_MOVE_UP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move item up"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move item up"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_ITEM_MOVE_DOWN_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Move item down"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Move item down"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_ITEM_EXTEND_LEFT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Extend item left"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Extend item left"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_ITEM_EXTEND_RIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Extend item right"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Extend item right"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_ITEM_REDUCE_LEFT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Reduce item left"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Reduce item left"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        TRACKEDIT_TRACK_VIEW_ITEM_REDUCE_RIGHT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Reduce item right"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Reduce item right"),
        InputSchema(),
        Decoration()
    },
};
}

std::string TrackeditCommandsRegister::moduleName() const
{
    return "trackedit";
}

const std::vector<Command>& TrackeditCommandsRegister::commandList() const
{
    static std::vector<Command> commands;
    if (commands.empty()) {
        commands.reserve(commandInfos.size());
        for (const auto& info : commandInfos) {
            commands.push_back(info.command);
        }
    }
    return commands;
}

const std::vector<CommandInfo>& TrackeditCommandsRegister::commandInfoList() const
{
    return commandInfos;
}
