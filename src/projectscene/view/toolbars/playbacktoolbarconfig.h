/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/ui/toolconfig.h"

#include "playback/playbackcommands.h"
#include "record/recordcommands.h"
#include "trackedit/trackeditcommands.h"
#include "projectscene/projectscenecommands.h"

namespace au::projectscene {
inline const muse::ui::ToolConfig& defaultPlaybackToolBarConfig()
{
    static const muse::ui::ToolConfig config = []() {
        muse::ui::ToolConfig c;
        c.items = {
            { playback::PLAYBACK_TOGGLE_PLAY_PAUSE_COMMAND, true },
            { playback::PLAYBACK_STOP_COMMAND, true },
            { record::RECORD_START_COMMAND, true },
            { playback::PLAYBACK_REWIND_START_COMMAND, true },
            { playback::PLAYBACK_REWIND_END_COMMAND, true },
            { playback::PLAYBACK_TOGGLE_LOOP_REGION_COMMAND, true },
            { "", true },
            { PROJECTSCENE_TOGGLE_CLIP_GAIN_AUTOMATION_COMMAND, true },
            { PROJECTSCENE_TOGGLE_SPLIT_TOOL_COMMAND, true },
            { trackedit::TRACKEDIT_GLOBAL_VIEW_SPECTROGRAM_COMMAND, true },
            { PROJECTSCENE_ZOOM_IN_COMMAND, true },
            { PROJECTSCENE_ZOOM_OUT_COMMAND, true },
            { PROJECTSCENE_ZOOM_TO_SELECTION_COMMAND, true },
            { PROJECTSCENE_ZOOM_TO_FIT_PROJECT_COMMAND, true },
            { PROJECTSCENE_ZOOM_TOGGLE_COMMAND, true },
            // { "split-cut", false },
            { trackedit::TRACKEDIT_CUT_COMMAND, false },
            { trackedit::TRACKEDIT_COPY_COMMAND, false },
            { trackedit::TRACKEDIT_PASTE_DEFAULT_COMMAND, false },
            { "", true },
            { trackedit::TRACKEDIT_TRIM_AUDIO_OUTSIDE_SELECTION_COMMAND, true },
            { trackedit::TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND, true },
            { "", true },
            { playback::PLAYBACK_TIME_COMMAND, true },
            { playback::PLAYBACK_BPM_COMMAND, false },
            { playback::PLAYBACK_TIME_SIGNATURE_COMMAND, false },
            { "", true },
            { PROJECTSCENE_SNAP_COMMAND, true },
            { "", true },
            { record::RECORD_LEVEL_COMMAND, true },
            { playback::PLAYBACK_LEVEL_COMMAND, true }
        };
        return c;
    }();

    return config;
}
}
