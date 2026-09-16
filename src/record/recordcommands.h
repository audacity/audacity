/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/rcommand/commandtypes.h"

namespace au::record {
inline static const muse::rcommand::Command RECORD_START_COMMAND("command://record/start");
inline static const muse::rcommand::Command RECORD_ON_CURRENT_TRACK_COMMAND("command://record/on-current-track");
inline static const muse::rcommand::Command RECORD_ON_NEW_TRACK_COMMAND("command://record/on-new-track");
inline static const muse::rcommand::Command RECORD_PAUSE_COMMAND("command://record/pause");
inline static const muse::rcommand::Command RECORD_STOP_COMMAND("command://record/stop");
inline static const muse::rcommand::Command RECORD_LEVEL_COMMAND("command://record/level");
inline static const muse::rcommand::Command RECORD_TOGGLE_MIC_METERING_COMMAND("command://record/toggle-mic-metering");
inline static const muse::rcommand::Command RECORD_TOGGLE_INPUT_MONITORING_COMMAND("command://record/toggle-input-monitoring");
inline static const muse::rcommand::Command RECORD_LEAD_IN_RECORDING_COMMAND("command://record/lead-in-recording");
}
