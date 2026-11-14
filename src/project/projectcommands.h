/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/rcommand/commandtypes.h"

namespace au::project {
inline static const muse::rcommand::Command PROJECT_NEW_COMMAND("command://project/new");
inline static const muse::rcommand::Command PROJECT_OPEN_COMMAND("command://project/open");
inline static const muse::rcommand::Command PROJECT_OPEN_CLOUD_COMMAND("command://project/open-cloud");
inline static const muse::rcommand::Command PROJECT_CLEAR_RECENT_COMMAND("command://project/clear-recent");
inline static const muse::rcommand::Command PROJECT_IMPORT_COMMAND("command://project/import");
inline static const muse::rcommand::Command PROJECT_IMPORT_STARTUP_MEDIA_COMMAND("command://project/import-startup-media");

inline static const muse::rcommand::Command PROJECT_SAVE_COMMAND("command://project/save");
inline static const muse::rcommand::Command PROJECT_SAVE_AS_COMMAND("command://project/save-as");
inline static const muse::rcommand::Command PROJECT_SAVE_TO_CLOUD_COMMAND("command://project/save-to-cloud");
inline static const muse::rcommand::Command PROJECT_CLOSE_COMMAND("command://project/close");

inline static const muse::rcommand::Command PROJECT_SHARE_AUDIO_COMMAND("command://project/share-audio");
inline static const muse::rcommand::Command PROJECT_OPEN_CLOUD_AUDIO_FILE_COMMAND("command://project/open-cloud-audio-file");
inline static const muse::rcommand::Command PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_COMMAND("command://project/update-cloud-audio-preview");
inline static const muse::rcommand::Command PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_FOR_PROJECT_COMMAND(
    "command://project/update-cloud-audio-preview-for-project");

inline static const muse::rcommand::Command PROJECT_EXPORT_AUDIO_COMMAND("command://project/export-audio");
inline static const muse::rcommand::Command PROJECT_EXPORT_LABELS_COMMAND("command://project/export-labels");
inline static const muse::rcommand::Command PROJECT_EXPORT_MIDI_COMMAND("command://project/export-midi");

inline static const muse::rcommand::Command PROJECT_OPEN_METADATA_DIALOG_COMMAND("command://project/open-metadata-dialog");
inline static const muse::rcommand::Command PROJECT_OPEN_CUSTOM_FFMPEG_OPTIONS_COMMAND("command://project/open-custom-ffmpeg-options");
inline static const muse::rcommand::Command PROJECT_OPEN_CUSTOM_MAPPING_COMMAND("command://project/open-custom-mapping");

inline static const std::string PROJECT_URL_PARAM("url");
inline static const std::string PROJECT_DISPLAY_NAME_PARAM("displayName");
inline static const std::string PROJECT_ID_PARAM("projectId");
inline static const std::string PROJECT_SNAPSHOT_ID_PARAM("snapshotId");
inline static const std::string PROJECT_FILES_PARAM("files");
inline static const std::string PROJECT_REMOVE_AFTER_IMPORT_PARAM("removeAfterImport");
inline static const std::string PROJECT_AUDIO_ID_PARAM("audioId");
inline static const std::string PROJECT_TRACK_ID_PARAM("trackId");
}
