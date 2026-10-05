/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/rcommand/commandtypes.h"

namespace au::appshell {
inline static const muse::rcommand::Command APP_TOGGLE_FULLSCREEN_COMMAND("command://app/toggle-fullscreen");
inline static const muse::rcommand::Command APP_ABOUT_COMMAND("command://app/about");
inline static const muse::rcommand::Command APP_ABOUT_QT_COMMAND("command://app/about-qt");
inline static const muse::rcommand::Command APP_ONLINE_HANDBOOK_COMMAND("command://app/online-handbook");
inline static const muse::rcommand::Command APP_ASK_HELP_COMMAND("command://app/ask-help");
inline static const muse::rcommand::Command APP_PREFERENCES_COMMAND("command://app/preferences");
inline static const muse::rcommand::Command APP_REVERT_FACTORY_COMMAND("command://app/revert-factory");
inline static const muse::rcommand::Command APP_AUDIO_SETTINGS_COMMAND("command://app/audio-settings");
inline static const muse::rcommand::Command APP_SHORTCUTS_PREFERENCES_COMMAND("command://app/shortcuts-preferences");
inline static const muse::rcommand::Command APP_EDITING_PREFERENCES_COMMAND("command://app/editing-preferences");
inline static const muse::rcommand::Command APP_SPECTROGRAM_PREFERENCES_COMMAND("command://app/spectrogram-preferences");

inline static const muse::rcommand::Command GLOBAL_QUIT_COMMAND("command://app/quit");
inline static const muse::rcommand::Command GLOBAL_RESTART_COMMAND("command://restart");
inline static const muse::rcommand::Command GLOBAL_COPY_COMMAND("command://copy");
inline static const muse::rcommand::Command GLOBAL_CUT_COMMAND("command://cut");
inline static const muse::rcommand::Command GLOBAL_PASTE_COMMAND("command://paste");
inline static const muse::rcommand::Command GLOBAL_UNDO_COMMAND("command://undo");
inline static const muse::rcommand::Command GLOBAL_REDO_COMMAND("command://redo");
inline static const muse::rcommand::Command GLOBAL_DELETE_COMMAND("command://delete");
inline static const muse::rcommand::Command GLOBAL_CANCEL_COMMAND("command://cancel");
inline static const muse::rcommand::Command GLOBAL_TRIGGER_COMMAND("command://trigger");
inline static const muse::rcommand::Command GLOBAL_ENTER_COMMAND("command://enter");
inline static const muse::rcommand::Command GLOBAL_SHIFT_ENTER_COMMAND("command://shift-enter");
inline static const muse::rcommand::Command GLOBAL_CONTEXT_MENU_COMMAND("command://context-menu");
}
