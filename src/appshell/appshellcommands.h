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

//! TODO: The framework's all_instances semantics aren't implemented yet.
//! SingleProcessProvider::quitForAll and AppUpdateScenario dispatch this command
//! with all_instances=false, expecting only the current window to close.
inline static const muse::rcommand::Command APP_QUIT_COMMAND("command://app/quit");
inline static const std::string INSTALLER_PATH_PARAM("installer_path");

inline static const muse::rcommand::Command APP_RESTART_COMMAND("command://app/restart");
inline static const muse::rcommand::Command APP_COPY_COMMAND("command://app/copy");
inline static const muse::rcommand::Command APP_CUT_COMMAND("command://app/cut");
inline static const muse::rcommand::Command APP_PASTE_COMMAND("command://app/paste");
inline static const muse::rcommand::Command APP_UNDO_COMMAND("command://app/undo");
inline static const muse::rcommand::Command APP_REDO_COMMAND("command://app/redo");
inline static const muse::rcommand::Command APP_DELETE_COMMAND("command://app/delete");
inline static const muse::rcommand::Command APP_CANCEL_COMMAND("command://app/cancel");
inline static const muse::rcommand::Command APP_TRIGGER_COMMAND("command://app/trigger");
inline static const muse::rcommand::Command APP_ENTER_COMMAND("command://app/enter");
inline static const muse::rcommand::Command APP_SHIFT_ENTER_COMMAND("command://app/shift-enter");
inline static const muse::rcommand::Command APP_CONTEXT_MENU_COMMAND("command://app/context-menu");
}
