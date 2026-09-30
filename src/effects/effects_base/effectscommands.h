/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/stringutils.h"
#include "framework/rcommand/commandtypes.h"

#include "effectstypes.h"

namespace au::effects {
inline static const muse::rcommand::Command EFFECTS_REPEAT_LAST_EFFECT_COMMAND("command://effects/repeat-last-effect");
inline static const muse::rcommand::Command EFFECTS_PLUGIN_MANAGER_COMMAND("command://effects/plugin-manager");
inline static const muse::rcommand::Command EFFECTS_TOGGLE_VENDOR_UI_COMMAND("command://effects/toggle-vendor-ui");

inline static const muse::rcommand::Command EFFECTS_PRESET_APPLY_COMMAND("command://effects/presets/apply");
inline static const muse::rcommand::Command EFFECTS_PRESET_SAVE_COMMAND("command://effects/presets/save");
inline static const muse::rcommand::Command EFFECTS_PRESET_SAVE_AS_COMMAND("command://effects/presets/save-as");
inline static const muse::rcommand::Command EFFECTS_PRESET_DELETE_COMMAND("command://effects/presets/delete");
inline static const muse::rcommand::Command EFFECTS_PRESET_IMPORT_COMMAND("command://effects/presets/import");
inline static const muse::rcommand::Command EFFECTS_PRESET_EXPORT_COMMAND("command://effects/presets/export");

constexpr std::string_view EFFECTS_SCHEME = "effects";
constexpr std::string_view EFFECT_OPEN_COMMAND = "open";
constexpr std::string_view EFFECT_APPLY_COMMAND = "apply";

inline muse::rcommand::Command makeEffectCommand(std::string_view commandName, const EffectId& effectId)
{
    // effects + commandName + effectId -> command://effects/commandName/effectId
    muse::rcommand::Command command;
    command.setScheme(std::string(muse::rcommand::COMMAND_SCHEME));
    command.addPath(std::string(EFFECTS_SCHEME));
    command.addPath(std::string(commandName));
    command.addPath(effectId.toStdString());
    return command;
}

inline EffectId effectIdFromCommand(const muse::rcommand::Command& command)
{
    const std::string& path = command.path();
    for (const std::string_view commandName : { EFFECT_OPEN_COMMAND, EFFECT_APPLY_COMMAND }) {
        const std::string prefix = std::string(EFFECTS_SCHEME) + "/" + std::string(commandName) + "/";
        if (muse::strings::startsWith(path, prefix)) {
            return EffectId::fromStdString(path.substr(prefix.size()));
        }
    }

    return EffectId();
}

inline muse::rcommand::Command makeEffectOpenCommand(const EffectId& effectId)
{
    return makeEffectCommand(EFFECT_OPEN_COMMAND, effectId);
}

inline muse::rcommand::Command makeEffectApplyCommand(const EffectId& effectId)
{
    return makeEffectCommand(EFFECT_APPLY_COMMAND, effectId);
}
}
