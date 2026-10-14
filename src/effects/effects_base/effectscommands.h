/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <iomanip>
#include <sstream>

#include "framework/global/stringutils.h"
#include "framework/rcommand/commandtypes.h"
#include "framework/rcommand/typedcommand.h"
#include "framework/ui/view/iconcodes.h"

#include "effectstypes.h"

namespace au::effects {
//! Typed commands: the id, the texts and the parameters of a command are declared once, here

struct RepeatLastEffectCommand {
    static inline const muse::rcommand::Command id { "command://effects/repeat-last-effect" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "Repeat last effect");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "Repeat last effect");

    static constexpr auto fields() { return std::tuple {}; }
};

struct PluginManagerCommand {
    static inline const muse::rcommand::Command id { "command://effects/plugin-manager" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "Plugin manager");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "Plugin manager");

    static constexpr auto fields() { return std::tuple {}; }
};

struct ToggleVendorUiCommand {
    static inline const muse::rcommand::Command id { "command://effects/toggle-vendor-ui" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("effects", "Use vendor UI");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("effects",
                                                                                        "Toggle between vendor UI and fallback UI");
    static inline const muse::rcommand::Decoration decoration { muse::rcommand::Checkable::Yes };

    EffectId effectId;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "effectId", &ToggleVendorUiCommand::effectId, u"Effect identifier" },
        };
    }
};

struct ApplyPresetCommand {
    static inline const muse::rcommand::Command id { "command://effects/presets/apply" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "&Apply preset");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "Apply preset");

    EffectInstanceId instanceId = 0;
    std::string presetId;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "instanceId", &ApplyPresetCommand::instanceId, u"Effect instance identifier" },
            Field { "presetId", &ApplyPresetCommand::presetId, u"Preset identifier" },
        };
    }
};
struct SavePresetCommand {
    static inline const muse::rcommand::Command id { "command://effects/presets/save" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "&Save preset");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "Save preset");

    EffectInstanceId instanceId = 0;
    std::string presetId;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "instanceId", &SavePresetCommand::instanceId, u"Effect instance identifier" },
            Field { "presetId", &SavePresetCommand::presetId, u"Preset identifier" },
        };
    }
};

struct SavePresetAsCommand {
    static inline const muse::rcommand::Command id { "command://effects/presets/save-as" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "Save preset as…");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "Save preset as");

    EffectInstanceId instanceId = 0;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "instanceId", &SavePresetAsCommand::instanceId, u"Effect instance identifier" },
        };
    }
};

struct DeletePresetCommand {
    static inline const muse::rcommand::Command id { "command://effects/presets/delete" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "&Delete preset");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "Delete preset");
    static inline const muse::rcommand::Decoration decoration { muse::ui::IconCode::Code::DELETE_TANK };

    EffectId effectId;
    std::string presetId;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "effectId", &DeletePresetCommand::effectId, u"Effect identifier" },
            Field { "presetId", &DeletePresetCommand::presetId, u"Preset identifier" },
        };
    }
};

struct ImportPresetCommand {
    static inline const muse::rcommand::Command id { "command://effects/presets/import" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "&Import…");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "Import preset");

    EffectInstanceId instanceId = 0;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "instanceId", &ImportPresetCommand::instanceId, u"Effect instance identifier" },
        };
    }
};

struct ExportPresetCommand {
    static inline const muse::rcommand::Command id { "command://effects/presets/export" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "&Export…");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "Export preset");

    EffectInstanceId instanceId = 0;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "instanceId", &ExportPresetCommand::instanceId, u"Effect instance identifier" },
        };
    }
};

constexpr std::string_view EFFECTS_SCHEME = "effects";
constexpr std::string_view EFFECT_OPEN_COMMAND = "open";
constexpr std::string_view EFFECT_APPLY_COMMAND = "apply";

// RFC 3986 percent-encoding: an effect id is a plugin path that may contain Uri delimiters such as ? and /
inline std::string encodeEffectId(const EffectId& effectId)
{
    static const std::string unreserved
        ="abcdefghijklmnopqrstuvwxyz"
         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
         "0123456789"
         "-_.~";

    std::ostringstream encoded;
    encoded.fill('0');
    encoded << std::hex << std::uppercase;
    for (const char c : effectId.toStdString()) {
        if (unreserved.find(c) != std::string::npos) {
            encoded << c;
        } else {
            encoded << '%' << std::setw(2) << static_cast<int>(static_cast<unsigned char>(c));
        }
    }
    return encoded.str();
}

inline EffectId decodeEffectId(const std::string& encoded)
{
    std::string decoded;
    for (size_t i = 0; i < encoded.size(); ++i) {
        if (encoded[i] == '%' && i + 2 < encoded.size()) {
            decoded += static_cast<char>(std::stoi(encoded.substr(i + 1, 2), nullptr, 16));
            i += 2;
        } else {
            decoded += encoded[i];
        }
    }
    return EffectId::fromStdString(decoded);
}

inline muse::rcommand::Command makeEffectCommand(std::string_view commandName, const EffectId& effectId)
{
    // effects + commandName + effectId -> command://effects/commandName/effectId
    muse::rcommand::Command command;
    command.setScheme(std::string(muse::rcommand::COMMAND_SCHEME));
    command.addPath(std::string(EFFECTS_SCHEME));
    command.addPath(std::string(commandName));
    command.addPath(encodeEffectId(effectId));
    return command;
}

inline EffectId effectIdFromCommand(const muse::rcommand::Command& command)
{
    const std::string& path = command.path();
    for (const std::string_view commandName : { EFFECT_OPEN_COMMAND, EFFECT_APPLY_COMMAND }) {
        const std::string prefix = std::string(EFFECTS_SCHEME) + "/" + std::string(commandName) + "/";
        if (muse::strings::startsWith(path, prefix)) {
            return decodeEffectId(path.substr(prefix.size()));
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
