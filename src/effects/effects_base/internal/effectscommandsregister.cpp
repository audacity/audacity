/*
* Audacity: A Digital Audio Editor
*/
#include "effectscommandsregister.h"

#include "framework/ui/view/iconcodes.h"
#include "framework/global/types/translatablestring.h"

#include "effectsutils.h"
#include "../effectscommands.h"

using namespace au::effects;
using namespace muse;
using namespace muse::rcommand;
using namespace muse::ui;

namespace {
const std::vector<CommandInfo> s_commandInfos = {
    CommandInfo{
        EFFECTS_REPEAT_LAST_EFFECT_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Repeat last effect"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Repeat last effect"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        EFFECTS_PLUGIN_MANAGER_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Plugin manager"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Plugin manager"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        EFFECTS_TOGGLE_VENDOR_UI_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("effects", "Use vendor UI"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("effects", "Toggle between vendor UI and fallback UI"),
        InputSchema({
                { "effectId", Arg(DataType::String, u"Effect identifier") },
            }),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        EFFECTS_PRESET_APPLY_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Apply preset"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Apply preset"),
        inputSchema<ApplyPresetCommand>(),
        Decoration()
    },
    CommandInfo{
        EFFECTS_PRESET_SAVE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Save preset"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Save preset"),
        InputSchema({
                { "instanceId", Arg(DataType::Integer, u"Effect instance identifier") },
                { "presetId", Arg(DataType::String, u"Preset identifier") },
            }),
        Decoration()
    },
    CommandInfo{
        EFFECTS_PRESET_SAVE_AS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Save preset as…"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Save preset as"),
        InputSchema({
                { "instanceId", Arg(DataType::Integer, u"Effect instance identifier") },
            }),
        Decoration()
    },
    CommandInfo{
        EFFECTS_PRESET_DELETE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Delete preset"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Delete preset"),
        InputSchema({
                { "effectId", Arg(DataType::String, u"Effect identifier") },
                { "presetId", Arg(DataType::String, u"Preset identifier") },
            }),
        Decoration(IconCode::Code::DELETE_TANK)
    },
    CommandInfo{
        EFFECTS_PRESET_IMPORT_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Import…"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Import preset"),
        InputSchema({
                { "instanceId", Arg(DataType::Integer, u"Effect instance identifier") },
            }),
        Decoration()
    },
    CommandInfo{
        EFFECTS_PRESET_EXPORT_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Export…"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Export preset"),
        InputSchema({
                { "instanceId", Arg(DataType::Integer, u"Effect instance identifier") },
            }),
        Decoration()
    },
};
}

void EffectsCommandsRegister::init()
{
    effectsProvider()->effectMetaListChanged().onNotify(this, [this]() {
        reload();
    });

    reload();
}

std::string EffectsCommandsRegister::moduleName() const
{
    return "effects";
}

void EffectsCommandsRegister::reload()
{
    m_commandInfos.clear();
    m_commands.clear();

    EffectMetaList effects = effectsProvider()->effectMetaList();
    utils::replaceIdenticalTitlesWithPaths(effects);

    m_commandInfos.reserve(s_commandInfos.size() + 2 * effects.size());
    m_commandInfos.insert(m_commandInfos.end(), s_commandInfos.begin(), s_commandInfos.end());

    for (const EffectMeta& meta : effects) {
        const muse::String title = utils::effectDisplayTitle(meta);

        m_commandInfos.push_back(CommandInfo {
            makeEffectOpenCommand(meta.id),
            TranslatableString::untranslatable(title),
            TranslatableString::untranslatable(meta.description),
            InputSchema(),
            Decoration()
        });

        m_commandInfos.push_back(CommandInfo {
            makeEffectApplyCommand(meta.id),
            //: Action title: shown as a menu item or a button label; keep it short. %1 is the effect name
            TranslatableString("action", "Apply %1").arg(title),
            //: Action description: shown as a tooltip; can be a full sentence. %1 is the effect name
            TranslatableString("action_description", "Apply %1 with the given parameters").arg(title),
            InputSchema(),
            Decoration()
        });
    }

    m_commandListChanged.notify();
}

const std::vector<Command>& EffectsCommandsRegister::commandList() const
{
    if (m_commands.empty()) {
        m_commands.reserve(m_commandInfos.size());
        for (const auto& info : m_commandInfos) {
            m_commands.push_back(info.command);
        }
    }
    return m_commands;
}

const std::vector<CommandInfo>& EffectsCommandsRegister::commandInfoList() const
{
    return m_commandInfos;
}

async::Notification EffectsCommandsRegister::commandListChanged() const
{
    return m_commandListChanged;
}
