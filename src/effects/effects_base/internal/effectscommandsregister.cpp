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
    makeCommandInfo<RepeatLastEffectCommand>(),
    makeCommandInfo<PluginManagerCommand>(),
    makeCommandInfo<ToggleVendorUiCommand>(),
    makeCommandInfo<ApplyPresetCommand>(),
    makeCommandInfo<SavePresetCommand>(),
    makeCommandInfo<SavePresetAsCommand>(),
    makeCommandInfo<DeletePresetCommand>(),
    makeCommandInfo<ImportPresetCommand>(),
    makeCommandInfo<ExportPresetCommand>(),
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
