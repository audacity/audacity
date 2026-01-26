/*
* Audacity: A Digital Audio Editor
*/
#include "nyquisteffectsrepository.h"

#include "au3-module-manager/PluginManager.h"
#include "au3wrap/internal/wxtypes_convert.h"

au::effects::EffectMetaList au::effects::NyquistEffectsRepository::effectMetaList() const
{
    au::effects::EffectMetaList effects;
    for (const PluginDescriptor& desc : PluginManager::Get().PluginsOfType(PluginTypeEffect)) {
        const std::string family = au3::wxToStdString(desc.GetEffectFamily());

        if (family != "Nyquist") {
            continue;
        }

        au::effects::EffectMeta meta;
        meta.id = au3::wxToString(desc.GetID());
        meta.family = au::effects::EffectFamily::Nyquist;
        meta.title = muse::String::fromStdString(desc.GetSymbol().Msgid().Translation().ToStdString());
        meta.isRealtimeCapable = desc.IsEffectRealtime();
        meta.vendor = muse::String::fromStdString(au3::wxToStdString(desc.GetVendor()));
        meta.path = desc.GetPath();

        // TODO: This is a simple "1st type wins" rule for menu placement.
        // AU3 uses semi-manual placement via resources/EffectsMenuDefaults.xml for organizing
        // effects into groups and categories. This will be improved later to support proper
        // menu organization and grouping.
        //
        // Type mapping for Nyquist effects (based on $type directive in .ny files):
        // - $type process  → Effects menu (Processor)
        // - $type generate → Generate menu (Generator)
        // - $type analyze  → Analyze menu (Analyzer)
        // - $type tool     → Tools menu (Tool)
        switch (desc.GetEffectType()) {
        case ::EffectTypeGenerate:
            meta.type = au::effects::EffectType::Generator;
            break;
        case ::EffectTypeProcess:
            meta.type = au::effects::EffectType::Processor;
            break;
        case ::EffectTypeAnalyze:
            meta.type = au::effects::EffectType::Analyzer;
            break;
        case ::EffectTypeTool:
            meta.type = au::effects::EffectType::Tool;
            break;
        default:
            meta.type = au::effects::EffectType::Unknown;
        }

        effects.push_back(std::move(meta));
    }

    return effects;
}
