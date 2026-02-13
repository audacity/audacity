/*
* Audacity: A Digital Audio Editor
*/
#include "nyquisteffectsrepository.h"

#include "au3-module-manager/PluginManager.h"
#include "au3wrap/internal/wxtypes_convert.h"

au::effects::NyquistEffectsRepository::NyquistEffectsRepository(const muse::modularity::ContextPtr& ctx)
    : muse::Injectable(ctx), m_helper(ctx, m_module, muse::audio::AudioResourceType::NyquistPlugin)
{
}

au::effects::EffectMetaList au::effects::NyquistEffectsRepository::effectMetaList() const
{
    au::effects::EffectMetaList effects;

    // Get all plugins from the AU4 registry
    const std::vector<muse::audioplugins::AudioPluginInfo> allEffects = m_helper.knownPlugins()->pluginInfoList();

    for (const muse::audioplugins::AudioPluginInfo& info : allEffects) {
        // Filter for Nyquist plugins only
        if (!(info.type == muse::audioplugins::AudioPluginType::Fx && info.meta.type == muse::audio::AudioResourceType::NyquistPlugin)) {
            continue;
        }

        // Ensure the plugin is loaded in the AU3 PluginManager
        // This will call DiscoverPluginsAtPath() if needed
        if (!m_helper.ensurePluginIsLoaded(muse::String(info.meta.id.c_str()))) {
            continue;
        }

        // Get the AU3 descriptor to access the effect type and proper name
        const auto ptr = PluginManager::Get().GetPlugin(au3::wxFromString(muse::String::fromStdString(info.meta.id)));
        if (!ptr) {
            continue;
        }

        const PluginDescriptor& desc = *ptr;

        au::effects::EffectMeta meta;
        meta.id = muse::String(info.meta.id.c_str());
        meta.family = au::effects::EffectFamily::Nyquist;
        meta.title = muse::String::fromStdString(desc.GetSymbol().Msgid().Translation().ToStdString());
        meta.isRealtimeCapable = desc.IsEffectRealtime();
        meta.vendor = muse::String::fromStdString(info.meta.vendor);
        meta.path = info.path;

        // Map the AU3 effect type to AU4 effect type for proper menu placement
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

bool au::effects::NyquistEffectsRepository::ensurePluginIsLoaded(const EffectId& effectId) const
{
    return m_helper.ensurePluginIsLoaded(effectId);
}
