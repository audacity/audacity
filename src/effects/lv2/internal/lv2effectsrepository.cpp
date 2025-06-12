/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2effectsrepository.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "log.h"

#include "libraries/lib-lv2/LoadLV2.h"
#include "libraries/lib-components/PluginProvider.h"
#include "libraries/lib-strings/TranslatableString.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-module-manager/ModuleManager.h"
#include "libraries/lib-effects/EffectManager.h"

#include <lilv/lilv.h>

namespace au::effects {
namespace {
muse::String effectTitle(const muse::io::path_t& path)
{
    const LilvPlugin* plugin = ::LV2EffectsModule::GetPlugin(path.c_str());
    if (!plugin) {
        // Plugin not found, maybe it was removed.
        return {};
    }
    return muse::String { lilv_node_as_string(lilv_plugin_get_name(plugin)) };
}
}

Lv2EffectsRepository::Lv2EffectsRepository()
    : m_helper{m_module, muse::audio::AudioResourceType::Lv2Plugin, effectTitle}
{
}

EffectMetaList Lv2EffectsRepository::effectMetaList() const
{
    return m_helper.effectMetaList();
}

bool Lv2EffectsRepository::ensurePluginIsLoaded(const EffectId& effectId) const
{
    return m_helper.ensurePluginIsLoaded(effectId);
}
}
