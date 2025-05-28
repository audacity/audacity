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

namespace au::effects {
Lv2EffectsRepository::Lv2EffectsRepository()
    : m_helper{m_module, muse::audio::AudioResourceType::Lv2Plugin}
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
