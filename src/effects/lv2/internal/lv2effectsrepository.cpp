/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2effectsrepository.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "log.h"

#include "au3-lv2/LoadLV2.h"
#include "au3-components/PluginProvider.h"
#include "au3-strings/TranslatableString.h"
#include "au3-module-manager/PluginManager.h"
#include "au3-module-manager/ModuleManager.h"
#include "au3-effects/EffectManager.h"

#include <lilv/lilv.h>

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
