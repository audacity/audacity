/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../ilv2effectsrepository.h"
#include "effects/effects_base/internal/effectsrepositoryhelper.h"
#include "modularity/ioc.h"
#include "libraries/lib-lv2/LoadLV2.h"

namespace au::effects {
class Lv2EffectsRepository final : public ILv2EffectsRepository
{
public:
    Lv2EffectsRepository();

    EffectMetaList effectMetaList() const override;
    bool ensurePluginIsLoaded(const EffectId& effectId) const override;

private:
    LV2EffectsModule m_module;
    EffectsRepositoryHelper m_helper;
};
}
