/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/au3effectloader.h"
#include "au3-lv2/LoadLV2.h"

namespace au::effects {
class Lv2EffectLoader final : public Au3EffectLoader
{
public:
    Lv2EffectLoader()
        : Au3EffectLoader(m_module, muse::audio::AudioResourceType::Lv2Plugin) {}

private:
    LV2EffectsModule m_module;
};
} // namespace au::effects
