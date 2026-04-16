/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/au3effectloader.h"
#include "au3-vst3/VST3EffectsModule.h"

namespace au::effects {
class Vst3EffectLoader final : public Au3EffectLoader
{
public:
    Vst3EffectLoader()
        : Au3EffectLoader(m_module, muse::audio::AudioResourceType::VstPlugin) {}

private:
    VST3EffectsModule m_module;
};
} // namespace au::effects
