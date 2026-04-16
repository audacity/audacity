/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/au3effectloader.h"
#include "au3-audio-unit/AudioUnitEffectsModule.h"

namespace au::effects {
class AudioUnitEffectLoader final : public Au3EffectLoader
{
public:
    AudioUnitEffectLoader()
        : Au3EffectLoader(m_module, muse::audio::AudioResourceType::AudioUnit) {}

private:
    ::AudioUnitEffectsModule m_module;
};
} // namespace au::effects
