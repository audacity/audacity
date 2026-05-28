/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/au3effectloader.h"
#include "au3-effects/LoadEffects.h"

namespace au::effects {
class BuiltinEffectsLoader final : public Au3EffectLoader
{
public:
    BuiltinEffectsLoader()
        : Au3EffectLoader(m_module, muse::audio::AudioResourceType::NativeEffect) {}

private:
    ::BuiltinEffectsModule m_module;
};
} // namespace au::effects
