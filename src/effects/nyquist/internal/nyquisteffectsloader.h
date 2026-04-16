/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/au3effectloader.h"
#include "au3-nyquist-effects/LoadNyquist.h"

namespace au::effects {
class NyquistEffectsLoader final : public Au3EffectLoader
{
public:
    NyquistEffectsLoader()
        : Au3EffectLoader(m_module, muse::audio::AudioResourceType::NyquistPlugin) {}

private:
    ::NyquistEffectsModule m_module;
};
} // namespace au::effects
