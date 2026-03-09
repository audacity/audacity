/*
 * Audacity: A Digital Audio Editor
 */
#include "spectraleffectsregister.h"

namespace au::spectrogram {
void SpectralEffectsRegister::registerSpectralEffect(const SpectralEffect& effect)
{
    m_spectralEffects.push_back(effect);
}

SpectralEffectList SpectralEffectsRegister::spectralEffects() const
{
    return m_spectralEffects;
}

std::optional<SpectralEffect> SpectralEffectsRegister::spectralEffect(SpectralEffectId id) const
{
    for (const auto& effect : m_spectralEffects) {
        if (effect.spectralEffectId == id) {
            return effect;
        }
    }
    return std::nullopt;
}
}
