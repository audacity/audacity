/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "ispectraleffectsregister.h"

namespace au::spectrogram {
class SpectralEffectsRegister : public ISpectralEffectsRegister
{
public:
    void registerSpectralEffect(const SpectralEffect&) override;
    SpectralEffectList spectralEffects()const override;
    std::optional<SpectralEffect> spectralEffect(SpectralEffectId id) const override;

private:
    SpectralEffectList m_spectralEffects;
};
}
