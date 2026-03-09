/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include "framework/global/modularity/imoduleinterface.h"

#include <optional>

namespace au::spectrogram {
class ISpectralEffectsRegister : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ISpectralEffectsRegister)

public:
    virtual ~ISpectralEffectsRegister() = default;

    virtual void registerSpectralEffect(const SpectralEffect&) = 0;
    virtual SpectralEffectList spectralEffects() const = 0;
    virtual std::optional<SpectralEffect> spectralEffect(SpectralEffectId id) const = 0;
};
}
