/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "effectstypes.h"

namespace au::effects {
class IEffectLoader
{
public:
    virtual ~IEffectLoader() = default;

    virtual EffectFamily family() const = 0;
    virtual bool ensurePluginIsLoaded(const EffectId& effectId) const = 0;
};

using IEffectLoaderPtr = std::shared_ptr<IEffectLoader>;
} // namespace au::effects
