/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "global/modularity/imoduleinterface.h"

#include "ieffectloader.h"

namespace au::effects {
class IEffectLoadersRegister : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectLoadersRegister)

public:
    virtual ~IEffectLoadersRegister() = default;

    virtual void registerLoader(IEffectLoaderPtr loader) = 0;
    virtual IEffectLoaderPtr loader(EffectFamily family) const = 0;
};
} // namespace au::effects
