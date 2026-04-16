/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/imoduleinterface.h"

namespace au::effects {
class IEffectsProviderInitializer : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsProviderInitializer)

public:
    virtual ~IEffectsProviderInitializer() = default;

    virtual void callAfterSplashScreen() = 0;
};
}
