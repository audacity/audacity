/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/imoduleinterface.h"

#include "effectstypes.h"

namespace au::effects {
class IEffectsProviderInitializer : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsProviderInitializer)

public:
    virtual ~IEffectsProviderInitializer() = default;

    //! NOTE Set before callAfterSplashScreen(); defaults to AskUser.
    virtual void setStartupPluginValidationPolicy(StartupPluginValidationPolicy policy) = 0;
    virtual void callAfterSplashScreen() = 0;
};
}
