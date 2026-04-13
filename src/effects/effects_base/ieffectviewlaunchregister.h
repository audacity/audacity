/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "global/modularity/imoduleinterface.h"

#include "ieffectviewlauncher.h"
#include "effectstypes.h"

namespace au::effects {
class IEffectViewLaunchRegister : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectViewLaunchRegister)

public:
    virtual ~IEffectViewLaunchRegister() = default;

    virtual void regLauncher(EffectFamily family, const IEffectViewLauncherPtr& launcher) = 0;
    virtual IEffectViewLauncherPtr launcher(EffectFamily family) const = 0;
};
}
