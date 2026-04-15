/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "global/modularity/imoduleinterface.h"

#include "ieffectviewlauncher.h"

namespace au::effects {
class IEffectViewLaunchRegister : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectViewLaunchRegister)

public:
    virtual ~IEffectViewLaunchRegister() = default;

    virtual void regLauncher(const std::string& family, const IEffectViewLauncherPtr& launcher) = 0;
    virtual IEffectViewLauncherPtr launcher(const std::string& family) const = 0;
};
}
