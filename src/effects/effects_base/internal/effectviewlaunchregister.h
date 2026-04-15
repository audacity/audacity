/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>

#include "../ieffectviewlaunchregister.h"

namespace au::effects {
class EffectViewLaunchRegister : public IEffectViewLaunchRegister
{
public:
    EffectViewLaunchRegister() = default;

    void regLauncher(EffectFamily family, const IEffectViewLauncherPtr& launcher) override;
    IEffectViewLauncherPtr launcher(EffectFamily family) const override;

private:

    std::map<EffectFamily, IEffectViewLauncherPtr> m_data;
};
}
