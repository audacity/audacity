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

    void regLauncher(const std::string& family, const IEffectViewLauncherPtr& launcher) override;
    IEffectViewLauncherPtr launcher(const std::string& family) const override;

private:

    std::map<std::string, IEffectViewLauncherPtr> m_data;
};
}
