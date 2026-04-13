/*
* Audacity: A Digital Audio Editor
*/
#include "effectviewlaunchregister.h"

using namespace au::effects;

void EffectViewLaunchRegister::regLauncher(EffectFamily family, const IEffectViewLauncherPtr& launcher)
{
    m_data.insert({ family, launcher });
}

IEffectViewLauncherPtr EffectViewLaunchRegister::launcher(EffectFamily family) const
{
    auto it = m_data.find(family);
    if (it != m_data.end()) {
        return it->second;
    }
    return nullptr;
}
