/*
 * Audacity: A Digital Audio Editor
 */
#include "effectsproviderinitializer.h"

namespace au::effects {
void EffectsProviderInitializer::setStartupPluginValidationPolicy(StartupPluginValidationPolicy policy)
{
    m_validationPolicy = policy;
}

void EffectsProviderInitializer::callAfterSplashScreen()
{
    static bool effectsProviderInitialized = false;
    if (effectsProviderInitialized) {
        return;
    }
    effectsProvider()->initOnce(iocContext(), *registerAudioPluginsScenario(), m_validationPolicy);
    effectsProviderInitialized = true;
}
} // namespace au::effects
