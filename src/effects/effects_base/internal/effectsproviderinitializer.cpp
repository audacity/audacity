/*
 * Audacity: A Digital Audio Editor
 */
#include "effectsproviderinitializer.h"

namespace au::effects {
void EffectsProviderInitializer::callAfterSplashScreen()
{
    static bool effectsProviderInitialized = false;
    if (effectsProviderInitialized) {
        return;
    }
    effectsProvider()->initOnce(*interactive(), *registerAudioPluginsScenario());
    effectsProviderInitialized = true;
}
} // namespace au::effects
