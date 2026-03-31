/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectsprovider.h"
#include "ieffectsproviderinitializer.h"

#include "framework/global/modularity/ioc.h"
#include "framework/audioplugins/iregisteraudiopluginsscenario.h"

namespace au::effects {
class EffectsProviderInitializer : public IEffectsProviderInitializer, public muse::Contextable
{
    muse::GlobalInject<IEffectsProvider> effectsProvider;
    muse::Inject<muse::audioplugins::IRegisterAudioPluginsScenario> registerAudioPluginsScenario { this };
    muse::Inject<muse::IInteractive> interactive { this };

public:
    EffectsProviderInitializer(const muse::modularity::ContextPtr& ctx)
        : Contextable(ctx) {}

    ~EffectsProviderInitializer() override = default;

    void callAfterSplashScreen() override;
};
}
