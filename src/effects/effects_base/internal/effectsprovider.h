/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"

#include "playback/iplayback.h"

#include "audioplugins/iknownaudiopluginsregister.h"
#include "../ieffectsconfiguration.h"
#include "../ieffectloadersregister.h"
#include "../ieffectviewlaunchregister.h"

#include "../ieffectsprovider.h"

class EffectBase;
class EffectSettingsAccess;
class TrackList;

namespace BasicUI {
class ProgressDialog;
}

namespace au::effects {
class EffectsProvider : public IEffectsProvider, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<IEffectsConfiguration> configuration;
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPluginsRegister;
    muse::GlobalInject<IEffectLoadersRegister> effectLoadersRegister;

    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<playback::IPlayback> playback{ this };
    muse::ContextInject<IEffectViewLaunchRegister> viewLaunchRegister{ this };

public:
    EffectsProvider(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    void reloadEffects();

    EffectMetaList effectMetaList() const override;
    muse::async::Notification effectMetaListChanged() const override;

    EffectMeta meta(const EffectId& effectId) const override;
    bool loadEffect(const EffectId& effectId) const override;
    std::string effectName(const std::string& effectId) const override;
    std::string effectName(const effects::RealtimeEffectState& state) const override;
    std::string effectSymbol(const std::string& effectId) const override;
    Effect* effect(const EffectId& effectId) const override;

    bool supportsMultipleClipSelection(const EffectId& effectId) const override;

    muse::Ret showEffect(const EffectId& effectId, const EffectInstanceId& instanceId) override;

    void showEffect(const RealtimeEffectStatePtr& state) const override;
    void hideEffect(const RealtimeEffectStatePtr& state) const override;

    muse::Ret previewEffect(const EffectId& effectId, EffectSettings& settings) override;
    void stopPreview() override;

private:
    struct EffectContext {
        double t0 = 0.0;
        double t1 = 0.0;
        std::shared_ptr<::TrackList> tracks;
        BasicUI::ProgressDialog* preparingPreviewProgress = nullptr;
        bool isPreview = false;
    };

    struct EffectPreviewState {
        EffectPreviewState(const EffectId& effectId, const EffectContext& originContext,
                           const std::shared_ptr<::TrackList>& previewTracks, bool loopWasActive)
            : effectId(effectId), originContext(originContext), previewTracks(previewTracks), loopWasActive(loopWasActive) {}
        const EffectId effectId;
        const EffectContext originContext;
        const std::shared_ptr<::TrackList> previewTracks;
        const bool loopWasActive;
    };

    muse::Ret doEffectPreview(EffectBase& effect, EffectSettings& settings);

    mutable EffectMetaList m_effects;
    muse::async::Notification m_effectsChanged;
    std::optional<EffectPreviewState> m_effectPreviewState;
};
}
