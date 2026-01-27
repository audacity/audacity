/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "global/iinteractive.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"
#include "effects/builtin/ibuiltineffectsrepository.h"
#include "effects/lv2/ilv2effectsrepository.h"
#include "effects/vst/ivsteffectsrepository.h"
#include "effects/nyquist/inyquisteffectsrepository.h"
#include "effects/audio_unit/iaudiouniteffectsrepository.h"
#include "audioplugins/iknownaudiopluginsregister.h"
#include "../ieffectsconfiguration.h"
#include "../ieffectviewlaunchregister.h"

#include "../ieffectsprovider.h"

class EffectBase;
class EffectSettingsAccess;

namespace BasicUI {
class ProgressDialog;
}

namespace au::effects {
class EffectsProvider : public IEffectsProvider, public muse::async::Asyncable, public muse::Injectable
{
    muse::GlobalInject<IEffectsConfiguration> configuration;

    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<IBuiltinEffectsRepository> builtinEffectsRepository{ this };
    muse::Inject<ILv2EffectsRepository> lv2EffectsRepository{ this };
    muse::Inject<IVstEffectsRepository> vstEffectsRepository{ this };
    muse::Inject<INyquistEffectsRepository> nyquistEffectsRepository{ this };
    muse::Inject<IAudioUnitEffectsRepository> audioUnitEffectsRepository{ this };
    muse::Inject<muse::IInteractive> interactive{ this };
    muse::Inject<playback::IPlayback> playback{ this };
    muse::Inject<IEffectViewLaunchRegister> viewLaunchRegister{ this };
    muse::Inject<muse::audioplugins::IKnownAudioPluginsRegister> knownPluginsRegister{ this };

public:
    EffectsProvider(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

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

    muse::Ret performEffect(au3::Au3Project& project, Effect* effect, std::shared_ptr<EffectInstance> effectInstance,
                            EffectSettings& settings) override;

    muse::Ret previewEffect(const EffectId& effectId, EffectSettings& settings) override;
    void stopPreview() override;

private:
    struct EffectContext {
        double t0 = 0.0;
        double t1 = 0.0;
        std::shared_ptr<TrackList> tracks;
        BasicUI::ProgressDialog* preparingPreviewProgress = nullptr;
        bool isPreview = false;
    };

    struct EffectPreviewState {
        EffectPreviewState(const EffectId& effectId, const EffectContext& originContext,
                           const std::shared_ptr<TrackList>& previewTracks, bool loopWasActive)
            : effectId(effectId), originContext(originContext), previewTracks(previewTracks), loopWasActive(loopWasActive) {}
        const EffectId effectId;
        const EffectContext originContext;
        const std::shared_ptr<TrackList> previewTracks;
        const bool loopWasActive;
    };

    bool isVstSupported() const;
    bool isNyquistSupported() const;
    bool isAudioUnitSupported() const;
    bool isLv2Supported() const;

    muse::Ret doEffectPreview(EffectBase& effect, EffectSettings& settings);

    mutable EffectMetaList m_effects;
    muse::async::Notification m_effectsChanged;
    std::optional<EffectPreviewState> m_effectPreviewState;
};
}
