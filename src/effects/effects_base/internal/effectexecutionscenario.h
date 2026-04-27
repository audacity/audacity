/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"
#include "framework/global/async/channel.h"
#include "framework/interactive/iinteractive.h"

#include "context/iglobalcontext.h"
#include "../ieffectinstancesregister.h"
#include "../ieffectsprovider.h"
#include "ieffectsconfiguration.h"
#include "ieffectviewcontroller.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/internal/itracknavigationcontroller.h"
#include "au3wrap/au3types.h"
#include "spectrogram/iglobalspectrogramconfiguration.h"
#include "spectrogram/ifrequencyselectioncontroller.h"
#include "playback/iplayback.h"

#include <optional>

#include "../ieffectexecutionscenario.h"

class Effect;
class EffectBase;
class EffectInstance;
class SimpleEffectSettingsAccess;

namespace BasicUI {
class ProgressDialog;
}

namespace au::effects {
class EffectExecutionScenario : public IEffectExecutionScenario, public muse::Contextable, public muse::async::Asyncable
{
    muse::GlobalInject<IEffectsConfiguration> effectsConfiguration;
    muse::GlobalInject<spectrogram::IGlobalSpectrogramConfiguration> spectrogramConfiguration;
    muse::GlobalInject<IEffectsProvider> effectsProvider;
    muse::GlobalInject<IEffectInstancesRegister> effectInstancesRegister;

    muse::ContextInject<IEffectViewController> effectViewController{ this };
    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<trackedit::ISelectionController> selectionController{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory{ this };
    muse::ContextInject<trackedit::ITrackNavigationController> trackNavigationController { this };
    muse::ContextInject<spectrogram::IFrequencySelectionController> frequencySelectionController { this };
    muse::ContextInject<playback::IPlayback> playback{ this };

public:
    EffectExecutionScenario(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    muse::Ret performEffect(const EffectId& effectId) override;
    muse::Ret performEffect(const EffectId& effectId, const std::string& params) override;
    bool lastProcessorIsAvailable() const override;
    muse::async::Notification lastProcessorIsNowAvailable() const override;
    muse::async::Channel<EffectId> lastProcessorIdChanged() const override;
    muse::Ret repeatLastProcessor() override;

    muse::Ret previewEffect(const EffectInstanceId& effectInstanceId, EffectSettings& settings) override;
    void stopPreview() override;

private:
    muse::Ret doPreviewEffect(const EffectId& effectId, EffectSettings& settings);

    std::pair<std::string, std::string> makeErrorMsg(const muse::Ret& ret, const EffectId& effectId);
    muse::Ret performEffectWithShowError(au3::Au3Project& project, const EffectId& effectId, unsigned int flags,
                                         const std::string& params = {});
    muse::Ret doPerformEffect(au3::Au3Project& project, const EffectId& effectId, unsigned int flags, const std::string& params = {});
    muse::Ret performEffectInternal(au3::Au3Project& project, Effect* effect, std::shared_ptr<EffectInstance> effectInstance,
                                    EffectSettings& settings);
    muse::Ret performGenerator(au3::Au3Project& project, Effect&, const std::shared_ptr<EffectInstanceEx>&, EffectSettings&);
    std::optional<trackedit::ClipId> performEffectOnSingleClip(au3::Au3Project&, Effect&, const std::shared_ptr<EffectInstanceEx>&,
                                                               EffectSettings&, trackedit::TrackId trackId, muse::Ret&);
    muse::Ret performEffectOnEachSelectedClip(au3::Au3Project& project, Effect&, const std::shared_ptr<EffectInstanceEx>&, EffectSettings&);
    au3::Au3Project& projectRef();

    using ShowEffectHostInterfaceCb = std::function<bool (Effect&, std::shared_ptr<EffectInstance>&, SimpleEffectSettingsAccess&)>;
    using StopPlaybackCb = std::function<void ()>;
    using SelectAllIfNoneCb = std::function<void ()>;

    bool DoEffect(const EffectId& effectId, au3::Au3Project& project, unsigned flags);

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

    muse::async::Notification m_lastProcessorIsAvailableChanged;
    muse::async::Channel<EffectId> m_lastProcessorIdChanged;
    std::optional<EffectId> m_lastProcessorId;
    std::optional<EffectPreviewState> m_effectPreviewState;
};
}
