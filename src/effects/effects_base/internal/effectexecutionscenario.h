/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"

#include "async/channel.h"
#include "context/iglobalcontext.h"
#include "global/iinteractive.h"

#include "../ieffectinstancesregister.h"
#include "../ieffectsprovider.h"
#include "ieffectsconfiguration.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/internal/itracknavigationcontroller.h"
#include "au3wrap/au3types.h"

#include <optional>

#include "../ieffectexecutionscenario.h"

class Effect;
class EffectBase;
class EffectInstance;
class SimpleEffectSettingsAccess;
namespace au::effects {
class EffectExecutionScenario : public IEffectExecutionScenario, public muse::Injectable
{
    muse::GlobalInject<IEffectsConfiguration> effectsConfiguration;

    muse::Inject<context::IGlobalContext> globalContext{ this };
    muse::Inject<IEffectsProvider> effectsProvider{ this };
    muse::Inject<IEffectInstancesRegister> effectInstancesRegister{ this };
    muse::Inject<trackedit::ISelectionController> selectionController{ this };
    muse::Inject<muse::IInteractive> interactive{ this };
    muse::Inject<trackedit::IProjectHistory> projectHistory{ this };
    muse::Inject<trackedit::ITrackNavigationController> trackNavigationController { this };

public:
    EffectExecutionScenario(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    muse::Ret performEffect(const EffectId& effectId) override;
    bool lastProcessorIsAvailable() const override;
    muse::async::Notification lastProcessorIsNowAvailable() const override;
    muse::async::Channel<EffectId> lastProcessorIdChanged() const override;
    muse::Ret repeatLastProcessor() override;

    muse::Ret previewEffect(const EffectInstanceId& effectInstanceId, EffectSettings& settings) override;

private:

    std::pair<std::string, std::string> makeErrorMsg(const muse::Ret& ret, const EffectId& effectId);
    muse::Ret performEffectWithShowError(au3::Au3Project& project, const EffectId& effectId, unsigned int flags);
    muse::Ret doPerformEffect(au3::Au3Project& project, const EffectId& effectId, unsigned int flags);
    muse::Ret performGenerator(au3::Au3Project& project, Effect&, const std::shared_ptr<EffectInstanceEx>&, EffectSettings&);
    std::optional<trackedit::ClipId> performEffectOnSingleClip(au3::Au3Project&, Effect&, const std::shared_ptr<EffectInstanceEx>&,
                                                               EffectSettings&, trackedit::TrackId trackId, muse::Ret&);
    muse::Ret performEffectOnEachSelectedClip(au3::Au3Project& project, Effect&, const std::shared_ptr<EffectInstanceEx>&, EffectSettings&);
    au3::Au3Project& projectRef();

    using ShowEffectHostInterfaceCb = std::function<bool (Effect&, std::shared_ptr<EffectInstance>&, SimpleEffectSettingsAccess&)>;
    using StopPlaybackCb = std::function<void ()>;
    using SelectAllIfNoneCb = std::function<void ()>;

    bool DoEffect(const EffectId& effectId, au3::Au3Project& project, unsigned flags);

    muse::async::Notification m_lastProcessorIsAvailableChanged;
    muse::async::Channel<EffectId> m_lastProcessorIdChanged;
    std::optional<EffectId> m_lastProcessorId;
};
}
