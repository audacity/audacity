/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ieffectexecutionscenario.h"

#include "async/channel.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "../ieffectsprovider.h"
#include "../ieffectinstancesregister.h"

#include <optional>

class AudacityProject;
class Effect;
class EffectBase;
class EffectInstance;
class SimpleEffectSettingsAccess;
namespace au::effects {
class EffectExecutionScenario : public IEffectExecutionScenario
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectInstancesRegister> effectInstancesRegister;

public:
    EffectExecutionScenario() = default;

    muse::Ret performEffect(const EffectId& effectId) override;
    bool lastProcessorIsAvailable() const override;
    muse::async::Notification lastProcessorIsNowAvailable() const override;
    muse::async::Channel<EffectId> lastProcessorIdChanged() const override;
    muse::Ret repeatLastProcessor() override;

    muse::Ret previewEffect(const EffectInstanceId& effectInstanceId, EffectSettings& settings) override;

private:

    muse::Ret doPerformEffect(AudacityProject& project, const EffectId& effectId, unsigned int flags);
    AudacityProject& projectRef();

    using ShowEffectHostInterfaceCb = std::function<bool (Effect&, std::shared_ptr<EffectInstance>&, SimpleEffectSettingsAccess&)>;
    using StopPlaybackCb = std::function<void ()>;
    using SelectAllIfNoneCb = std::function<void ()>;

    bool DoEffect(const EffectId& effectId, AudacityProject& project, unsigned flags);

    muse::async::Notification m_lastProcessorIsAvailableChanged;
    muse::async::Channel<EffectId> m_lastProcessorIdChanged;
    std::optional<EffectId> m_lastProcessorId;
};
}
