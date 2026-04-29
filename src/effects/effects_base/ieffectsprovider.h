/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/audioplugins/iregisteraudiopluginsscenario.h"
#include "framework/interactive/iinteractive.h"
#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"

#include "effectstypes.h"

struct EffectSettings;
namespace au::effects {
class IEffectsProvider : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsProvider)

public:
    virtual ~IEffectsProvider() = default;

    virtual void initOnce(muse::IInteractive& interactive,
                          muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario) = 0;

    virtual EffectMetaList effectMetaList() const = 0;
    virtual muse::async::Notification initialized() const = 0;
    virtual muse::async::Notification effectMetaListChanged() const = 0;

    virtual EffectMeta meta(const EffectId& effectId) const = 0;
    virtual bool loadEffect(const EffectId& effectId) const = 0;
    virtual std::string effectName(const std::string& effectId) const = 0;
    virtual std::string effectName(const effects::RealtimeEffectState& state) const = 0;
    virtual Effect* effect(const EffectId& effectId) const = 0;

    virtual bool paramsAreInputAgnostic(const EffectId& effectId) const = 0;
};
}
