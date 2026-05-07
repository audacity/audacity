/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/audioplugins/iregisteraudiopluginsscenario.h"
#include "framework/interactive/iinteractive.h"
#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"

#include "effectstypes.h"

#include <functional>

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
    virtual EffectMeta meta(const EffectId& effectId) const = 0;
    virtual std::string effectName(const std::string& effectId) const = 0;
    virtual std::string effectName(const effects::RealtimeEffectState& state) const = 0;
    virtual bool paramsAreInputAgnostic(const EffectId& effectId) const = 0;

    virtual muse::async::Notification effectMetaListChanged() const = 0;

    virtual bool loadEffect(const EffectId& effectId) const = 0;
    virtual Effect* effect(const EffectId& effectId) const = 0;
    virtual void setEffectActivated(const EffectId& effectId, bool activated) = 0;

    using EffectFilter = std::function<bool (const EffectMeta&)>;

    /**
     * @brief Soft rescan: plugins already in the configuration aren't reevaluated. Use `forgetPlugins` beforehand to force re-evaluation.
     */
    virtual void rescanPlugins(muse::IInteractive& interactive,
                               muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario,
                               const EffectFilter& exclude = nullptr) = 0;
    virtual void forgetPlugins(const EffectFilter& forget = nullptr) = 0;

    virtual void save() = 0;
};
}
