/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/audioplugins/iregisteraudiopluginsscenario.h"
#include "framework/global/async/notification.h"
#include "framework/global/async/promise.h"
#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/modularity/ioc.h"

#include "effectstypes.h"

#include <functional>

struct EffectSettings;
namespace au::effects {
class IEffectsProvider : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsProvider)

public:
    virtual ~IEffectsProvider() = default;

    virtual void initOnce(const muse::modularity::ContextPtr& ctx,
                          muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario) = 0;

    virtual EffectMetaList effectMetaList() const = 0;
    virtual EffectMeta meta(const EffectId& effectId) const = 0;
    virtual std::string effectName(const std::string& effectId) const = 0;
    virtual std::string effectName(const effects::RealtimeEffectState& state) const = 0;
    virtual std::string effectPath(const std::string& effectId) const = 0;
    virtual bool paramsAreInputAgnostic(const EffectId& effectId) const = 0;

    virtual bool hasEffectFamily(EffectFamily family) const = 0;

    virtual muse::async::Notification effectMetaListChanged() const = 0;

    // Validate-on-first-use, synchronous: if the plugin still needs its first
    // in-session validation it is run now (blocking, behind a modal dialog) and
    // this returns whether it is then loadable; otherwise returns its current
    // loadability at once. `ctx` is the caller's window context - the provider is
    // a global singleton and cannot know which window/project asked, so it hosts
    // the modal dialog in the caller's context.
    virtual bool validateEffect(const muse::modularity::ContextPtr& ctx, const EffectId& effectId) = 0;
    virtual muse::async::Promise<bool> validateEffectAsync(const EffectId&) = 0;

    virtual bool loadEffect(const EffectId& effectId) const = 0;
    // True if the effect is ready to use right now: loadable AND already validated
    // in this session. A plugin validated only in a previous session is NOT yet
    // available - it must be re-validated first (see #11746). Used to gate whether
    // a realtime-effect-list item is clickable.
    virtual bool isEffectAvailable(const EffectId& effectId) const = 0;
    virtual Effect* effect(const EffectId& effectId) const = 0;
    virtual void setEffectActivated(const EffectId& effectId, bool activated) = 0;

    using EffectFilter = std::function<bool (const EffectMeta&)>;

    /**
     * @brief Soft rescan: plugins already in the configuration aren't reevaluated. Use `forgetPlugins` beforehand to force re-evaluation.
     */
    virtual NewPluginsRegistered rescanPlugins(const muse::modularity::ContextPtr& ctx,
                                               muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario) = 0;
    virtual void forgetPlugins(const EffectFilter& forget = nullptr) = 0;

    virtual void save() = 0;
};
}
