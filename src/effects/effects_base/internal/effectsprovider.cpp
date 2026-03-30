/*
* Audacity: A Digital Audio Editor
*/
#include "effectsprovider.h"
#include "effectsutils.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-effects/Effect.h"
#include "au3-components/EffectInterface.h"
#include "au3-effects/EffectManager.h"
#include "au3-realtime-effects/RealtimeEffectState.h"

#include "au3-module-manager/PluginManager.h"

#include "framework/global/log.h"

using namespace muse;
using namespace au::effects;

void EffectsProvider::init()
{
    knownPluginsRegister()->pluginInfoListChanged().onNotify(this, [this]() {
        reloadEffects();
    });
}

void EffectsProvider::reloadEffects()
{
    m_effects.clear();

    const auto knownPlugins = knownPluginsRegister()->pluginInfoList();
    std::for_each(knownPlugins.begin(), knownPlugins.end(),
                  [this](const muse::audioplugins::AudioPluginInfo& info) {
        if (info.enabled) {
            m_effects.push_back(utils::museToAuEffectMeta(info.path, info.meta));
        }
    });

    m_effectsChanged.notify();
}

EffectMetaList EffectsProvider::effectMetaList() const
{
    return m_effects;
}

muse::async::Notification EffectsProvider::effectMetaListChanged() const
{
    return m_effectsChanged;
}

EffectMeta EffectsProvider::meta(const EffectId& effectId) const
{
    for (const EffectMeta& meta : m_effects) {
        if (meta.id == effectId) {
            return meta;
        }
    }

    LOGE() << "not found meta: " << effectId;
    return EffectMeta();
}

bool EffectsProvider::loadEffect(const EffectId& effectId) const
{
    const auto it = std::find_if(m_effects.begin(), m_effects.end(), [&](const EffectMeta& meta) {
        return meta.id == effectId;
    });
    if (it == m_effects.end()) {
        return false;
    }
    auto loader = effectLoadersRegister()->loader(it->family);
    if (!loader) {
        LOGE() << "no loader for family: " << static_cast<int>(it->family);
        return false;
    }
    return loader->ensurePluginIsLoaded(effectId);
}

std::string EffectsProvider::effectName(const std::string& effectId) const
{
    const auto desc = PluginManager::Get().GetPlugin(effectId);
    if (!desc) {
        return "";
    }
    return desc->GetSymbol().Msgid().Translation().ToStdString();
}

std::string EffectsProvider::effectName(const effects::RealtimeEffectState& state) const
{
    return effectName(state.GetID().ToStdString());
}

std::string EffectsProvider::effectSymbol(const std::string& effectId) const
{
    const auto desc = PluginManager::Get().GetPlugin(effectId);
    if (!desc) {
        return "";
    }
    return desc->GetSymbol().Internal().ToStdString();
}

bool EffectsProvider::supportsMultipleClipSelection(const EffectId& effectId) const
{
    for (const EffectMeta& meta : m_effects) {
        if (meta.id == effectId) {
            return meta.supportsMultipleClipSelection;
        }
    }

    LOGE() << "not found meta: " << effectId;
    return false;
}

Effect* EffectsProvider::effect(const EffectId& effectId) const
{
    if (!loadEffect(effectId)) {
        return nullptr;
    }
    PluginID pluginID = effectId.toStdString();
    const PluginDescriptor* plug = PluginManager::Get().GetPlugin(pluginID);
    if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
        LOGE() << "plugin not available, effectId: " << effectId;
        return nullptr;
    }

    Effect* effect = dynamic_cast<Effect*>(EffectManager::Get().GetEffect(pluginID));
    IF_ASSERT_FAILED(effect) {
        LOGE() << "effect not available, effectId: " << effectId;
        return nullptr;
    }

    return effect;
}

namespace {
IEffectViewLauncherPtr getLauncher(const EffectId& effectId, const IEffectViewLaunchRegister& launchRegister)
{
    PluginID pluginID = effectId.toStdString();
    const PluginDescriptor* plug = PluginManager::Get().GetPlugin(pluginID);
    if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
        LOGE() << "plugin not available, effectId: " << effectId;
        return {};
    }

    const auto family = au::au3::wxToStdString(plug->GetEffectFamily());
    const auto launcher = launchRegister.launcher(family);
    IF_ASSERT_FAILED(launcher) {
        LOGE() << "not found launcher for family:" << family;
        return {};
    }
    return launcher;
}

void callOnLauncher(const RealtimeEffectStatePtr& state, const IEffectViewLaunchRegister& launchRegister,
                    std::function<void(const IEffectViewLauncher&, const RealtimeEffectStatePtr&)> func)
{
    IF_ASSERT_FAILED(state) {
        return;
    }
    if (const auto launcher = getLauncher(au::au3::wxToString(state->GetID()), launchRegister)) {
        func(*launcher, state);
    }
}
}

muse::Ret EffectsProvider::showEffect(const EffectId& effectId, const EffectInstanceId& instanceId)
{
    LOGD() << "try open effect: " << effectId << ", instanceId: " << instanceId;

    if (!loadEffect(effectId)) {
        return muse::make_ret(muse::Ret::Code::NotSupported);
    }

    const auto launcher = getLauncher(effectId, *viewLaunchRegister());
    if (!launcher) {
        return muse::make_ret(muse::Ret::Code::NotSupported);
    }

    Ret ret = launcher->showEffect(instanceId);

    LOGD() << "open ret: " << ret.toString();
    return ret;
}

void EffectsProvider::showEffect(const RealtimeEffectStatePtr& state) const
{
    callOnLauncher(state, *viewLaunchRegister(), [](const IEffectViewLauncher& launcher, const RealtimeEffectStatePtr& state) {
        launcher.showRealtimeEffect(state);
    });
}

void EffectsProvider::hideEffect(const RealtimeEffectStatePtr& state) const
{
    callOnLauncher(state, *viewLaunchRegister(), [](const IEffectViewLauncher& launcher, const RealtimeEffectStatePtr& state) {
        launcher.hideRealtimeEffect(state);
    });
}
