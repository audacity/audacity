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
