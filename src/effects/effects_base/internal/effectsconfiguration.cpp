/*
* Audacity: A Digital Audio Editor
*/
#include "effectsconfiguration.h"

#include "global/settings.h"

using namespace au::effects;

static const std::string moduleName("effects");
static const muse::Settings::Key APPLY_EFFECT_TO_ALL_AUDIO(moduleName, "effects/applyEffectToAllAudio");
static const muse::Settings::Key EFFECT_MENU_ORGANIZATION(moduleName, "effects/effectMenuOrganization");
static const muse::Settings::Key PREVIEW_MAX_DURATION(moduleName, "effects/previewMaxDuration");
static const std::string PLUGIN_UI_MODE_PREFIX = "effects/pluginUIMode/";

static muse::Settings::Key makePluginUIModeKey(const EffectId& effectId)
{
    return { moduleName, PLUGIN_UI_MODE_PREFIX + effectId.toStdString() };
}

void EffectsConfiguration::init()
{
    muse::settings()->setDefaultValue(APPLY_EFFECT_TO_ALL_AUDIO, muse::Val(true));
    muse::settings()->valueChanged(APPLY_EFFECT_TO_ALL_AUDIO).onReceive(nullptr, [this](const muse::Val&) {
        m_applyEffectToAllAudioChanged.notify();
    });

    muse::settings()->setDefaultValue(EFFECT_MENU_ORGANIZATION, muse::Val(EffectMenuOrganization::Grouped));
    muse::settings()->valueChanged(EFFECT_MENU_ORGANIZATION).onReceive(nullptr, [this](const muse::Val&) {
        m_effectMenuOrganizationChanged.notify();
    });

    muse::settings()->setDefaultValue(PREVIEW_MAX_DURATION, muse::Val(60.0));
}

bool EffectsConfiguration::applyEffectToAllAudio() const
{
    return muse::settings()->value(APPLY_EFFECT_TO_ALL_AUDIO).toBool();
}

void EffectsConfiguration::setApplyEffectToAllAudio(bool value)
{
    if (applyEffectToAllAudio() == value) {
        return;
    }

    muse::settings()->setSharedValue(APPLY_EFFECT_TO_ALL_AUDIO, muse::Val(value));
}

muse::async::Notification EffectsConfiguration::applyEffectToAllAudioChanged() const
{
    return m_applyEffectToAllAudioChanged;
}

EffectMenuOrganization EffectsConfiguration::effectMenuOrganization() const
{
    return static_cast<EffectMenuOrganization>(muse::settings()->value(EFFECT_MENU_ORGANIZATION).toInt());
}

void EffectsConfiguration::setEffectMenuOrganization(EffectMenuOrganization organization)
{
    muse::settings()->setSharedValue(EFFECT_MENU_ORGANIZATION, muse::Val(static_cast<int>(organization)));
}

muse::async::Notification EffectsConfiguration::effectMenuOrganizationChanged() const
{
    return m_effectMenuOrganizationChanged;
}

double EffectsConfiguration::previewMaxDuration() const
{
    return muse::settings()->value(PREVIEW_MAX_DURATION).toDouble();
}

void EffectsConfiguration::setPreviewMaxDuration(double value)
{
    muse::settings()->setSharedValue(PREVIEW_MAX_DURATION, muse::Val(value));
}

PluginUIMode EffectsConfiguration::pluginUIMode(const EffectId& effectId) const
{
    if (effectId.empty()) {
        return PluginUIMode::VendorUI;
    }

    const muse::Settings::Key key = makePluginUIModeKey(effectId);
    // Set default to VendorUI (native plugin UI) for backward compatibility
    muse::settings()->setDefaultValue(key, muse::Val(static_cast<int>(PluginUIMode::VendorUI)));
    return static_cast<PluginUIMode>(muse::settings()->value(key).toInt());
}

void EffectsConfiguration::setPluginUIMode(const EffectId& effectId, PluginUIMode mode)
{
    const muse::Settings::Key key = makePluginUIModeKey(effectId);
    // Set default to VendorUI (native plugin UI) for backward compatibility
    muse::settings()->setDefaultValue(key, muse::Val(static_cast<int>(PluginUIMode::VendorUI)));

    if (pluginUIMode(effectId) == mode) {
        return;
    }

    muse::settings()->setSharedValue(key, muse::Val(static_cast<int>(mode)));
    m_pluginUIModeChanged.notify();
}

muse::async::Notification EffectsConfiguration::pluginUIModeChanged() const
{
    return m_pluginUIModeChanged;
}
