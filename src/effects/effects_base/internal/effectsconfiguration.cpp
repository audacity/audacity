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
static const std::string EFFECT_UI_MODE_PREFIX = "effects/effectUIMode/";
static const std::string LAST_USED_PRESET_PREFIX = "effects/lastUsedPreset/";
static const muse::Settings::Key LV2_CUSTOM_PATHS(moduleName, "effects/lv2CustomPaths");
static const muse::Settings::Key VST3_CUSTOM_PATHS(moduleName, "effects/vst3CustomPaths");

static muse::io::paths_t readPathList(const muse::Settings::Key& key)
{
    const muse::ValList list = muse::settings()->value(key).toList();
    muse::io::paths_t result;
    result.reserve(list.size());
    for (const auto& item : list) {
        result.push_back(item.toPath());
    }
    return result;
}

static muse::Val toVal(const muse::io::paths_t& paths)
{
    muse::ValList list;
    list.reserve(paths.size());
    for (const auto& p : paths) {
        list.push_back(muse::Val(p));
    }
    return muse::Val(list);
}

static muse::Settings::Key makeEffectUIModeKey(const EffectId& effectId)
{
    return { moduleName, EFFECT_UI_MODE_PREFIX + effectId.toStdString() };
}

static muse::Settings::Key makeLastUsedPresetKey(const EffectId& effectId)
{
    return { moduleName, LAST_USED_PRESET_PREFIX + effectId.toStdString() };
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

    muse::settings()->setDefaultValue(LV2_CUSTOM_PATHS, muse::Val(muse::ValList {}));
    muse::settings()->valueChanged(LV2_CUSTOM_PATHS).onReceive(nullptr, [this](const muse::Val&) {
        m_lv2CustomPathsChanged.notify();
    });

    muse::settings()->setDefaultValue(VST3_CUSTOM_PATHS, muse::Val(muse::ValList {}));
    muse::settings()->valueChanged(VST3_CUSTOM_PATHS).onReceive(nullptr, [this](const muse::Val&) {
        m_vst3CustomPathsChanged.notify();
    });
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

EffectUIMode EffectsConfiguration::effectUIMode(const EffectId& effectId) const
{
    constexpr EffectUIMode defaultMode = EffectUIMode::VendorUI;
    if (effectId.empty()) {
        return defaultMode;
    }

    const muse::Settings::Key key = makeEffectUIModeKey(effectId);
    const muse::Val value = muse::settings()->value(key);
    if (value.isNull()) {
        return defaultMode;
    }

    return static_cast<EffectUIMode>(value.toInt());
}

void EffectsConfiguration::setEffectUIMode(const EffectId& effectId, EffectUIMode mode)
{
    const muse::Settings::Key key = makeEffectUIModeKey(effectId);
    if (effectUIMode(effectId) == mode) {
        return;
    }

    muse::settings()->setSharedValue(key, muse::Val(static_cast<int>(mode)));
    m_effectUIModeChanged.notify();
}

muse::async::Notification EffectsConfiguration::effectUIModeChanged() const
{
    return m_effectUIModeChanged;
}

std::string EffectsConfiguration::lastUsedPreset(const EffectId& effectId) const
{
    if (effectId.empty()) {
        return {};
    }

    const muse::Val value = muse::settings()->value(makeLastUsedPresetKey(effectId));
    if (value.isNull()) {
        return {};
    }

    return value.toString();
}

void EffectsConfiguration::setLastUsedPreset(const EffectId& effectId, const std::string& presetId)
{
    if (effectId.empty()) {
        return;
    }

    const muse::Settings::Key key = makeLastUsedPresetKey(effectId);
    if (muse::settings()->value(key).toString() == presetId) {
        return;
    }

    muse::settings()->setSharedValue(key, muse::Val(presetId));
}

muse::io::paths_t EffectsConfiguration::lv2CustomPaths() const
{
    return readPathList(LV2_CUSTOM_PATHS);
}

void EffectsConfiguration::setLv2CustomPaths(const muse::io::paths_t& paths)
{
    if (lv2CustomPaths() == paths) {
        return;
    }
    muse::settings()->setSharedValue(LV2_CUSTOM_PATHS, toVal(paths));
}

muse::async::Notification EffectsConfiguration::lv2CustomPathsChanged() const
{
    return m_lv2CustomPathsChanged;
}

muse::io::paths_t EffectsConfiguration::vst3CustomPaths() const
{
    return readPathList(VST3_CUSTOM_PATHS);
}

void EffectsConfiguration::setVst3CustomPaths(const muse::io::paths_t& paths)
{
    if (vst3CustomPaths() == paths) {
        return;
    }
    muse::settings()->setSharedValue(VST3_CUSTOM_PATHS, toVal(paths));
}

muse::async::Notification EffectsConfiguration::vst3CustomPathsChanged() const
{
    return m_vst3CustomPathsChanged;
}
