/*
* Audacity: A Digital Audio Editor
*/
#include "effectsconfiguration.h"

#include "global/settings.h"

using namespace au::effects;

static const std::string moduleName("effects");
static const muse::Settings::Key APPLY_EFFECT_TO_ALL_AUDIO(moduleName, "effects/applyEffectToAllAudio");
static const muse::Settings::Key EFFECT_MENU_ORGANIZATION(moduleName, "effects/effectMenuOrganization");

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
