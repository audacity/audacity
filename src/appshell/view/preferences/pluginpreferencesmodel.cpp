/*
 * Audacity: A Digital Audio Editor
 */
#include "pluginpreferencesmodel.h"

namespace au::appshell {
PluginPreferencesModel::PluginPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void PluginPreferencesModel::init()
{
    effectsConfiguration()->effectMenuOrganizationChanged().onNotify(this, [this] {
        emit effectMenuOrganizationChanged();
    });
    effectsConfiguration()->realtimeEffectOrganizationChanged().onNotify(this, [this] {
        emit realtimeEffectOrganizationChanged();
    });
}

au::effects::EffectMenuOrganization PluginPreferencesModel::effectMenuOrganization() const
{
    return effectsConfiguration()->effectMenuOrganization();
}

void PluginPreferencesModel::setEffectMenuOrganization(effects::EffectMenuOrganization organization)
{
    if (effectMenuOrganization() == organization) {
        return;
    }

    effectsConfiguration()->setEffectMenuOrganization(organization);
}

au::effects::EffectMenuOrganization PluginPreferencesModel::realtimeEffectOrganization() const
{
    return effectsConfiguration()->realtimeEffectOrganization();
}

void PluginPreferencesModel::setRealtimeEffectOrganization(
    effects::EffectMenuOrganization organization)
{
    if (realtimeEffectOrganization() == organization) {
        return;
    }

    effectsConfiguration()->setRealtimeEffectOrganization(organization);
}
}
