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
}
