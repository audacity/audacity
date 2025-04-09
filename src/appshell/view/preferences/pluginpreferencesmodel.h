/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/ieffectsconfiguration.h"
#include "async/asyncable.h"
#include "modularity/ioc.h"

#include <QObject>

namespace au::appshell {
class PluginPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(effects::EffectMenuOrganization effectMenuOrganization READ effectMenuOrganization NOTIFY effectMenuOrganizationChanged)
    Q_PROPERTY(
        effects::EffectMenuOrganization realtimeEffectOrganization READ realtimeEffectOrganization NOTIFY realtimeEffectOrganizationChanged)

    muse::Inject<effects::IEffectsConfiguration> effectsConfiguration;

public:
    explicit PluginPreferencesModel(QObject* parent = nullptr);

    effects::EffectMenuOrganization effectMenuOrganization() const;
    Q_INVOKABLE void setEffectMenuOrganization(effects::EffectMenuOrganization);

    effects::EffectMenuOrganization realtimeEffectOrganization() const;
    Q_INVOKABLE void setRealtimeEffectOrganization(effects::EffectMenuOrganization);

    Q_INVOKABLE void init();

signals:
    void effectMenuOrganizationChanged();
    void realtimeEffectOrganizationChanged();
};
}
