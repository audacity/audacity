/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectinstancesregister.h"
#include "ieffectsprovider.h"
#include "ieffectsconfiguration.h"

#include "modularity/ioc.h"

#include <QObject>

namespace au::effects {
class EffectViewerDialogModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)
    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(bool useVendorUI READ useVendorUI NOTIFY useVendorUIChanged FINAL)

    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectsConfiguration> configuration;

public:
    explicit EffectViewerDialogModel(QObject* parent = nullptr);
    ~EffectViewerDialogModel() override = default;

    QString title() const;
    int instanceId() const;
    void setInstanceId(int newInstanceId);
    bool useVendorUI() const;

    Q_INVOKABLE void refreshUIMode();

signals:
    void titleChanged();
    void instanceIdChanged();
    void useVendorUIChanged();

private:
    QString m_title;
    int m_instanceId = -1;
    EffectId m_effectId;
};
}
