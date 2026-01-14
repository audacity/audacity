/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectinstancesregister.h"
#include "ieffectsprovider.h"
#include "ieffectsconfiguration.h"
#include "effectstypes.h"
#include "realtimeeffectviewerdialogmodel.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include <QObject>

namespace au::effects {
class DestructiveEffectViewerDialogModel : public QObject, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)
    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(bool useVendorUI READ useVendorUI NOTIFY useVendorUIChanged FINAL)
    Q_PROPERTY(EffectFamily effectFamily READ effectFamily NOTIFY effectFamilyChanged FINAL)
    Q_PROPERTY(ViewerComponentType viewerComponentType READ viewerComponentType NOTIFY viewerComponentTypeChanged FINAL)

    muse::GlobalInject<IEffectsConfiguration> configuration;

    muse::Inject<IEffectInstancesRegister> instancesRegister{ this };
    muse::Inject<IEffectsProvider> effectsProvider{ this };

public:
    explicit DestructiveEffectViewerDialogModel(QObject* parent = nullptr);
    ~DestructiveEffectViewerDialogModel() override = default;

    Q_INVOKABLE void load();
    Q_INVOKABLE void refreshUIMode();

    QString title() const;
    int instanceId() const;
    void setInstanceId(int newInstanceId);
    bool useVendorUI() const;
    EffectFamily effectFamily() const;
    ViewerComponentType viewerComponentType() const;

signals:
    void titleChanged();
    void instanceIdChanged();
    void useVendorUIChanged();
    void effectFamilyChanged();
    void viewerComponentTypeChanged();

private:
    QString m_title;
    int m_instanceId = -1;
    EffectId m_effectId;
};
}
