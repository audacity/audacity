/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectinstancesregister.h"
#include "ieffectsprovider.h"

#include "modularity/ioc.h"

#include <QObject>

namespace au::effects {
class EffectViewerDialogModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)
    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)

    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectsProvider> effectsProvider;

public:
    explicit EffectViewerDialogModel(QObject* parent = nullptr);
    ~EffectViewerDialogModel() override = default;

    QString title() const;
    int instanceId() const;
    void setInstanceId(int newInstanceId);

signals:
    void titleChanged();
    void instanceIdChanged();

private:
    QString m_title;
    int m_instanceId = -1;
};
}
