/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectinstancesregister.h"

class Effect;
namespace au::effects {
class AbstractEffectModel : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)

    muse::Inject<IEffectInstancesRegister> effectInstancesRegister;

public:
    AbstractEffectModel(QObject* parent = nullptr);

    QString instanceId() const;
    void setInstanceId(const QString& newInstanceId);

signals:
    void instanceIdChanged();

protected:

    Effect* effect() const;
    EffectSettings* settings() const;

private:
    QString m_instanceId;
};
}
