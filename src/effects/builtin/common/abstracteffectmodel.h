/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"

class Effect;
namespace au::effects {
class AbstractEffectModel : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString instanceId READ instanceId_prop WRITE setInstanceId_prop NOTIFY instanceIdChanged FINAL)

public:
    muse::Inject<IEffectInstancesRegister> effectInstancesRegister;
    muse::Inject<IEffectExecutionScenario> effectExecutionScenario;

public:
    AbstractEffectModel(QObject* parent = nullptr);

    QString instanceId_prop() const;
    void setInstanceId_prop(const QString& newInstanceId);

    EffectInstanceId instanceId() const;

signals:
    void instanceIdChanged();

protected:

    Effect* effect() const;

private:
    QString m_instanceId;
};
}
