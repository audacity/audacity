/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/ieffectinstancesregister.h"
#include "global/modularity/ioc.h"

#include <QObject>

class CompressorInstance;

namespace au::effects {
class AbstractDynamicsEffectInstanceModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)

    muse::Inject<IEffectInstancesRegister> instancesRegister;

public:
    AbstractDynamicsEffectInstanceModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    int instanceId() const { return m_instanceId; }
    void setInstanceId(int id);

signals:
    void instanceIdChanged();

protected:
    std::weak_ptr<::CompressorInstance> m_instance;

private:
    virtual void doInit() = 0;

    int m_instanceId = -1;
};
} // namespace au::effects
