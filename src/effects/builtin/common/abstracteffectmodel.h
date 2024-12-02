/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "libraries/lib-components/EffectInterface.h"

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"

class Effect;
namespace au::effects {
class AbstractEffectModel : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString instanceId READ instanceId_prop WRITE setInstanceId_prop NOTIFY instanceIdChanged FINAL)

    Q_PROPERTY(QString effectId READ effectId_prop NOTIFY effectIdChanged FINAL)

public:
    muse::Inject<IEffectInstancesRegister> effectInstancesRegister;
    muse::Inject<IEffectExecutionScenario> effectExecutionScenario;

public:
    AbstractEffectModel(QObject* parent = nullptr);

    QString instanceId_prop() const;
    void setInstanceId_prop(const QString& newInstanceId);
    QString effectId_prop() const;

    Q_INVOKABLE void init();
    Q_INVOKABLE void preview();

    EffectInstanceId instanceId() const;
    bool inited() const;

signals:
    void instanceIdChanged();
    void effectIdChanged();

protected:

    virtual void doInit() = 0;

    Effect* effect() const;
    EffectSettings* settings() const;

    template<typename T>
    const T& settings() const
    {
        auto pSettings = settings()->cast<T>();
        assert(pSettings);
        return *pSettings;
    }

    template<typename T>
    T& mutSettings()
    {
        auto pSettings = settings()->cast<T>();
        assert(pSettings);
        return *pSettings;
    }

private:
    QString m_instanceId;
    bool m_inited = false;
};
}
