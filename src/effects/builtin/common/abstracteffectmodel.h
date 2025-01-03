/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "libraries/lib-components/EffectInterface.h"

#include "effects/effects_base/ieffectexecutionscenario.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "modularity/ioc.h"

#include "global/async/asyncable.h"

class Effect;
namespace au::effects {
class AbstractEffectModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(QString instanceId READ instanceId_prop WRITE setInstanceId_prop NOTIFY instanceIdChanged FINAL)

    Q_PROPERTY(QString effectId READ effectId_prop NOTIFY effectIdChanged FINAL)

public:
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectExecutionScenario> executionScenario;

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
    virtual void doReload() = 0;

    Effect* effect() const;
    EffectSettings* settings() const;

    template <typename T>
    const T& settings() const
    {
        EffectSettings* s = this->settings();
        if (!s) {
            static T null;
            return null;
        }
        T* st = s->cast<T>();
        assert(st);
        return *st;
    }

    template <typename T>
    T& mutSettings()
    {
        EffectSettings* s = this->settings();
        assert(s);
        if (!s) {
            static T null;
            return null;
        }
        T* st = s->cast<T>();
        assert(st);
        return *st;
    }

protected:
    bool m_inited = false;

private:
    QString m_instanceId;
};
}  // namespace au::effects
