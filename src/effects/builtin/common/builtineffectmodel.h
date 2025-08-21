/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "libraries/lib-components/EffectInterface.h"

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "trackedit/iprojecthistory.h"

#include "global/async/asyncable.h"

namespace au::effects {
class BuiltinEffectModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(int instanceId READ instanceId CONSTANT FINAL)

    Q_PROPERTY(QString effectId READ effectId NOTIFY effectIdChanged FINAL)
    Q_PROPERTY(bool usesPresets READ usesPresets CONSTANT FINAL)

public:
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectExecutionScenario> executionScenario;
    muse::Inject<IRealtimeEffectService> realtimeEffectService;
    muse::Inject<trackedit::IProjectHistory> projectHistory;
    muse::Inject<IEffectsProvider> effectsProvider;

public:
    BuiltinEffectModel(QObject* parent = nullptr);

    int instanceId() const;
    QString effectId() const;

    Q_INVOKABLE void init();
    Q_INVOKABLE void preview();
    Q_INVOKABLE void commitSettings();

    virtual bool usesPresets() const { return true; }

signals:
    void effectIdChanged();

protected:

    virtual void doReload() = 0;
    virtual void doUpdateSettings() {}

    std::shared_ptr<effects::EffectInstance> instance() const;
    const EffectSettings& settings() const;
    void modifySettings(const std::function<void(EffectSettings& settings)>&);

    template<typename T>
    const T& settings() const
    {
        const T* st = settings().cast<T>();
        IF_ASSERT_FAILED(st) {
            static T null;
            return null;
        }
        return *st;
    }

    template<typename EffectType>
    const EffectType& effect() const
    {
        const EffectId effectId = this->effectId();
        const Effect* e = effectsProvider()->effect(effectId);
        return *dynamic_cast<const EffectType*>(e);
    }

    template<typename EffectType>
    EffectType& effect()
    {
        return const_cast<EffectType&>(static_cast<const BuiltinEffectModel*>(this)->effect<EffectType>());
    }

private:
    EffectSettingsAccessPtr settingsAccess() const;
    const int m_instanceId;
};
}
