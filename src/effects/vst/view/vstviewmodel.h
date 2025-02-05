/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"

#include "effects/effects_base/effectstypes.h"

namespace au::effects {
class VstViewModel : public QObject
{
    Q_OBJECT
    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)

public:
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectExecutionScenario> executionScenario;

public:
    VstViewModel() = default;

    int instanceId() const;
    void setInstanceId(int newInstanceId);

    Q_INVOKABLE void onApply();
    Q_INVOKABLE void preview();

signals:
    void instanceIdChanged();

private:

    EffectSettingsAccess* settingsAccess() const;
    void updateSettings();

    EffectInstanceId m_instanceId = -1;
};
}
