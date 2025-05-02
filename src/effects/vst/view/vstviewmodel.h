/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "trackedit/iprojecthistory.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"
#include "effects/effects_base/irealtimeeffectservice.h"

#include "effects/effects_base/effectstypes.h"

class VST3Instance;
class EffectSettingsAccess;
namespace au::effects {
class VstViewModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)

public:
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectExecutionScenario> executionScenario;
    muse::Inject<IRealtimeEffectService> realtimeEffectService;
    muse::Inject<trackedit::IProjectHistory> projectHistory;

public:
    VstViewModel() = default;
    ~VstViewModel() override;

    int instanceId() const;
    void setInstanceId(int newInstanceId);

    Q_INVOKABLE void init();
    Q_INVOKABLE void preview();

signals:
    void instanceIdChanged();

private:

    std::shared_ptr<EffectSettingsAccess> settingsAccess() const;
    void settingsToView();
    void settingsFromView();
    void checkSettingChangesFromUiWhileIdle();
    void checkSettingChangesFromUi(bool forceCommitting);

    EffectInstanceId m_instanceId = -1;
    std::shared_ptr<VST3Instance> m_auVst3Instance;
    std::shared_ptr<EffectSettingsAccess> m_settingsAccess;
    QTimer m_settingUpdateTimer;
};
}
