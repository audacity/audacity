/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QTimer>

#include "modularity/ioc.h"
#include "trackedit/iprojecthistory.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/view/abstracteffectviewmodel.h"

class VST3Instance;
class EffectSettingsAccess;
namespace au::effects {
class VstViewModel : public AbstractEffectViewModel
{
    Q_OBJECT

public:
    muse::GlobalInject<IEffectInstancesRegister> instancesRegister;

    muse::ContextInject<IEffectExecutionScenario> executionScenario{ this };
    muse::ContextInject<IRealtimeEffectService> realtimeEffectService{ this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory{ this };

public:
    VstViewModel(QObject* parent, int instanceId);
    ~VstViewModel() override;

private:
    void doInit() override;
    void doStartPreview() override;
    void doStopPreview() override;

    std::shared_ptr<EffectSettingsAccess> settingsAccess() const;
    void settingsToView();
    void settingsFromView();
    void checkSettingChangesFromUiWhileIdle();
    void checkSettingChangesFromUi(bool forceCommitting);

    std::shared_ptr<VST3Instance> m_auVst3Instance;
    std::shared_ptr<EffectSettingsAccess> m_settingsAccess;
    QTimer m_settingUpdateTimer;
};

class VstViewModelFactory : public EffectViewModelFactory<VstViewModel>
{
};
}
