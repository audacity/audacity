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
    void deliverPendingUiMessages();

    std::shared_ptr<VST3Instance> m_auVst3Instance;
    std::shared_ptr<EffectSettingsAccess> m_settingsAccess;
    QTimer m_settingUpdateTimer;

    // Unlike m_settingUpdateTimer (which only does anything while inactive/idle), this
    // runs regardless of active state: it delivers the data a plugin sends to its own
    // editor while audio is being processed. See VST3Wrapper::DeliverPendingUiMessages
    // for why it must run on this (UI) thread rather than the audio thread.
    QTimer m_pluginUiUpdateTimer;
};

class VstViewModelFactory : public EffectViewModelFactory<VstViewModel>
{
};
}
