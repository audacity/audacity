/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "trackedit/iprojecthistory.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/view/abstracteffectviewmodel.h"
#include "effects/effects_base/ieffectsprovider.h"

class VST3Instance;
class EffectSettingsAccess;
namespace au::effects {
class VstViewModel : public AbstractEffectViewModel
{
    Q_OBJECT

public:
    muse::Inject<IRealtimeEffectService> realtimeEffectService{ this };
    muse::Inject<trackedit::IProjectHistory> projectHistory{ this };
    muse::Inject<IEffectsProvider> effectsProvider{ this };

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
