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

class VST3Instance;
class EffectSettingsAccess;
namespace au::effects {
class VstViewModel : public AbstractEffectViewModel
{
    Q_OBJECT

public:
    muse::Inject<IRealtimeEffectService> realtimeEffectService;
    muse::Inject<trackedit::IProjectHistory> projectHistory;

public:
    VstViewModel() = default;
    ~VstViewModel() override;

private:
    void doInit() override;
    void doStartPreview() override;

    std::shared_ptr<EffectSettingsAccess> settingsAccess() const;
    void settingsToView();
    void settingsFromView();
    void checkSettingChangesFromUiWhileIdle();
    void checkSettingChangesFromUi(bool forceCommitting);

    std::shared_ptr<VST3Instance> m_auVst3Instance;
    std::shared_ptr<EffectSettingsAccess> m_settingsAccess;
    QTimer m_settingUpdateTimer;
};
}
