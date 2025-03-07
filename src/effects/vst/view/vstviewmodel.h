/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"

#include "effects/effects_base/effectstypes.h"

class VST3Instance;
class EffectSettingsAccess;
namespace au::effects {
class VstViewModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(bool isPreviewing READ isPreviewing NOTIFY isPreviewingChanged FINAL)

public:
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectExecutionScenario> executionScenario;

public:
    VstViewModel(QObject* parent = nullptr);

    int instanceId() const;
    void setInstanceId(int newInstanceId);

    bool isPreviewing() const;
    void setIsPreviewing(bool newIsPreviewing);

    Q_INVOKABLE void init();
    Q_INVOKABLE void togglePreview();

signals:
    void instanceIdChanged();
    void isPreviewingChanged();

private:

    EffectSettingsAccess* settingsAccess() const;
    void settingsToView();
    void settingsFromView();

    EffectInstanceId m_instanceId = -1;
    std::shared_ptr<VST3Instance> m_auVst3Instance;
    EffectSettingsAccess* m_settingsAccess = nullptr;
    bool m_isPreviewing = false;
    muse::ProgressPtr m_currentPreviewProgress;
};
}
