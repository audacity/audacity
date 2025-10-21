/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"
#include "playback/iplayback.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include <QObject>

namespace au::effects {
class AbstractEffectViewModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(EffectInstanceId instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(bool isPreviewing READ isPreviewing NOTIFY isPreviewingChanged FINAL)

protected:
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectExecutionScenario> executionScenario;
    muse::Inject<au::playback::IPlayback> playback;

public:
    AbstractEffectViewModel(QObject* parent = nullptr);
    ~AbstractEffectViewModel() override = default;

    Q_INVOKABLE void init();
    Q_INVOKABLE void togglePreview();

    EffectInstanceId instanceId() const;
    void setInstanceId(EffectInstanceId newInstanceId);

    bool isPreviewing() const;

signals:
    void instanceIdChanged();
    void isPreviewingChanged();

private:
    virtual void doInit() = 0;
    virtual void doStartPreview() = 0;

    EffectInstanceId m_instanceId = -1;
};
}
