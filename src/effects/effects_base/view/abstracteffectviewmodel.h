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
    Q_PROPERTY(EffectInstanceId instanceId READ instanceId CONSTANT FINAL)
    Q_PROPERTY(bool isPreviewing READ isPreviewing NOTIFY isPreviewingChanged FINAL)

protected:
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectExecutionScenario> executionScenario;
    muse::Inject<au::playback::IPlayback> playback;

public:
    AbstractEffectViewModel(QObject* parent, int instanceId);
    ~AbstractEffectViewModel() override = default;

    Q_INVOKABLE void init();
    Q_INVOKABLE void startPreview();
    Q_INVOKABLE void stopPreview();

    EffectInstanceId instanceId() const;
    bool isPreviewing() const;

signals:
    void isPreviewingChanged();

protected:
    const EffectInstanceId m_instanceId;

private:
    virtual void doInit() = 0;
    virtual void doStartPreview() = 0;
};

class AbstractEffectViewModelFactory : public QObject
{
    Q_OBJECT
public:
    virtual ~AbstractEffectViewModelFactory() = default;

    Q_INVOKABLE AbstractEffectViewModel* createModel(QObject* parent, int instanceId) const
    {
        return doCreateModel(parent, instanceId);
    }

private:
    virtual AbstractEffectViewModel* doCreateModel(QObject* parent, int instanceId) const = 0;
};

template<typename T>
class EffectViewModelFactory : public AbstractEffectViewModelFactory
{
    AbstractEffectViewModel* doCreateModel(QObject* parent, int instanceId) const override
    {
        return new T(parent, instanceId);
    }
};
}
