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
//! Helper class that provides effect preview functionality via composition.
//! Use this when you can't inherit from AbstractEffectViewModel (e.g., when
//! your model needs to inherit from QAbstractListModel instead of QObject).
class EffectPreviewHelper : public QObject, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectExecutionScenario> executionScenario;
    muse::Inject<au::playback::IPlayback> playback;

public:
    explicit EffectPreviewHelper(QObject* parent, EffectInstanceId instanceId);
    ~EffectPreviewHelper() override = default;

    void init();
    void startPreview();
    void stopPreview();
    bool isPreviewing() const;

    EffectInstanceId instanceId() const { return m_instanceId; }

signals:
    void isPreviewingChanged();

private:
    EffectInstanceId m_instanceId;
};
}
