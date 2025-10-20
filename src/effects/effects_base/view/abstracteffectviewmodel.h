/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"

#include "framework/global/modularity/ioc.h"

#include <QObject>

namespace au::effects {
class AbstractEffectViewModel : public QObject
{
    Q_OBJECT

protected:
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectExecutionScenario> executionScenario;

public:
    AbstractEffectViewModel(QObject* parent = nullptr);
    ~AbstractEffectViewModel() override = default;
};
}
