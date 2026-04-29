/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "au3-components/EffectInterface.h"

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectexecutionscenario.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "effects/effects_base/view/abstracteffectviewmodel.h"
#include "trackedit/iprojecthistory.h"

#include "builtineffectinstanceaccess.h"

namespace au::effects {
class BuiltinEffectModel : public AbstractEffectViewModel, public BuiltinEffectInstanceAccess
{
    Q_OBJECT
    Q_PROPERTY(QString effectId READ effectId NOTIFY effectIdChanged FINAL)
    Q_PROPERTY(bool usesPresets READ usesPresets CONSTANT FINAL)

public:
    muse::ContextInject<IEffectExecutionScenario> executionScenario{ this };
    muse::ContextInject<IRealtimeEffectService> realtimeEffectService{ this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory{ this };

    BuiltinEffectModel(QObject* parent, int instanceId);

    QString effectId() const;

    Q_INVOKABLE void commitSettings();

    virtual bool usesPresets() const { return true; }

signals:
    void effectIdChanged();

protected:
    virtual void doReload() = 0;
    virtual void doUpdateSettings() {}

private:
    void doInit() override;
    void doStartPreview() override;
    void doStopPreview() override;
};
}
