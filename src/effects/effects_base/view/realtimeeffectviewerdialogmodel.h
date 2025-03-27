/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "ieffectinstancesregister.h"
#include "ieffectsprovider.h"
#include "effectstypes.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "effects/effects_base/irealtimeeffectstateregister.h"
#include "context/iglobalcontext.h"
#include "actions/actionable.h"
#include "async/asyncable.h"

#include <QObject>

namespace au::effects {
class RealtimeEffectViewerDialogModel : public QObject, public muse::Injectable, public muse::async::Asyncable,
    public muse::actions::Actionable
{
    Q_OBJECT
    Q_PROPERTY(RealtimeEffectStateId effectStateId READ prop_effectStateId WRITE prop_setEffectStateId FINAL)
    Q_PROPERTY(QString trackName READ prop_trackName NOTIFY trackNameChanged);
    Q_PROPERTY(bool isActive READ prop_isActive WRITE prop_setIsActive NOTIFY isActiveChanged);
    Q_PROPERTY(bool isMasterEffect READ prop_isMasterEffect NOTIFY isMasterEffectChanged);

    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IRealtimeEffectService> realtimeEffectService;
    muse::Inject<IRealtimeEffectStateRegister> stateRegister;
    muse::Inject<context::IGlobalContext> globalContext;

public:
    Q_INVOKABLE void load();
    Q_INVOKABLE bool isVst3() const;

    RealtimeEffectViewerDialogModel(QObject* parent = nullptr);

    RealtimeEffectStateId prop_effectStateId() const;
    void prop_setEffectStateId(RealtimeEffectStateId effectStateId);
    QString prop_trackName() const;

    bool prop_isActive() const;
    void prop_setIsActive(bool isActive);

    bool prop_isMasterEffect() const;

signals:
    void trackNameChanged();
    void isActiveChanged();
    void isMasterEffectChanged();

private:
    void subscribe();
    void unregisterState();

    std::optional<RealtimeEffectStateId> m_stateId;
    bool m_isVst3 = false;
};
}
