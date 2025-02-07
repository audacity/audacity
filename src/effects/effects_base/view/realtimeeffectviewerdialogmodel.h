/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "ieffectinstancesregister.h"
#include "ieffectsprovider.h"
#include "effectstypes.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "context/iglobalcontext.h"
#include "actions/actionable.h"
#include "async/asyncable.h"

#include <QObject>

namespace au::effects {
class RealtimeEffectViewerDialogModel : public QObject, public muse::Injectable, public muse::async::Asyncable,
    public muse::actions::Actionable
{
    Q_OBJECT
    Q_PROPERTY(QString effectState READ prop_effectState WRITE prop_setEffectState FINAL)
    Q_PROPERTY(QString trackName READ prop_trackName NOTIFY trackNameChanged);
    Q_PROPERTY(bool isActive READ prop_isActive WRITE prop_setIsActive NOTIFY isActiveChanged);
    Q_PROPERTY(bool isMasterEffect READ prop_isMasterEffect NOTIFY isMasterEffectChanged);

    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<effects::IRealtimeEffectService> realtimeEffectService;
    muse::Inject<context::IGlobalContext> globalContext;

public:
    Q_INVOKABLE void load();

    RealtimeEffectViewerDialogModel(QObject* parent = nullptr);
    ~RealtimeEffectViewerDialogModel() override;

    QString prop_effectState() const;
    void prop_setEffectState(const QString& effectState);
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

    RealtimeEffectStatePtr m_effectState;
};
}
