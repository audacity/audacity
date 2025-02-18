/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "effects/effects_base/irealtimeeffectservice.h"

#include <QObject>

namespace au::projectscene {
class RealtimeEffectListItemModel : public QObject, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(bool isActive READ prop_isActive WRITE prop_setIsActive NOTIFY isActiveChanged)
    Q_PROPERTY(bool isMasterEffect READ prop_isMasterEffect CONSTANT)

    muse::Inject<effects::IEffectsProvider> effectsProvider;
    muse::Inject<effects::IRealtimeEffectService> realtimeEffectService;

public:
    RealtimeEffectListItemModel(QObject* parent, effects::RealtimeEffectStatePtr effectState);
    ~RealtimeEffectListItemModel();

    Q_INVOKABLE effects::RealtimeEffectStatePtr effectState() const;
    Q_INVOKABLE QString effectName() const;
    Q_INVOKABLE void toggleDialog();

    bool prop_isActive() const;
    void prop_setIsActive(bool isActive);
    bool prop_isMasterEffect() const;

signals:
    void isActiveChanged();

private:
    const effects::RealtimeEffectStatePtr m_effectState;
};

using RealtimeEffectListItemModelPtr = std::shared_ptr<RealtimeEffectListItemModel>;
}
