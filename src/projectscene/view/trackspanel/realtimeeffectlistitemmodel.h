/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "effects/effects_base/ieffectviewcontroller.h"

#include <QObject>

namespace au::projectscene {
class RealtimeEffectListItemModel : public QObject, public muse::Contextable, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(bool isAvailable READ prop_isAvailable CONSTANT)
    Q_PROPERTY(bool isActive READ prop_isActive WRITE prop_setIsActive NOTIFY isActiveChanged)
    Q_PROPERTY(bool isMasterEffect READ prop_isMasterEffect CONSTANT)

    muse::GlobalInject<effects::IEffectsProvider> effectsProvider;
    muse::ContextInject<effects::IRealtimeEffectService> realtimeEffectService{ this };
    muse::ContextInject<effects::IEffectViewController> effectViewController{ this };

public:
    RealtimeEffectListItemModel(QObject* parent, effects::RealtimeEffectStatePtr effectState);
    ~RealtimeEffectListItemModel();

    Q_INVOKABLE QString effectState() const;
    effects::RealtimeEffectStatePtr effectStatePtr() const;
    Q_INVOKABLE QString effectName() const;
    Q_INVOKABLE void showEffectDialog();

    bool prop_isAvailable() const;
    bool prop_isActive() const;
    void prop_setIsActive(bool isActive);
    bool prop_isMasterEffect() const;

signals:
    void isActiveChanged();

private:
    std::weak_ptr<RealtimeEffectState> m_effectState;
};

using RealtimeEffectListItemModelPtr = std::shared_ptr<RealtimeEffectListItemModel>;
}
