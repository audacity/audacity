/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "realtimeeffectmenumodelbase.h"
#include "effects/effects_base/effectstypes.h"
#include "context/iglobalcontext.h"
#include <QObject>
#include <map>

namespace au::projectscene {
class RealtimeEffectListItemMenuModel : public RealtimeEffectMenuModelBase
{
    Q_OBJECT

    Q_PROPERTY(QVariantList availableEffects READ availableEffects)
    Q_PROPERTY(effects::RealtimeEffectStatePtr effectState READ prop_effectState WRITE prop_setEffectState NOTIFY effectStateChanged)

public:
    explicit RealtimeEffectListItemMenuModel(QObject* parent = nullptr);

    QVariantList availableEffects() const;

    effects::RealtimeEffectStatePtr prop_effectState() const;
    void prop_setEffectState(effects::RealtimeEffectStatePtr);

signals:
    void effectStateChanged();

private:
    void handleMenuItem(const QString& itemId) override;

    void doLoad() override;
    void doPopulateMenu() override;

    bool belongsWithMe(effects::TrackId) const;
    void updateEffectCheckmarks();

    effects::RealtimeEffectStatePtr m_effectState;
};
}
