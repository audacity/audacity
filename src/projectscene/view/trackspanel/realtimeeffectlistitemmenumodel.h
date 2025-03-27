/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "realtimeeffectmenumodelbase.h"
#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/irealtimeeffectstateregister.h"
#include "context/iglobalcontext.h"
#include <QObject>

#include <map>
#include <optional>

namespace au::projectscene {
class RealtimeEffectListItemMenuModel : public RealtimeEffectMenuModelBase
{
    Q_OBJECT

    Q_PROPERTY(QVariantList availableEffects READ availableEffects)
    Q_PROPERTY(effects::RealtimeEffectStateId effectStateId READ prop_effectStateId WRITE prop_setEffectStateId NOTIFY effectStateIdChanged)

    muse::Inject<effects::IRealtimeEffectStateRegister> stateRegister;

public:
    explicit RealtimeEffectListItemMenuModel(QObject* parent = nullptr);

    QVariantList availableEffects() const;

    effects::RealtimeEffectStateId prop_effectStateId() const;
    void prop_setEffectStateId(effects::RealtimeEffectStateId);

signals:
    void effectStateIdChanged();

private:
    void handleMenuItem(const QString& itemId) override;

    void doLoad() override;
    void doPopulateMenu() override;

    bool belongsWithMe(effects::TrackId) const;
    void updateEffectCheckmarks();

    std::optional<effects::RealtimeEffectStateId> m_stateId;
};
}
