/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "realtimeeffectmenumodelbase.h"
#include "effects/effects_base/effectstypes.h"
#include <QObject>

namespace au::projectscene {
class RealtimeEffectListItemMenuModel : public RealtimeEffectMenuModelBase
{
    Q_OBJECT

    Q_PROPERTY(QString effectState READ prop_effectState WRITE prop_setEffectState NOTIFY effectStateChanged)

public:
    explicit RealtimeEffectListItemMenuModel(QObject* parent = nullptr);

    QString prop_effectState() const;
    void prop_setEffectState(const QString&);

signals:
    void effectStateChanged();

private:
    void handleMenuItem(const QString& itemId) override;

    // effects::IEffectMenuItemFactory
    muse::uicomponents::MenuItem* makeMenuEffectItem(const effects::EffectId& effectId) override;
    muse::uicomponents::MenuItem* makeMenuEffect(const muse::String& title, const muse::uicomponents::MenuItemList& items) override;

    void doPopulateMenu() override;

    bool belongsWithMe(effects::TrackId) const;
    void updateEffectCheckmarks();

    effects::RealtimeEffectStatePtr m_effectState;
};
}
