/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "realtimeeffectmenumodelbase.h"

namespace au::projectscene {
class AddEffectMenuModel : public RealtimeEffectMenuModelBase
{
    Q_OBJECT

public:
    explicit AddEffectMenuModel(QObject* parent = nullptr);

private:
    void handleMenuItem(const QString& itemId) override;

    // effects::IEffectMenuItemFactory
    muse::uicomponents::MenuItem* makeMenuEffectItem(const effects::EffectId& effectId) override;
    muse::uicomponents::MenuItem* makeMenuEffect(const muse::String& title, const muse::uicomponents::MenuItemList& items) override;
};
}
