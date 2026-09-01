/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "realtimeeffectmenumodelbase.h"

#include "context/iglobalcontext.h"

namespace au::projectscene {
class AddEffectMenuModel : public RealtimeEffectMenuModelBase
{
    Q_OBJECT

    muse::ContextInject<context::IGlobalContext> globalContext{ this };

public:
    explicit AddEffectMenuModel(QObject* parent = nullptr);

private:
    void handleMenuItem(const QString& itemId) override;

    // effects::IEffectMenuItemFactory
    muse::uicomponents::MenuItem* makeMenuEffectItem(const effects::EffectId& effectId) override;
    muse::uicomponents::MenuItem* makeMenuEffect(const muse::String& title, const muse::uicomponents::MenuItemList& items) override;
};
}
