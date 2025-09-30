/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectsmenuprovider.h"
#include "ieffectsprovider.h"
#include "effectsconfiguration.h"

#include "global/async/asyncable.h"

namespace au::effects {
class EffectsMenuProvider : public IEffectsMenuProvider, public muse::async::Asyncable
{
    muse::Inject<effects::IEffectsConfiguration> configuration;
    muse::Inject<effects::IEffectsProvider> effectsProvider;

public:
    void init();

    muse::uicomponents::MenuItemList destructiveEffectMenu(IEffectMenuItemFactory& effectMenu, EffectFilter filter) override;
    muse::uicomponents::MenuItemList realtimeEffectMenu(IEffectMenuItemFactory& effectMenu) override;
    muse::async::Notification effectMenusChanged() const override;

private:
    muse::async::Notification m_effectMenusChanged;
};
}
