/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectstypes.h"

#include "global/modularity/ioc.h"
#include "global/async/notification.h"
#include "uicomponents/view/menuitem.h"

namespace au::effects {
enum class EffectFilter {
    GeneratorsOnly,
    ProcessorsOnly,
    RealtimeProcessorsOnly,
};

class IEffectsMenuProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsMenuProvider)

public:
    virtual ~IEffectsMenuProvider() = default;

    virtual muse::uicomponents::MenuItemList destructiveEffectMenu(IEffectMenuItemFactory& effectMenu, EffectFilter filter) = 0;
    virtual muse::uicomponents::MenuItemList realtimeEffectMenu(IEffectMenuItemFactory& effectMenu) = 0;
    virtual muse::async::Notification effectMenusChanged() const = 0;
};
}
