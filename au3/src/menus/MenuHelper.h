#pragma once

#include <functional>
#include "MenuRegistry.h"
#include "EffectInterface.h"

class PluginDescriptor;

namespace MenuHelper {
using Group = MenuRegistry::GroupItem<MenuRegistry::Traits>;

/// The effects come from a plug in list
/// This code iterates through the list, adding effects into
/// the menu.
void PopulateEffectsMenu(
    Group& menuItems, EffectType type, CommandFlag batchflags, const wxString& groupby, void (*onMenuCommand)(
        const CommandContext&), std::function<bool(const PluginDescriptor&)> pred = {});
}
