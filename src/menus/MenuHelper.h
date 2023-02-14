#pragma once

#include <functional>
#include "commands/CommandManager.h"
#include "EffectInterface.h"

class PluginDescriptor;

namespace MenuHelper
{

/// The effects come from a plug in list
/// This code iterates through the list, adding effects into
/// the menu.
MenuTable::BaseItemPtrs PopulateEffectsMenu(
   EffectType type,
   CommandFlag batchflags,
   const wxString& groupby,
   void (*onMenuCommand)(const CommandContext&),
   std::function<bool(const PluginDescriptor&)> pred = {});

}
