#pragma once

#include "commands/CommandManager.h"
#include "EffectInterface.h"

namespace MenuHelper
{

/// The effects come from a plug in list
/// This code iterates through the list, adding effects into
/// the menu.
MenuTable::BaseItemPtrs PopulateEffectsMenu(
   EffectType type,
   CommandFlag batchflags,
   CommandFlag realflags,
   void (*onMenuCommand)(const CommandContext&));

}
