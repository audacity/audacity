/**********************************************************************

  Audacity: A Digital Audio Editor

  @file CommandDispatch.cpp
  @brief implements function HandleTextualCommand

  Paul Licameli split from BatchCommands.cpp

**********************************************************************/

#include "CommandDispatch.h"

#include "CommandManager.h"
#include "PluginManager.h"
#include "../effects/EffectManager.h"
#include "../effects/EffectUI.h"

bool HandleTextualCommand( CommandManager &commandManager,
   const CommandID & Str,
   const CommandContext & context, CommandFlag flags, bool alwaysEnabled)
{
   switch ( commandManager.HandleTextualCommand(
      Str, context, flags, alwaysEnabled) ) {
   case CommandManager::CommandSuccess:
      return true;
   case CommandManager::CommandFailure:
      return false;
   case CommandManager::CommandNotFound:
   default:
      break;
   }

   // Not one of the singleton commands.
   // We could/should try all the list-style commands.
   // instead we only try the effects.
   EffectManager & em = EffectManager::Get();
   for (auto &plug : PluginManager::Get().PluginsOfType(PluginTypeEffect))
      if (em.GetCommandIdentifier(plug.GetID()) == Str)
         return EffectUI::DoEffect(
            plug.GetID(), context,
            EffectManager::kConfigured);

   return false;
}
