/**********************************************************************

  Audacity: A Digital Audio Editor

  @file CommandDispatch.h
  @brief Interpret text as a command or effect name

  Paul Licameli split from BatchCommands.h

**********************************************************************/

#ifndef __AUDACITY_COMMAND_DISPATCH
#define __AUDACITY_COMMAND_DISPATCH

#include "CommandFlag.h"
#include "Identifier.h" // for CommandID

class CommandContext;
class CommandManager;

AUDACITY_DLL_API bool HandleTextualCommand( CommandManager &commandManager,
   const CommandID & Str,
   const CommandContext & context, CommandFlag flags, bool alwaysEnabled);

#endif
