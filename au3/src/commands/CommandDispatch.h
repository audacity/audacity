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

using PluginID = wxString;

namespace CommandDispatch {
AUDACITY_DLL_API bool HandleTextualCommand(
    const CommandID& Str, const CommandContext& context, CommandFlag flags, bool alwaysEnabled);

AUDACITY_DLL_API bool DoAudacityCommand(
    const PluginID& ID, const CommandContext& context, unsigned flags);

AUDACITY_DLL_API void OnAudacityCommand(const CommandContext& ctx);
}

#endif
