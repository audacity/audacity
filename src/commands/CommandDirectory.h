/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandDirectory.h
\brief Contains declarations for CommandDirectory class

\class CommandDirectory
\brief Allows registration and lookup (by name) of command types.

A singleton. This fulfills a similar purpose to CommandManager, but for general
Commands rather than menu items. Eventually they could be unified but for now
they are kept separate to make things simpler.

*//*******************************************************************/

#ifndef __COMMANDDIRECTORY__
#define __COMMANDDIRECTORY__

#include "../MemoryX.h"
#include "CommandMisc.h"
#include "CommandType.h"

class Command;
class CommandOutputTarget;

class CommandDirectory
{
private:
   static std::unique_ptr<CommandDirectory> mInstance;
   CommandMap mCmdMap;
public:
   ~CommandDirectory();

   /// If a command with the given name has been registered in the directory,
   /// return a pointer to the factory for commands of that type.
   /// Otherwise return NULL.
   CommandType *LookUp(const wxString &cmdName) const;

   /// Register a type of command with the directory.
   void AddCommand(movable_ptr<CommandType> &&type);

   /// Get a pointer to the singleton instance
   static CommandDirectory *Get();

private:
   CommandDirectory();
};

#endif /* End of include guard: __COMMANDDIRECTORY__ */
