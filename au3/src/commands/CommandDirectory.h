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

#include "CommandType.h"

class CommandOutputTargets;

class AUDACITY_DLL_API CommandDirectory
{
private:
    static std::unique_ptr<CommandDirectory> mInstance;
    static CommandMap& sCmdMap();

    static void AddCommand(std::unique_ptr<OldStyleCommandType> type);
public:
    /// Register a type of command with the directory with a statically
    /// constructed instance of this class.
    struct RegisterType {
        RegisterType(std::unique_ptr<OldStyleCommandType> type)
        { AddCommand(std::move(type)); }
    };

    ~CommandDirectory();

    /// If a command with the given name has been registered in the directory,
    /// return a pointer to the factory for commands of that type.
    /// Otherwise return NULL.
    OldStyleCommandType* LookUp(const wxString& cmdName) const;

    /// Get a pointer to the singleton instance
    static CommandDirectory* Get();

private:
    CommandDirectory();
};

#endif /* End of include guard: __COMMANDDIRECTORY__ */
