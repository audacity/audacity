/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandBuilder.h
\brief Contains declaration of CommandBuilder class.

*//*******************************************************************/

#ifndef __COMMANDBUILDER__
#define __COMMANDBUILDER__

#include "../MemoryX.h"
#include <wx/string.h>

class Command;
using CommandHolder = std::shared_ptr<Command>;
class wxString;

// CommandBuilder has the task of validating and interpreting a command string.
// If the string represents a valid command, it builds the command object.

class CommandBuilder
{
   private:
      bool mValid;
      CommandHolder mCommand;
      wxString mError;

      void Failure(const wxString &msg = wxEmptyString);
      void Success(const CommandHolder &cmd);
      void BuildCommand(const wxString &cmdName, const wxString &cmdParams);
      void BuildCommand(const wxString &cmdString);
   public:
      CommandBuilder(const wxString &cmdString);
      CommandBuilder(const wxString &cmdName,
                     const wxString &cmdParams);
      ~CommandBuilder();
      bool WasValid();
      CommandHolder GetCommand();
      const wxString &GetErrorMessage();
};
#endif /* End of include guard: __COMMANDBUILDER__ */
