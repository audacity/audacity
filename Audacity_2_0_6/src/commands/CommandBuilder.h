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

class Command;
class wxString;

// CommandBuilder has the task of validating and interpreting a command string.
// If the string represents a valid command, it builds the command object.

class CommandBuilder
{
   private:
      bool mValid;
      Command *mCommand;
      wxString mError;

      void Failure(const wxString &msg = wxEmptyString);
      void Success(Command *cmd);
      void BuildCommand(const wxString &cmdName, wxString cmdParams);
      void BuildCommand(wxString cmdString);
   public:
      CommandBuilder(const wxString &cmdString);
      CommandBuilder(const wxString &cmdName,
                     const wxString &cmdParams);
      ~CommandBuilder();
      bool WasValid();
      Command *GetCommand();
      void Cleanup();
      const wxString &GetErrorMessage();
};
#endif /* End of include guard: __COMMANDBUILDER__ */
