/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ExecMenuCommand.h
\brief Contains declaration of ExecMenuCommand class.

\class ExecMenuCommand
\brief A command which asks the CommandManager to execute a menu command by
name.

*//*******************************************************************/

#ifndef __EXECMENUCOMMAND__
#define __EXECMENUCOMMAND__

#include "Command.h"
#include "CommandType.h"

class ExecMenuCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class ExecMenuCommand : public CommandImplementation
{
public:
   ExecMenuCommand(CommandType &type,
                   CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }
   virtual ~ExecMenuCommand() { }
   virtual bool Apply(CommandExecutionContext context);
};

#endif /* End of include guard: __EXECMENUCOMMAND__ */
