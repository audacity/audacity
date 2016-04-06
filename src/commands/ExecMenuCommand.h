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

class ExecMenuCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class ExecMenuCommand final : public CommandImplementation
{
public:
   ExecMenuCommand(CommandType &type,
                   std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }
   virtual ~ExecMenuCommand() { }
   bool Apply(CommandExecutionContext context) override;
};

#endif /* End of include guard: __EXECMENUCOMMAND__ */
