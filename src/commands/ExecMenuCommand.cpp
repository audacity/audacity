/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ExecMenuCommand.cpp
\brief Contains definitions for ExecMenuCommand class.

*//*******************************************************************/

#include "ExecMenuCommand.h"
#include "CommandManager.h"
#include "../Project.h"

wxString ExecMenuCommandType::BuildName()
{
   return wxT("MenuCommand");
}

void ExecMenuCommandType::BuildSignature(CommandSignature &signature)
{
   auto menuCommandValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("CommandName"), wxT(""), std::move(menuCommandValidator));
}

CommandHolder ExecMenuCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<ExecMenuCommand>(*this, std::move(target));
}

bool ExecMenuCommand::Apply(CommandExecutionContext context)
{
   CommandManager *cmdManager = context.GetProject()->GetCommandManager();

   wxString cmdName = GetString(wxT("CommandName"));
   auto cmdFlags = AlwaysEnabledFlag; // TODO ?
   auto cmdMask = AlwaysEnabledFlag;
   return cmdManager->HandleTextualCommand(cmdName, cmdFlags, cmdMask);
}
