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
   Validator *menuCommandValidator(new Validator());
   signature.AddParameter(wxT("CommandName"), wxT(""), menuCommandValidator);
}

Command *ExecMenuCommandType::Create(CommandOutputTarget *target)
{
   return new ExecMenuCommand(*this, target);
}

bool ExecMenuCommand::Apply(CommandExecutionContext context)
{
   CommandManager *cmdManager = context.GetProject()->GetCommandManager();

   wxString cmdName = GetString(wxT("CommandName"));
   wxUint32 cmdFlags = 0; // TODO ?
   wxUint32 cmdMask = 0;
   return cmdManager->HandleTextualCommand(cmdName, cmdFlags, cmdMask);
}
