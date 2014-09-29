/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file HelpCommand.cpp
\brief Definitions for HelpCommand and HelpCommandType classes

*//*******************************************************************/

#include "HelpCommand.h"
#include "CommandDirectory.h"
#include <wx/string.h>

wxString HelpCommandType::BuildName()
{
   return wxT("Help");
}

void HelpCommandType::BuildSignature(CommandSignature &signature)
{
   Validator *commandNameValidator = new Validator();
   signature.AddParameter(wxT("CommandName"), wxT(""), commandNameValidator);
}

Command *HelpCommandType::Create(CommandOutputTarget *target)
{
   return new HelpCommand(*this, target);
}

bool HelpCommand::Apply(CommandExecutionContext WXUNUSED(context))
{
   wxString commandName = GetString(wxT("CommandName"));
   CommandType *type = CommandDirectory::Get()->LookUp(commandName);
   if (type == NULL)
   {
      Error(wxString::Format(wxT("Command '%s' does not exist!"), commandName.c_str()));
      return false;
   }
   Status(type->Describe());
   return true;
}
