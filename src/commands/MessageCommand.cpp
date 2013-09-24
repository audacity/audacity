/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file MessageCommand.cpp
\brief Definitions for MessageCommand class

*//*******************************************************************/

#include "MessageCommand.h"
#include "CommandType.h"

wxString MessageCommandType::BuildName()
{
   return wxT("Message");
}

void MessageCommandType::BuildSignature(CommandSignature &signature)
{
   Validator *stringValidator = new Validator();
   signature.AddParameter(wxT("MessageString"), wxT(""), stringValidator);
}

Command *MessageCommandType::Create(CommandOutputTarget *target)
{
   return new MessageCommand(*this, target);
}

bool MessageCommand::Apply(CommandExecutionContext WXUNUSED(context))
{
   wxString message = GetString(wxT("MessageString"));
   Status(message);
   return true;
}
