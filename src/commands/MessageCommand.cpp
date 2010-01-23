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

bool MessageCommand::Apply(CommandExecutionContext context)
{
   wxString message = GetString(wxT("MessageString"));
   Status(message);
   return true;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: TBD
