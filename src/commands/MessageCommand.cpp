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
   auto stringValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("MessageString"), wxT(""), std::move(stringValidator));
}

CommandHolder MessageCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<MessageCommand>(*this, std::move(target));
}

bool MessageCommand::Apply(CommandExecutionContext WXUNUSED(context))
{
   wxString message = GetString(wxT("MessageString"));
   Status(message);
   return true;
}
