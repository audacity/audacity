/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file MessageCommand.cpp
\brief Definitions for MessageCommand class

*//*******************************************************************/

#include "../Audacity.h"
#include "MessageCommand.h"
#include "CommandType.h"
#include "CommandContext.h"

wxString MessageCommandType::BuildName()
{
   return wxT("Message");
}

void MessageCommandType::BuildSignature(CommandSignature &signature)
{
   auto stringValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("MessageString"), wxT("Connected"), std::move(stringValidator));
}

OldStyleCommandPointer MessageCommandType::Create(std::unique_ptr<CommandOutputTargets> &&target)
{
   return std::make_shared<MessageCommand>(*this);
}

bool MessageCommand::Apply(const CommandContext & context)
{
   wxString message = GetString(wxT("MessageString"));
   context.Status(message);
   return true;
}
