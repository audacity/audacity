/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file MessageCommand.h
\brief Contains definition of MessageCommand class.

*//***************************************************************//**

\class MessageCommand
\brief Command to send a message (currently on the status channel)

*//*******************************************************************/

#ifndef __MESSAGECOMMAND__
#define __MESSAGECOMMAND__

#include "Command.h"
#include "CommandType.h"

class MessageCommandType final : public OldStyleCommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   OldStyleCommandPointer Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class MessageCommand final : public CommandImplementation
{
public:
   MessageCommand(OldStyleCommandType &type)
      : CommandImplementation(type) {}
   bool Apply(const CommandContext &context ) override;
};

#endif /* End of include guard: __MESSAGECOMMAND__ */
