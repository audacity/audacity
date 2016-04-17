/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file MessageCommand.h
\brief Contains definition of MessageCommand class.

*//***************************************************************//***

\class MessageCommand
\brief Command to send a message (currently on the status channel)

*//*******************************************************************/

#ifndef __MESSAGECOMMAND__
#define __MESSAGECOMMAND__

#include "Command.h"
#include "CommandType.h"

class MessageCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class MessageCommand final : public CommandImplementation
{
public:
   MessageCommand(CommandType &type,
                  std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target)) {}
   bool Apply(CommandExecutionContext context) override;
};

#endif /* End of include guard: __MESSAGECOMMAND__ */
