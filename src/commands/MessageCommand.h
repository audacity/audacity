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

#ifndef __MESSAGE_COMMAND__
#define __MESSAGE_COMMAND__

#include "CommandType.h"
#include "Command.h"

#define MESSAGE_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Message") }

class MessageCommand : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return MESSAGE_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Echos a message.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#message");};
public:
   wxString mMessage;
};


#endif /* End of include guard: __MESSAGECOMMAND__ */
