/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file HelpCommand
\brief Declarations of HelpCommand and HelpCommandType classes

\class HelpCommand
\brief Command which returns information about the given command

*//*******************************************************************/

#ifndef __HELPCOMMAND__
#define __HELPCOMMAND__

#include "CommandType.h"
#include "Command.h"

#define HELP_PLUGIN_SYMBOL XO("Help")

class HelpCommand : public AudacityCommand
{
public:
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return HELP_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Gives help on a command.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Help");};
public:
   wxString mCommandName;
};


#endif /* End of include guard: __HELPCOMMAND__ */
