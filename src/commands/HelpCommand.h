/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

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

class HelpCommand : public AudacityCommand
{
public:
   static const ComponentInterfaceSymbol Symbol;
   int mFormat;

   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;};
   TranslatableString GetDescription() override {return XO("Gives help on a command.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;
   bool ApplyInner(const CommandContext & context);

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#help");};
public:
   wxString mCommandName;
};

class CommentCommand : public AudacityCommand
{
public:
   static const ComponentInterfaceSymbol Symbol;
   int mFormat;

   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;};
   TranslatableString GetDescription() override {return XO("For comments in a macro.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override {
      return true;
   };
   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#comment");};
public:
   wxString mComment;
};


#endif /* End of include guard: __HELPCOMMAND__ */
