/**********************************************************************

   Sneedacity - A Digital Audio Editor
   Copyright 1999-2018 Sneedacity Team
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

class HelpCommand : public SneedacityCommand
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

   // SneedacityCommand overrides
   ManualPageID ManualPage() override {return L"Extra_Menu:_Scriptables_II#help";}
public:
   wxString mCommandName;
};

class CommentCommand : public SneedacityCommand
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
   // SneedacityCommand overrides
   ManualPageID ManualPage() override {return L"Extra_Menu:_Scriptables_II#comment";}
public:
   wxString mComment;
};


#endif /* End of include guard: __HELPCOMMAND__ */
