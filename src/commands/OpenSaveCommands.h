/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2018 Audacity Team.
   File License: wxwidgets

   OpenSaveCommands.h
   Stephen Parry
   James Crook

******************************************************************//**

\class OpenProjectCommand
\brief Command for opening an Audacity project

\class SaveProjectCommand
\brief Command for saving an Audacity project

*//*******************************************************************/

#include "Command.h"
#include "CommandType.h"

class OpenProjectCommand : public AudacityCommand
{
public:
   static const ComponentInterfaceSymbol Symbol;

   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;};
   TranslatableString GetDescription() override {return XO("Opens a project.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#open_project");};
public:
   wxString mFileName;
   bool mbAddToHistory;
   bool bHasAddToHistory;
};

class SaveProjectCommand : public AudacityCommand
{
public:
   static const ComponentInterfaceSymbol Symbol;

   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;};
   TranslatableString GetDescription() override {return XO("Saves a project.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#save_project");};
public:
   wxString mFileName;
   bool mbAddToHistory;
   bool mbCompress;
   bool bHasAddToHistory;
   bool bHasCompress;
};
