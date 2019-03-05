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

#define OPEN_PROJECT_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Open Project2") }

class OpenProjectCommand : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return OPEN_PROJECT_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Opens a project.");};
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

#define SAVE_PROJECT_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Save Project2") }

class SaveProjectCommand : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return SAVE_PROJECT_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Saves a project.");};
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
