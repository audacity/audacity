/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2018 Audacity Team.
   File License: wxwidgets

   ImportExportCommands.h
   Dan Horgan
   James Crook

******************************************************************//**

\class ImportCommand
\brief Command for importing audio

\class ExportCommand
\brief Command for exporting audio

*//*******************************************************************/

#include "Command.h"
#include "CommandType.h"

// Import

#define IMPORT_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Import2") }

class ImportCommand : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return IMPORT_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Imports from a file.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#import");};
public:
   wxString mFileName;
};

#define EXPORT_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Export2") }

class ExportCommand : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return EXPORT_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Exports to a file.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#export");};
public:
   wxString mFileName;
   int mnChannels;
};
