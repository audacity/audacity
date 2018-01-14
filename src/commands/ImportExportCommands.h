/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2009 Audacity Team.
   File License: wxwidgets

   ImportExportCommands.h
   Dan Horgan

******************************************************************//**

\class ImportCommand
\brief Command for importing audio

\class ExportCommand
\brief Command for exporting audio

*//*******************************************************************/

#include "Command.h"
#include "CommandType.h"

// Import

#define IMPORT_PLUGIN_SYMBOL XO("Import")

class ImportCommand : public AudacityCommand
{
public:
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return IMPORT_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Imports from a file.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Import");};
public:
   wxString mFileName;
};

#define EXPORT_PLUGIN_SYMBOL XO("Export")

class ExportCommand : public AudacityCommand
{
public:
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return EXPORT_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Exports to a file.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Export");};
public:
   wxString mFileName;
   int mnChannels;
};
