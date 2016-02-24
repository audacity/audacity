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

class ImportCommandType final : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class ImportCommand final : public CommandImplementation
{
public:
   ImportCommand(CommandType &type,
                    CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }

   virtual ~ImportCommand();
   virtual bool Apply(CommandExecutionContext context);
};

// Export

class ExportCommandType final : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class ExportCommand final : public CommandImplementation
{
public:
   ExportCommand(CommandType &type,
                    CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }

   virtual ~ExportCommand();
   virtual bool Apply(CommandExecutionContext context);
};
