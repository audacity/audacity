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
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class ImportCommand final : public CommandImplementation
{
public:
   ImportCommand(CommandType &type,
                    std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }

   virtual ~ImportCommand();
   bool Apply(CommandExecutionContext context) override;
};

// Export

class ExportCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class ExportCommand final : public CommandImplementation
{
public:
   ExportCommand(CommandType &type,
                    std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }

   virtual ~ExportCommand();
   bool Apply(CommandExecutionContext context) override;
};
