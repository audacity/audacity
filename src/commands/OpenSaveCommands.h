/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2009 Audacity Team.
   File License: wxwidgets

   OpenSaveCommands.h
   Stephen Parry

******************************************************************//**

\class OpenProjectCommand
\brief Command for opening an Audacity project

\class SaveProjectCommand
\brief Command for saving an Audacity project

*//*******************************************************************/

#include "Command.h"
#include "CommandType.h"

// Open

class OpenProjectCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class OpenProjectCommand final : public CommandImplementation
{
public:
   OpenProjectCommand(CommandType &type,
                    std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }

   virtual ~OpenProjectCommand();
   bool Apply(CommandExecutionContext context) override;
};

// Save

class SaveProjectCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class SaveProjectCommand final : public CommandImplementation
{
public:
   SaveProjectCommand(CommandType &type,
                    std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }

   virtual ~SaveProjectCommand();
   bool Apply(CommandExecutionContext context) override;
};
