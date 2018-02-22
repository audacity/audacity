/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2009 Audacity Team.
   File License: wxwidgets

   PreferenceCommands.h
   Dan Horgan

******************************************************************//**

\class SetPreferenceCommand
\brief Command for setting a preference to a given value

\class GetPreferenceCommand
\brief Command for getting the value of a preference

*//*******************************************************************/

#ifndef __PREFERENCECOMMANDS__
#define __PREFERENCECOMMANDS__

#include "Command.h"
#include "CommandType.h"

// GetPreference

class GetPreferenceCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class GetPreferenceCommand final : public CommandImplementation
{
public:
   GetPreferenceCommand(CommandType &type,
                    std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }

   virtual ~GetPreferenceCommand();
   bool Apply(CommandExecutionContext context) override;
};

// SetPreference

class SetPreferenceCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class SetPreferenceCommand final : public CommandImplementation
{
public:
   SetPreferenceCommand(CommandType &type,
                    std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }

   virtual ~SetPreferenceCommand();
   bool Apply(CommandExecutionContext context) override;
};

#endif /* End of include guard: __PREFERENCECOMMANDS__ */
