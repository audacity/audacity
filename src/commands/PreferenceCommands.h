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
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class GetPreferenceCommand final : public CommandImplementation
{
public:
   GetPreferenceCommand(CommandType &type,
                    CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }

   virtual ~GetPreferenceCommand();
   virtual bool Apply(CommandExecutionContext context);
};

// SetPreference

class SetPreferenceCommandType final : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class SetPreferenceCommand final : public CommandImplementation
{
public:
   SetPreferenceCommand(CommandType &type,
                    CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }

   virtual ~SetPreferenceCommand();
   virtual bool Apply(CommandExecutionContext context);
};

#endif /* End of include guard: __PREFERENCECOMMANDS__ */
