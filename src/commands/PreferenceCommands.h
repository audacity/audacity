/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2009 Audacity Team.
   File License: wxwidgets

   PreferenceCommands.h
   Dan Horgan
   James Crook

******************************************************************//**

\class GetPreferenceCommand
\brief Command for getting the value of a preference

\class SetPreferenceCommand
\brief Command for setting a preference to a given value

*//*******************************************************************/

#ifndef __PREFERENCECOMMANDS__
#define __PREFERENCECOMMANDS__

#include "Command.h"
#include "CommandType.h"

// GetPreference

#define GET_PREFERENCE_PLUGIN_SYMBOL XO("Get Preference")
#define SET_PREFERENCE_PLUGIN_SYMBOL XO("Set Preference")

class GetPreferenceCommand final : public AudacityCommand
{
public:
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return GET_PREFERENCE_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Gets the value of a single preference.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Preferences");};

   wxString mName;
};

// SetPreference

class SetPreferenceCommand final : public AudacityCommand
{
public:
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return SET_PREFERENCE_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Sets the value of a single preference.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Preferences");};

   wxString mName;
   wxString mValue;
   bool mbReload;
};

#endif /* End of include guard: __PREFERENCECOMMANDS__ */
