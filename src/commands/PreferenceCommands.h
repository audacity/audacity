/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2018 Audacity Team.
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

#ifndef __PREFERENCE_COMMANDS__
#define __PREFERENCE_COMMANDS__

#include "Command.h"
#include "CommandType.h"

// GetPreference

#define GET_PREFERENCE_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Get Preference") }
#define SET_PREFERENCE_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Set Preference") }

class GetPreferenceCommand final : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return GET_PREFERENCE_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Gets the value of a single preference.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#get_preference");};

   wxString mName;
};

// SetPreference

class SetPreferenceCommand final : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return SET_PREFERENCE_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Sets the value of a single preference.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#set_preference");};

   wxString mName;
   wxString mValue;
   bool mbReload;
   bool bHasReload;
};

#endif /* End of include guard: __PREFERENCE_COMMANDS__ */
