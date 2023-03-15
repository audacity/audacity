/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   File License: wxWidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file PreferenceCommands.cpp
\brief Contains definitions for the GetPreferenceCommand and
SetPreferenceCommand classes

*//*******************************************************************/


#include "PreferenceCommands.h"

#include "CommandDispatch.h"
#include "CommandManager.h"
#include "../CommonCommandFlags.h"
#include "LoadCommands.h"
#include "Prefs.h"
#include "SettingsVisitor.h"
#include "ShuttleGui.h"
#include "../commands/CommandContext.h"
#include "../prefs/PrefsDialog.h"

const ComponentInterfaceSymbol GetPreferenceCommand::Symbol
{ XO("Get Preference") };

namespace{ BuiltinCommandsModule::Registration< GetPreferenceCommand > reg; }

template<bool Const>
bool GetPreferenceCommand::VisitSettings( SettingsVisitorBase<Const> & S ){
   S.Define( mName, wxT("Name"),   wxString{} );
   return true;
}

bool GetPreferenceCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool GetPreferenceCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

void GetPreferenceCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("Name:"),mName);
   }
   S.EndMultiColumn();
}


bool GetPreferenceCommand::Apply(const CommandContext & context)
{
   wxString prefValue;
   if (!gPrefs->Read(mName, &prefValue))
      return false;

   context.Status(prefValue);
   return true;
}

const ComponentInterfaceSymbol SetPreferenceCommand::Symbol
{ XO("Set Preference") };

namespace{ BuiltinCommandsModule::Registration< SetPreferenceCommand > reg2; }

template<bool Const>
bool SetPreferenceCommand::VisitSettings( SettingsVisitorBase<Const> & S ){
   S.Define(    mName,   wxT("Name"),    wxString{} );
   S.Define(   mValue,   wxT("Value"),   wxString{} );
   S.Define( mbReload,   wxT("Reload"),  false );
   return true;
}

bool SetPreferenceCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool SetPreferenceCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

void SetPreferenceCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("Name:"),mName);
      S.TieTextBox(XXO("Value:"),mValue);
      S.TieCheckBox(XXO("Reload"),mbReload);
   }
   S.EndMultiColumn();
}

bool SetPreferenceCommand::Apply(const CommandContext & context)
{
   bool bOK = gPrefs->Write(mName, mValue) && gPrefs->Flush();
   if( bOK && mbReload ){
      auto &project = context.project;
      DoReloadPreferences( project );
   }
   return bOK;
}

namespace {
using namespace MenuTable;

// Register menu items

AttachedItem sAttachment1{
   wxT("Optional/Extra/Part2/Scriptables1"),
   Items( wxT(""),
      // Note that the PLUGIN_SYMBOL must have a space between words,
      // whereas the short-form used here must not.
      // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
      // you would have to use "CompareAudio" here.)
      Command( wxT("GetPreference"), XXO("Get Preference..."),
         CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag() ),
      Command( wxT("SetPreference"), XXO("Set Preference..."),
         CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag() )
   )
};
}
