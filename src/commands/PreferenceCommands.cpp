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

#include "LoadCommands.h"
#include "Prefs.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../commands/CommandContext.h"
#include "../prefs/PrefsDialog.h"

const ComponentInterfaceSymbol GetPreferenceCommand::Symbol
{ XO("Get Preference") };

namespace{ BuiltinCommandsModule::Registration< GetPreferenceCommand > reg; }

bool GetPreferenceCommand::DefineParams( ShuttleParams & S ){
   S.Define( mName, wxT("Name"),   wxT("") );
   return true;
}

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

bool SetPreferenceCommand::DefineParams( ShuttleParams & S ){
   S.Define(    mName,   wxT("Name"),    wxT("") );
   S.Define(   mValue,   wxT("Value"),   wxT("") );
   S.Define( mbReload,   wxT("Reload"),  false );
   return true;
}

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

