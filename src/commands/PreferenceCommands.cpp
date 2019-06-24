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

#include "../Audacity.h"
#include "PreferenceCommands.h"

#include "../Prefs.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../commands/CommandContext.h"
#include "../prefs/PrefsDialog.h"

const ComponentInterfaceSymbol GetPreferenceCommand::Symbol
{ XO("Get Preference") };

bool GetPreferenceCommand::DefineParams( ShuttleParams & S ){
   S.Define( mName, wxT("Name"),   wxT("") );
   return true;
}

void GetPreferenceCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XO("Name:"),mName);
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
      S.TieTextBox(XO("Name:"),mName);
      S.TieTextBox(XO("Value:"),mValue);
      S.TieCheckBox(XO("Reload:"),mbReload);
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

