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

#include "../Menus.h"
#include "../Prefs.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../commands/CommandContext.h"

bool GetPreferenceCommand::DefineParams( ShuttleParams & S ){
   S.Define( mName, wxT("Name"),   wxT("") );
   return true;
}

void GetPreferenceCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(_("Name:"),mName);
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
      S.TieTextBox(_("Name:"),mName);
      S.TieTextBox(_("Value:"),mValue);
      S.TieCheckBox(_("Reload:"),mbReload);
   }
   S.EndMultiColumn();
}

bool SetPreferenceCommand::Apply(const CommandContext & context)
{
   bool bOK = gPrefs->Write(mName, mValue) && gPrefs->Flush();
   if( bOK && mbReload ){
      auto &project = context.project;
      EditActions::DoReloadPreferences( project );
   }
   return bOK;
}

