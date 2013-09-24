/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file PreferenceCommands.cpp
\brief Contains definitions for the GetPreferenceCommand and
SetPreferenceCommand classes

*//*******************************************************************/

#include "PreferenceCommands.h"
#include "../Prefs.h"

// GetPreference

wxString GetPreferenceCommandType::BuildName()
{
   return wxT("GetPreference");
}

void GetPreferenceCommandType::BuildSignature(CommandSignature &signature)
{
   Validator *prefNameValidator(new Validator());
   signature.AddParameter(wxT("PrefName"), wxT(""), prefNameValidator);
}

Command *GetPreferenceCommandType::Create(CommandOutputTarget *target)
{
   return new GetPreferenceCommand(*this, target);
}

bool GetPreferenceCommand::Apply(CommandExecutionContext WXUNUSED(context))
{
   wxString prefName = GetString(wxT("PrefName"));
   wxString prefValue;
   if (!gPrefs->Read(prefName, &prefValue))
   {
      return false;
   }
   Status(prefValue);
   return true;
}

GetPreferenceCommand::~GetPreferenceCommand()
{ }

// SetPreference

wxString SetPreferenceCommandType::BuildName()
{
   return wxT("SetPreference");
}

void SetPreferenceCommandType::BuildSignature(CommandSignature &signature)
{
   Validator *prefNameValidator(new Validator());
   signature.AddParameter(wxT("PrefName"), wxT(""), prefNameValidator);
   Validator *prefValueValidator(new Validator());
   signature.AddParameter(wxT("PrefValue"), wxT(""), prefValueValidator);
}

Command *SetPreferenceCommandType::Create(CommandOutputTarget *target)
{
   return new SetPreferenceCommand(*this, target);
}

bool SetPreferenceCommand::Apply(CommandExecutionContext WXUNUSED(context))
{
   wxString prefName = GetString(wxT("PrefName"));
   wxString prefValue = GetString(wxT("PrefValue"));
   return (gPrefs->Write(prefName, prefValue) && gPrefs->Flush());
}

SetPreferenceCommand::~SetPreferenceCommand()
{ }
