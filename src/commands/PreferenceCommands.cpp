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
   auto prefNameValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("PrefName"), wxT(""), std::move(prefNameValidator));
}

CommandHolder GetPreferenceCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<GetPreferenceCommand>(*this, std::move(target));
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
   auto prefNameValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("PrefName"), wxT(""), std::move(prefNameValidator));
   auto prefValueValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("PrefValue"), wxT(""), std::move(prefValueValidator));
}

CommandHolder SetPreferenceCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<SetPreferenceCommand>(*this, std::move(target));
}

bool SetPreferenceCommand::Apply(CommandExecutionContext WXUNUSED(context))
{
   wxString prefName = GetString(wxT("PrefName"));
   wxString prefValue = GetString(wxT("PrefValue"));
   return (gPrefs->Write(prefName, prefValue) && gPrefs->Flush());
}

SetPreferenceCommand::~SetPreferenceCommand()
{ }
