/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file GetAllMenuCommands.cpp
\brief Contains definitions for GetAllMenuCommands class.

*//*******************************************************************/

#include "GetAllMenuCommands.h"
#include "../Project.h"
#include "CommandManager.h"

wxString GetAllMenuCommandsType::BuildName()
{
   return wxT("GetAllMenuCommands");
}

void GetAllMenuCommandsType::BuildSignature(CommandSignature &signature)
{
   BoolValidator *showStatusValidator = new BoolValidator();
   signature.AddParameter(wxT("ShowStatus"), 0, showStatusValidator);
}

Command *GetAllMenuCommandsType::Create(CommandOutputTarget *target)
{
   return new GetAllMenuCommands(*this, target);
}

bool GetAllMenuCommands::Apply(CommandExecutionContext context)
{
   bool showStatus = GetBool(wxT("ShowStatus"));
   wxArrayString names;
   CommandManager *cmdManager = context.proj->GetCommandManager();
   cmdManager->GetAllCommandNames(names, false);
   wxArrayString::iterator iter;
   for (iter = names.begin(); iter != names.end(); ++iter)
   {
      wxString name = *iter;
      wxString out = name;
      if (showStatus)
      {
         out += wxT("\t");
         out += cmdManager->GetEnabled(name) ? wxT("Enabled") : wxT("Disabled");
      }
      Status(out);
   }
   return true;
}
