/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file CommandType.cpp
\brief Contains definitions for CommandType class

\class CommandType
\brief Base class for containing data common to all commands of a given type.
Also acts as a factory.

*//*******************************************************************/

#include "CommandMisc.h"
#include "CommandType.h"
#include "CommandSignature.h"
#include <wx/string.h>

CommandType::CommandType()
   : mName(NULL), mSignature(NULL)
{ }

CommandType::~CommandType()
{
   if (mName != NULL)
   {
      delete mName;
   }
   if (mSignature != NULL)
   {
      delete mSignature;
   }
}

wxString CommandType::GetName()
{
   if (mName == NULL)
   {
      mName = new wxString(BuildName());
   }
   return *mName;
}

CommandSignature &CommandType::GetSignature()
{
   if (mSignature == NULL)
   {
      mSignature = new CommandSignature();
      BuildSignature(*mSignature);
   }
   return *mSignature;
}

wxString CommandType::Describe()
{
   wxString desc = GetName() + wxT("\nParameters:");
   GetSignature();
   ParamValueMap::iterator iter;
   ParamValueMap defaults = mSignature->GetDefaults();
   for (iter = defaults.begin(); iter != defaults.end(); ++iter)
   {
      desc += wxT("\n") + iter->first + wxT(": ")
         + mSignature->GetValidator(iter->first).GetDescription()
         + wxT(" (default: ")
         + iter->second.MakeString() + wxT(")");
   }
   return desc;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: TBD
