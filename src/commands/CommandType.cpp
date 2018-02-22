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

#include "../Audacity.h"
#include "CommandType.h"
#include "CommandMisc.h"
#include "CommandSignature.h"
#include <wx/string.h>

CommandType::CommandType()
   : mName{}, mSignature{}
{ }

CommandType::~CommandType()
{
}

const wxString &CommandType::GetName()
{
   if (mName.empty())
      mName = BuildName();
   return mName;
}

CommandSignature &CommandType::GetSignature()
{
   if (!mSignature)
   {
      mSignature.create();
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
