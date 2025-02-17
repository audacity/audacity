/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file CommandType.cpp
\brief Contains definitions for CommandType class

\class OldStyleCommandType
\brief Base class for containing data common to all commands of a given type.
Also acts as a factory.

*//*******************************************************************/

#include "CommandType.h"

OldStyleCommandType::OldStyleCommandType()
    : mSignature{}
{
}

OldStyleCommandType::~OldStyleCommandType()
{
}

ComponentInterfaceSymbol OldStyleCommandType::GetSymbol() const
{
    return BuildName();
}

CommandSignature& OldStyleCommandType::GetSignature()
{
    if (!mSignature) {
        mSignature.emplace();
        BuildSignature(*mSignature);
    }
    return *mSignature;
}

wxString OldStyleCommandType::Describe()
{
    // PRL: Is this intended for end-user visibility or just debugging?  It did not
    // use _(""), so I assume it is meant to use internal strings
    wxString desc = GetSymbol().Internal() + wxT("\nParameters:");
    GetSignature();
    ParamValueMap::iterator iter;
    ParamValueMap defaults = mSignature->GetDefaults();
    for (iter = defaults.begin(); iter != defaults.end(); ++iter) {
        desc += wxT("\n") + iter->first + wxT(": ")
                + mSignature->GetValidator(iter->first).GetDescription()
                + wxT(" (default: ")
                + iter->second.MakeString() + wxT(")");
    }
    return desc;
}
