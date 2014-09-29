/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file CommandSignature.cpp
\brief Definitions for CommandSignature class

*//*******************************************************************/

#include "CommandMisc.h"
#include "CommandSignature.h"
#include "Validators.h"

CommandSignature::~CommandSignature()
{
   // Delete the validators
   ValidatorMap::iterator iter;
   for (iter = mValidators.begin(); iter != mValidators.end(); ++iter)
   {
      delete iter->second;
   }
}

void CommandSignature::AddParameter(const wxString &name,
      const wxVariant &dft,
      Validator *valid)
{
   wxASSERT_MSG(valid->Validate(dft),
         wxT("Invalid command signature: the default value of '")
         + dft.MakeString()
         + wxT("' for the '")
         + name
         + wxT("' parameter doesn't satisfy the provided validator.")
         + wxT(" It should be ")
         + valid->GetDescription()
         + wxT("."));
   mDefaults.insert(std::pair<wxString, wxVariant>(name, dft));
   mValidators.insert(std::pair<wxString, Validator*>(name, valid));
}

ParamValueMap CommandSignature::GetDefaults() const
{
   return mDefaults;
}

Validator &CommandSignature::GetValidator(const wxString &paramName)
{
   wxASSERT(mValidators.find(paramName) != mValidators.end());
   return *mValidators[paramName];
}
