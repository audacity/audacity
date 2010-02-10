/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file Command.cpp
\brief Contains definitions for Command, DecoratedCommand,
ApplyAndSendResponse, and CommandImplementation classes

*//*******************************************************************/

#include "Command.h"
#include <map>
#include <wx/string.h>
#include <wx/variant.h>
#include <wx/arrstr.h>

#include "Validators.h"
#include "CommandType.h"
#include "CommandMisc.h"
#include "CommandBuilder.h"
#include "CommandTargets.h"
#include "CommandDirectory.h"

bool Command::SetParameter(const wxString &paramName,
                           const wxVariant &paramValue)
{
   wxASSERT_MSG(false, wxT("Tried to set parameter for command which doesn't support parameters!"));
   return false;
}

void DecoratedCommand::Progress(double completed)
{
   mCommand->Progress(completed);
}

void DecoratedCommand::Status(wxString message)
{
   mCommand->Status(message);
}

void DecoratedCommand::Error(wxString message)
{
   mCommand->Error(message);
}

DecoratedCommand::~DecoratedCommand()
{
   delete mCommand;
}

wxString DecoratedCommand::GetName()
{
   return mCommand->GetName();
}

CommandSignature &DecoratedCommand::GetSignature()
{
   return mCommand->GetSignature();
}

bool DecoratedCommand::SetParameter(const wxString &paramName,
                                    const wxVariant &paramValue)
{
   return mCommand->SetParameter(paramName, paramValue);
}

bool ApplyAndSendResponse::Apply(CommandExecutionContext context)
{
   bool result = mCommand->Apply(context);
   wxString response = GetName();
   // These three strings are deliberately not localised.
   // They are used in script responses and always happen in English.
   response += wxT(" finished: ");
   if (result)
   {
      response += wxT("OK");
   } else
   {
      response += wxT("Failed!");
   }
   Status(response);
   return result;
}

CommandImplementation::CommandImplementation(CommandType &type,
      CommandOutputTarget *output)
: mType(type),
   mParams(type.GetSignature().GetDefaults()),
   mOutput(output)
{
   wxASSERT(output != NULL);
}

CommandImplementation::~CommandImplementation()
{
   delete mOutput;
}

void CommandImplementation::TypeCheck(const wxString &typeName,
                                      const wxString &paramName,
                                      const wxVariant &param)
{
   wxASSERT_MSG(param.IsType(typeName),
                GetName()
                + wxT("command tried to get '")
                + paramName
                + wxT("' parameter as a ")
                + typeName
                + wxT(", but that wasn't enforced by the command signature."));
}

void CommandImplementation::CheckParam(const wxString &paramName)
{
   wxASSERT_MSG(mParams.find(paramName) != mParams.end(),
                GetName()
                + wxT("command tried to get '")
                + paramName
                + wxT("' parameter, but that parameter doesn't exist in the command signature!"));
}

bool CommandImplementation::GetBool(const wxString &paramName)
{
   CheckParam(paramName);
   const wxVariant &v = mParams[paramName];
   TypeCheck(wxT("bool"), paramName, v);
   return v.GetBool();
}

long CommandImplementation::GetLong(const wxString &paramName)
{
   CheckParam(paramName);
   const wxVariant &v = mParams[paramName];
   TypeCheck(wxT("double"), paramName, v);
   return (long)v.GetDouble();
}

double CommandImplementation::GetDouble(const wxString &paramName)
{
   CheckParam(paramName);
   const wxVariant &v = mParams[paramName];
   TypeCheck(wxT("double"), paramName, v);
   return v.GetDouble();
}

wxString CommandImplementation::GetString(const wxString &paramName)
{
   CheckParam(paramName);
   const wxVariant &v = mParams[paramName];
   TypeCheck(wxT("string"), paramName, v);
   return v.GetString();
}

// Convenience methods for passing messages to the output target
void CommandImplementation::Progress(double completed)
{
   mOutput->Progress(completed);
}

void CommandImplementation::Status(wxString status)
{
   mOutput->Status(status);
}

void CommandImplementation::Error(wxString message)
{
   mOutput->Error(message);
}

/// Get the name of the command
wxString CommandImplementation::GetName()
{
   return mType.GetName();
}

/// Get the signature of the command
CommandSignature &CommandImplementation::GetSignature()
{
   return mType.GetSignature();
}

bool CommandImplementation::SetParameter(const wxString &paramName, const wxVariant &paramValue)
{
   wxASSERT(!paramValue.IsType(wxT("null")));

   ParamValueMap::iterator iter = mParams.find(paramName);
   if (iter == mParams.end())
   {
      Error(paramName + wxT(" is not a parameter accepted by ") + GetName());
      return false;
   }

   Validator &validator = mType.GetSignature().GetValidator(iter->first);
   if (!validator.Validate(paramValue))
   {
      Error(wxT("Invalid value for parameter '")
            + paramName + wxT("': should be ")
            + validator.GetDescription());
      return false;
   }
   mParams[paramName] = validator.GetConverted();

   // (debug)
   // Status(wxT("Set parameter ") + paramName + wxT(" to type ") + mParams[paramName].GetType() + wxT(", value ") + mParams[paramName].MakeString());

   return true;
}

bool CommandImplementation::Apply(CommandExecutionContext context)
{
   return true;
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
