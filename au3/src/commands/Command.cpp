/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file Command.cpp
\brief Contains definitions for Command, DecoratedCommand,
ApplyAndSendResponse, and CommandImplementation classes.  These are
remnants of Dan Horgans external scripting commands.  We now use
AudacityCommand and a shuttle system.  This allows commands to be used
from within macros too, to have settings dialogs, using ShuttleGui and
without need for validators.

Here's the doxygen for the still-remaining going-away classes.


\class BatchEvalCommandType
\brief The <something>CommandType classes are classes which are going
away.  They all provided a BuildSignature (what parameters they accept)
and Name, but that is now replaced by the AudacityCommand interface.

We in effect merge the <something>CommandType classes into the
<something>Command classes.

\class MessageCommandType
\brief The <something>CommandType classes are classes which are going
away.  They all provided a BuildSignature (what parameters they accept)
and Name, but that is now replaced by the AudacityCommand interface.

We in effect merge the <something>CommandType classes into the
<something>Command classes.

\class BatchEvalCommand
\brief Command to make processing of macros available to scripting.  It can either
make a one command macro, or invoke an existing macro.  It will become redundant
when menu commands are integrated into scripting.

\class HelpCommand
\brief Command to get help about other commands.

\class MessageCommand
\brief Command to get a message response.  Used for testing, and used internally
to create messages for forwarding.

******************************************************************//**

\class OldStyleCommand
\brief Abstract base class for command interface.  This is the version
created by Dan Horgan.  It was previously a factory for other command classes.
It created a separation between the type of a command and the command itself,
which is being removed.  These Commands were managed by CommandDirectory.

\class OldStyleCommandPointer
\brief OldStyleCommandPointer is a unique_ptr to an OldStyleCommand.

\class DecoratedCommand
\brief DecoratedCommand is a decorator for command.  It forwards functions
to the mCommand it holds.

\class ApplyAndSendResponse
\brief ApplyAndSendResponse is a DecoratoredCommand that performs the given
command and then outputs a status message according to the result.  It
manages a CommandContext which is passed into its mCommand, so that result
messages are routed back to the right place.

\class CommandImplementation,
\brief is derived from OldStyleCommand.  It validates and
applies the command.  CommandImplementation::Apply() is overloaded in
classes derived from it.

*//*******************************************************************/

#include "Command.h"

#include <map>
#include <wx/log.h>
#include <wx/variant.h>
#include <wx/arrstr.h>

#include "CommandTargets.h"
#include "CommandDirectory.h"

#include "CommandContext.h"

#include "AudacityException.h"

bool OldStyleCommand::SetParameter(const wxString& WXUNUSED(paramName),
                                   const wxVariant& WXUNUSED(paramValue))
{
    wxASSERT_MSG(false, wxT("Tried to set parameter for command which doesn't support parameters!"));
    return false;
}

DecoratedCommand::~DecoratedCommand()
{
}

ComponentInterfaceSymbol DecoratedCommand::GetSymbol()
{
    return mCommand->GetSymbol();
}

CommandSignature& DecoratedCommand::GetSignature()
{
    return mCommand->GetSignature();
}

bool DecoratedCommand::SetParameter(const wxString& paramName,
                                    const wxVariant& paramValue)
{
    return mCommand->SetParameter(paramName, paramValue);
}

ApplyAndSendResponse::ApplyAndSendResponse(
    const OldStyleCommandPointer& cmd, std::unique_ptr<CommandOutputTargets>& target)
    : DecoratedCommand(cmd),
    mCtx(std::make_unique<CommandContext>(cmd->mProject, std::move(target)))
{
}

bool ApplyAndSendResponse::Apply(const CommandContext& WXUNUSED(context))
{
    wxLogMessage("Context was passed in, but was ignored.  ApplyAndSendResponse has its own one");
    return Apply();
}

bool ApplyAndSendResponse::Apply()
{
    // ApplyAndSendResponse IS a command.
    // It also HOLDS a command.

    // Mostly its functions forward to the recipient.
    // However it uses its OWN context, not the one of
    // the command it holds.
    auto result = GuardedCall<bool>(
        [&] {
        bool bResult = mCommand->Apply(*(mCtx.get()));
        return bResult;
    }
        );
    wxString response = wxT("\n");

    // PRL: it's all right to send untranslated strings to this channel
    // I don't see _("") used with literal strings.
    response += GetSymbol().Internal();

    // These three strings are deliberately not localised.
    // They are used in script responses and always happen in English.
    response += wxT(" finished: ");
    if (result) {
        response += wxT("OK");
    } else {
        response += wxT("Failed!");
    }
    mCtx->Status(response, true);
    return result;
}

CommandImplementation::CommandImplementation(
    AudacityProject& project, OldStyleCommandType& type)
    :  OldStyleCommand{project},
    mType(type),
    mParams(type.GetSignature().GetDefaults()),
    mSetParams()
{
}

CommandImplementation::~CommandImplementation()
{
}

void CommandImplementation::TypeCheck(const wxString& typeName,
                                      const wxString& paramName,
                                      const wxVariant& param)
{
    // this macro is empty if wxWidgets is not compiled in debug mode
    wxASSERT_MSG(param.IsType(typeName),
                 GetSymbol().Internal()
                 + wxT("command tried to get '")
                 + paramName
                 + wxT("' parameter as a ")
                 + typeName
                 + wxT(", but that wasn't enforced by the command signature."));
}

void CommandImplementation::CheckParam(const wxString& paramName)
{
    // this macro is empty if wxWidgets is not compiled in debug mode
    wxASSERT_MSG(mParams.find(paramName) != mParams.end(),
                 GetSymbol().Internal()
                 + wxT("command tried to get '")
                 + paramName
                 + wxT("' parameter, but that parameter doesn't exist in the command signature!"));
}

bool CommandImplementation::HasParam(const wxString& paramName)
{
    // Test for not even in map...
    if (mParams.count(paramName) < 1) {
        return false;
    }
    return mSetParams[paramName];
}

bool CommandImplementation::GetBool(const wxString& paramName)
{
    CheckParam(paramName);
    const wxVariant& v = mParams[paramName];
    TypeCheck(wxT("bool"), paramName, v);
    return v.GetBool();
}

long CommandImplementation::GetLong(const wxString& paramName)
{
    CheckParam(paramName);
    const wxVariant& v = mParams[paramName];
    TypeCheck(wxT("double"), paramName, v);
    return (long)v.GetDouble();
}

double CommandImplementation::GetDouble(const wxString& paramName)
{
    CheckParam(paramName);
    const wxVariant& v = mParams[paramName];
    TypeCheck(wxT("double"), paramName, v);
    return v.GetDouble();
}

wxString CommandImplementation::GetString(const wxString& paramName)
{
    CheckParam(paramName);
    const wxVariant& v = mParams[paramName];
    TypeCheck(wxT("string"), paramName, v);
    return v.GetString();
}

/// Get the name of the command
ComponentInterfaceSymbol CommandImplementation::GetSymbol()
{
    return mType.GetSymbol();
}

/// Get the signature of the command
CommandSignature& CommandImplementation::GetSignature()
{
    return mType.GetSignature();
}

bool CommandImplementation::SetParameter(const wxString& paramName, const wxVariant& paramValue)
{
    wxASSERT(!paramValue.IsType(wxT("null")));
    CommandContext context(mProject);
    ParamValueMap::iterator iter = mParams.find(paramName);
    if (iter == mParams.end()) {
        // Translated format, but untranslated command name substituted into it?
        // Perhaps these formats that don't need translating.
        context.Error(wxString::Format(
                          _("%s is not a parameter accepted by %s"),
                          paramName, GetSymbol().Internal()));
        // neglect translation for scripting ??
        return false;
    }

    Validator& validator = mType.GetSignature().GetValidator(iter->first);
    if (!validator.Validate(paramValue)) {
        context.Error(wxString::Format(
                          _("Invalid value for parameter '%s': should be %s"),
                          paramName, validator.GetDescription()));
        return false;
    }
    mParams[paramName] = validator.GetConverted();
    mSetParams[ paramName ] = true;

    // (debug)
    // context.Status(wxT("Set parameter ") + paramName + wxT(" to type ") + mParams[paramName].GetType() + wxT(", value ") + mParams[paramName].MakeString());

    return true;
}

bool CommandImplementation::Apply(const CommandContext& WXUNUSED(context))
{
    return true;
}
