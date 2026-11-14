/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandBuilder.cpp
\brief Contains definitions for CommandBuilder class.

\class CommandBuilder
\brief A type of factory for Commands of various sorts.

CommandBuilder has the task of deciding what command is meant by a given
command string, and producing a suitable command object. For now, it doesn't
actually do any processing - it just passes everything on to the BatchCommand
system by constructing BatchCommandEval objects.

*//*******************************************************************/

#include "CommandBuilder.h"

#include "CommandDirectory.h"
#include "Command.h"
#include "CommandContext.h"
#include "CommandTargets.h"
#include "SettingsVisitor.h"

CommandBuilder::CommandBuilder(
    AudacityProject& project, const wxString& cmdString)
    : mValid(false)
{
    BuildCommand(project, cmdString);
}

CommandBuilder::CommandBuilder(AudacityProject& project,
                               const wxString& cmdName, const wxString& params)
    : mValid(false)
{
    BuildCommand(project, cmdName, params);
}

CommandBuilder::~CommandBuilder()
{
}

bool CommandBuilder::WasValid()
{
    return mValid;
}

OldStyleCommandPointer CommandBuilder::GetCommand()
{
    wxASSERT(mValid);
    wxASSERT(mCommand);
    auto result = mCommand;
    mCommand.reset();
    return result;
}

wxString CommandBuilder::GetResponse()
{
    if (!mValid && !mError.empty()) {
        return mError + wxT("\n");
    }
    return mResponse->GetResponse() + wxT("\n");
}

void CommandBuilder::Failure(const wxString& msg)
{
    mError = msg;
    mValid = false;
}

void CommandBuilder::Success(const OldStyleCommandPointer& cmd)
{
    mCommand = cmd;
    mValid = true;
}

namespace {
// This class was formerly in Shuttle.cpp and inherited Shuttle but the
// polymorphism wasn't really needed in the sole use of the class in this file
struct ShuttleCli final // : public Shuttle
{
    wxString mValueString;
    bool TransferString(const wxString& Name, wxString& strValue);
    wxString mParams;

//   virtual ~ShuttleCli() {}

    bool ExchangeWithMaster(const wxString& Name);
};

// uses values of the form
// param1=value1 param2=value2
bool ShuttleCli::ExchangeWithMaster(const wxString& Name)
{
    int i;
    mParams = L" " + mParams;
    i = mParams.Find(L" " + Name + L"=");
    if (i >= 0) {
        int j = i + 2 + Name.Length();
        wxString terminator = L' ';
        if (mParams.GetChar(j) == L'"') { //Strings are surrounded by quotes
            terminator = L'"';
            j++;
        } else if (mParams.GetChar(j) == L'\'') { // or by single quotes.
            terminator = L'\'';
            j++;
        }
        i = j;
        while (j < (int)mParams.Length() && mParams.GetChar(j) != terminator) {
            j++;
        }
        mValueString = mParams.Mid(i, j - i);
        return true;
    }
    return false;
}

bool ShuttleCli::TransferString(const wxString& Name, wxString& strValue)
{
    if (ExchangeWithMaster(Name)) {
        strValue = mValueString;
        return true;
    } else {
        return false;
    }
}
}

void CommandBuilder::BuildCommand(AudacityProject& project,
                                  const wxString& cmdName,
                                  const wxString& cmdParamsArg)
{
    // Stage 1: create a Command object of the right type

    mResponse = std::make_shared< ResponseTarget >();
    auto output
        = std::make_unique<CommandOutputTargets>(std::make_unique<NullProgressTarget>(),
                                                 mResponse,
                                                 mResponse);

#ifdef OLD_BATCH_SYSTEM
    OldStyleCommandType* factory = CommandDirectory::Get()->LookUp(cmdName);

    if (factory == NULL) {
        // Fall back to hoping the Batch Command system can handle it
#endif
    OldStyleCommandType* type = CommandDirectory::Get()->LookUp(wxT("BatchCommand"));
    wxASSERT(type != NULL);
    mCommand = type->Create(project, nullptr);
    mCommand->SetParameter(wxT("CommandName"), cmdName);
    mCommand->SetParameter(wxT("ParamString"), cmdParamsArg);
    auto aCommand = std::make_shared<ApplyAndSendResponse>(mCommand, output);
    Success(aCommand);
    return;
#ifdef OLD_BATCH_SYSTEM
}

CommandSignature& signature = factory->GetSignature();
mCommand = factory->Create(nullptr);
//mCommand->SetOutput( std::move(output) );
// Stage 2: set the parameters

ShuttleCli shuttle;
shuttle.mParams = cmdParamsArg;

ParamValueMap::const_iterator iter;
ParamValueMap params = signature.GetDefaults();

// Iterate through the parameters defined by the command
for (iter = params.begin(); iter != params.end(); ++iter) {
    wxString paramString;
    // IF there is a match in the args actually used
    if (shuttle.TransferString(iter->first, paramString, wxT(""))) {
        // Then set that parameter.
        if (!mCommand->SetParameter(iter->first, paramString)) {
            Failure();
            return;
        }
    }
}

// Check for unrecognised parameters

wxString cmdParams(cmdParamsArg);

while (!cmdParams.empty())
{
    cmdParams.Trim(true);
    cmdParams.Trim(false);
    int splitAt = cmdParams.Find(wxT('='));
    if (splitAt < 0 && !cmdParams.empty()) {
        Failure(wxT("Parameter string is missing '='"));
        return;
    }
    wxString paramName = cmdParams.Left(splitAt);
    if (params.find(paramName) == params.end()) {
        Failure(wxT("Unrecognized parameter: '") + paramName + wxT("'"));
        return;
    }
    // Handling of quoted strings is quite limited.
    // You start and end with a " or a '.
    // There is no escaping in the string.
    cmdParams = cmdParams.Mid(splitAt + 1);
    if (cmdParams.empty()) {
        splitAt =-1;
    } else if (cmdParams[0] == '\"') {
        cmdParams = cmdParams.Mid(1);
        splitAt = cmdParams.Find(wxT('\"')) + 1;
    } else if (cmdParams[0] == '\'') {
        cmdParams = cmdParams.Mid(1);
        splitAt = cmdParams.Find(wxT('\'')) + 1;
    } else {
        splitAt = cmdParams.Find(wxT(' ')) + 1;
    }
    if (splitAt < 1) {
        splitAt = cmdParams.length();
    }
    cmdParams = cmdParams.Mid(splitAt);
}
auto aCommand = std::make_shared<ApplyAndSendResponse>(mCommand, output);
Success(aCommand);
#endif
}

void CommandBuilder::BuildCommand(
    AudacityProject& project, const wxString& cmdStringArg)
{
    wxString cmdString(cmdStringArg);

    // Find the command name terminator...  If there is more than one word and
    // no terminator, the command is badly formed
    cmdString.Trim(true);
    cmdString.Trim(false);
    int splitAt = cmdString.Find(wxT(':'));
    if (splitAt < 0 && cmdString.Find(wxT(' ')) >= 0) {
        Failure(wxT("Syntax error!\nCommand is missing ':'"));
        return;
    }

    wxString cmdName = cmdString.Left(splitAt);
    wxString cmdParams = cmdString.Mid(splitAt + 1);
    if (splitAt < 0) {
        cmdParams = "";
    }

    cmdName.Trim(true);
    cmdParams.Trim(false);

    BuildCommand(project, cmdName, cmdParams);
}
