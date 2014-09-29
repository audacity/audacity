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

#include "CommandDirectory.h"
#include "CommandBuilder.h"
#include "../Shuttle.h"
#include "BatchEvalCommand.h"
#include "Command.h"
#include "CommandTargets.h"
#include "ScriptCommandRelay.h"

CommandBuilder::CommandBuilder(const wxString &cmdString)
   : mValid(false), mCommand(NULL)
{
   BuildCommand(cmdString);
}

CommandBuilder::CommandBuilder(const wxString &cmdName, const wxString &params)
   : mValid(false), mCommand(NULL)
{
   BuildCommand(cmdName, params);
}

CommandBuilder::~CommandBuilder()
{
   Cleanup();
}

bool CommandBuilder::WasValid()
{
   return mValid;
}

const wxString &CommandBuilder::GetErrorMessage()
{
   return mError;
}

Command *CommandBuilder::GetCommand()
{
   wxASSERT(mValid);
   wxASSERT(NULL != mCommand);
   Command *tmp = mCommand;
   mCommand = NULL;
   return tmp;
}

void CommandBuilder::Cleanup()
{
   if (mCommand != NULL)
   {
      delete mCommand;
      mCommand = NULL;
   }
}

void CommandBuilder::Failure(const wxString &msg)
{
   mError = msg;
   mValid = false;
}

void CommandBuilder::Success(Command *cmd)
{
   mCommand = cmd;
   mValid = true;
}

void CommandBuilder::BuildCommand(const wxString &cmdName,
                                  wxString cmdParams)
{
   // Stage 1: create a Command object of the right type

   CommandMessageTarget *scriptOutput = ScriptCommandRelay::GetResponseTarget();
   CommandOutputTarget *output
      = new CommandOutputTarget(new NullProgressTarget(),
                                scriptOutput,
                                scriptOutput);

   CommandType *factory = CommandDirectory::Get()->LookUp(cmdName);

   if (factory == NULL)
   {
      // Fall back to hoping the Batch Command system can handle it
      CommandType *type = CommandDirectory::Get()->LookUp(wxT("BatchCommand"));
      wxASSERT(type != NULL);
      mCommand = type->Create(output);
      mCommand->SetParameter(wxT("CommandName"), cmdName);
      mCommand->SetParameter(wxT("ParamString"), cmdParams);
      Success(new ApplyAndSendResponse(mCommand));
      return;
   }

   CommandSignature &signature = factory->GetSignature();
   mCommand = factory->Create(output);

   // Stage 2: set the parameters

   ShuttleCli shuttle;
   shuttle.mParams = cmdParams;
   shuttle.mbStoreInClient = true;

   ParamValueMap::const_iterator iter;
   ParamValueMap params = signature.GetDefaults();

   for (iter = params.begin(); iter != params.end(); ++iter)
   {
      wxString paramString;
      if (shuttle.TransferString(iter->first, paramString, wxT("")))
      {
         if (!mCommand->SetParameter(iter->first, paramString))
         {
            Failure();
            return;
         }
      }
   }

   // Check for unrecognised parameters

   while (cmdParams != wxEmptyString)
   {
      cmdParams.Trim(true);
      cmdParams.Trim(false);
      int splitAt = cmdParams.Find(wxT('='));
      if (splitAt < 0 && cmdParams != wxEmptyString)
      {
         Failure(wxT("Parameter string is missing '='"));
         return;
      }
      wxString paramName = cmdParams.Left(splitAt);
      if (params.find(paramName) == params.end())
      {
         Failure(wxT("Unrecognized parameter: '") + paramName + wxT("'"));
         return;
      }
      cmdParams = cmdParams.Mid(splitAt+1);
      splitAt = cmdParams.Find(wxT(' '));
      if (splitAt < 0)
      {
         splitAt = cmdParams.Len();
      }
      cmdParams = cmdParams.Mid(splitAt);
   }

   Success(new ApplyAndSendResponse(mCommand));
}

void CommandBuilder::BuildCommand(wxString cmdString)
{
   // Find the command name terminator...  If there is more than one word and
   // no terminator, the command is badly formed
   cmdString.Trim(true); cmdString.Trim(false);
   int splitAt = cmdString.Find(wxT(':'));
   if (splitAt < 0 && cmdString.Find(wxT(' ')) >= 0) {
      mError = wxT("Command is missing ':'");
      ScriptCommandRelay::SendResponse(wxT("\n"));
      mValid = false;
      return;
   }

   wxString cmdName = cmdString.Left(splitAt);
   wxString cmdParams = cmdString.Mid(splitAt+1);
   cmdName.Trim(true);
   cmdParams.Trim(false);

   BuildCommand(cmdName, cmdParams);
}
