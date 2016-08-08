/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file BatchEvalCommand.cpp
\brief Contains definitions for the BatchEvalCommand class

*//*******************************************************************/

#include "BatchEvalCommand.h"

wxString BatchEvalCommandType::BuildName()
{
   return wxT("BatchCommand");
}

void BatchEvalCommandType::BuildSignature(CommandSignature &signature)
{
   auto commandNameValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("CommandName"), wxT(""), std::move(commandNameValidator));
   auto paramValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("ParamString"), wxT(""), std::move(paramValidator));
   auto chainValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("ChainName"), wxT(""), std::move(chainValidator));
}

CommandHolder BatchEvalCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<BatchEvalCommand>(*this, std::move(target));
}

bool BatchEvalCommand::Apply(CommandExecutionContext WXUNUSED(context))
{

   wxString chainName = GetString(wxT("ChainName"));
   if (chainName != wxT(""))
   {
      BatchCommands batch;
      batch.ReadChain(chainName);
      return batch.ApplyChain();
   }

   wxString cmdName = GetString(wxT("CommandName"));
   wxString cmdParams = GetString(wxT("ParamString"));

   // Create a Batch that will have just one command in it...
   BatchCommands Batch;

   return Batch.ApplyCommand(cmdName, cmdParams);
}

BatchEvalCommand::~BatchEvalCommand()
{ }
