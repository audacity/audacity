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
   Validator *commandNameValidator(new Validator());
   signature.AddParameter(wxT("CommandName"), wxT(""), commandNameValidator);
   Validator *paramValidator(new Validator());
   signature.AddParameter(wxT("ParamString"), wxT(""), paramValidator);
   Validator *chainValidator(new Validator());
   signature.AddParameter(wxT("ChainName"), wxT(""), chainValidator);
}

Command *BatchEvalCommandType::Create(CommandOutputTarget *target)
{
   return new BatchEvalCommand(*this, target);
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
