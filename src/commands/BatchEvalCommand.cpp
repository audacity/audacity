/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file BatchEvalCommand.cpp
\brief Contains definitions for the BatchEvalCommand class

*//*******************************************************************/

#include "../Audacity.h"
#include "BatchEvalCommand.h"
#include "CommandContext.h"

ComponentInterfaceSymbol BatchEvalCommandType::BuildName()
{
   return { wxT("BatchCommand"), XO("Batch Command") };
}

void BatchEvalCommandType::BuildSignature(CommandSignature &signature)
{
   auto commandNameValidator = std::make_unique<DefaultValidator>();
   signature.AddParameter(wxT("CommandName"), wxT(""), std::move(commandNameValidator));
   auto paramValidator = std::make_unique<DefaultValidator>();
   signature.AddParameter(wxT("ParamString"), wxT(""), std::move(paramValidator));
   auto macroValidator = std::make_unique<DefaultValidator>();
   signature.AddParameter(wxT("MacroName"), wxT(""), std::move(macroValidator));
}

OldStyleCommandPointer BatchEvalCommandType::Create(std::unique_ptr<CommandOutputTargets> && WXUNUSED(target))
{
   return std::make_shared<BatchEvalCommand>(*this);
}

bool BatchEvalCommand::Apply(const CommandContext & context)
{
   // Uh oh, I need to build a catalog, expensively
   // Maybe it can be built in one long-lived place and shared among command
   // objects instead?
   MacroCommandsCatalog catalog(&context.project);

   wxString macroName = GetString(wxT("MacroName"));
   if (!macroName.empty())
   {
      MacroCommands batch;
      batch.ReadMacro(macroName);
      return batch.ApplyMacro(catalog);
   }

   wxString cmdName = GetString(wxT("CommandName"));
   wxString cmdParams = GetString(wxT("ParamString"));
   auto iter = catalog.ByCommandId(cmdName);
   const wxString &friendly = (iter == catalog.end())
      ? cmdName // Expose internal name to user, in default of a better one!
      : iter->name.Translated();

   // Create a Batch that will have just one command in it...
   MacroCommands Batch;
   bool bResult = Batch.ApplyCommandInBatchMode(friendly, cmdName, cmdParams, &context);
   // Relay messages, if any.
   wxString Message = Batch.GetMessage();
   if( !Message.empty() )
      context.Status( Message );
   return bResult;
}

BatchEvalCommand::~BatchEvalCommand()
{ }
