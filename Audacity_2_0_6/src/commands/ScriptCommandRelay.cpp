/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ScriptCommandRelay.cpp
\brief Contains definitions for ScriptCommandRelay

*//****************************************************************//**

\class ScriptCommandRelay
\brief ScriptCommandRelay is just a way to move some of the scripting-specific
code out of LoadModules.

*//*******************************************************************/

#include "ScriptCommandRelay.h"
#include "CommandTargets.h"
#include "CommandBuilder.h"
#include "AppCommandEvent.h"
#include "ResponseQueue.h"
#include "../Project.h"
#include <wx/string.h>

// Declare static class members
CommandHandler *ScriptCommandRelay::sCmdHandler;
tpRegScriptServerFunc ScriptCommandRelay::sScriptFn;
ResponseQueue ScriptCommandRelay::sResponseQueue;

void ScriptCommandRelay::SetRegScriptServerFunc(tpRegScriptServerFunc scriptFn)
{
   sScriptFn = scriptFn;
}

void ScriptCommandRelay::SetCommandHandler(CommandHandler &ch)
{
   sCmdHandler = &ch;
}

/// Calls the script function, passing it the function for obeying commands
void ScriptCommandRelay::Run()
{
   wxASSERT( sScriptFn != NULL );
   while( true )
      sScriptFn(&ExecCommand);
}

/// Send a command to a project, to be applied in that context.
void ScriptCommandRelay::PostCommand(AudacityProject *project, Command *cmd)
{
   wxASSERT(project != NULL);
   wxASSERT(cmd != NULL);
   AppCommandEvent ev;
   ev.SetCommand(cmd);
   project->AddPendingEvent(ev);
}

/// This is the function which actually obeys one command.  Rather than applying
/// the command directly, an event containing a reference to the command is sent
/// to the main (GUI) thread. This is because having more than one thread access
/// the GUI at a time causes problems with wxwidgets.
int ExecCommand(wxString *pIn, wxString *pOut)
{
   CommandBuilder builder(*pIn);
   if (builder.WasValid())
   {
      AudacityProject *project = GetActiveProject();
      project->SafeDisplayStatusMessage(wxT("Received script command"));
      Command *cmd = builder.GetCommand();
      ScriptCommandRelay::PostCommand(project, cmd);

      *pOut = wxEmptyString;
   } else
   {
      *pOut = wxT("Syntax error!\n");
      *pOut += builder.GetErrorMessage() + wxT("\n");
      builder.Cleanup();
   }

   // Wait until all responses from the command have been received.
   // The last response is signalled by an empty line.
   wxString msg = ScriptCommandRelay::ReceiveResponse().GetMessage();
   while (msg != wxT("\n"))
   {
      *pOut += msg + wxT("\n");
      msg = ScriptCommandRelay::ReceiveResponse().GetMessage();
   }

   return 0;
}

/// Adds a response to the queue to be sent back to the script
void ScriptCommandRelay::SendResponse(const wxString &response)
{
   sResponseQueue.AddResponse(response);
}

/// Gets a response from the queue (may block)
Response ScriptCommandRelay::ReceiveResponse()
{
   return ScriptCommandRelay::sResponseQueue.WaitAndGetResponse();
}

/// Get a pointer to a message target which allows commands to send responses
/// back to a script.
ResponseQueueTarget *ScriptCommandRelay::GetResponseTarget()
{
   // This should be deleted by a Command destructor
   return new ResponseQueueTarget(sResponseQueue);
}
