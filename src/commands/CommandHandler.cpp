/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandHandler.cpp
\brief Contains definitions for the CommandHandler class.

\class CommandHandler
\brief Contains methods for applying commands that are passed to it.

*//*******************************************************************/

#include <wx/event.h>
#include "CommandHandler.h"
#include "../Project.h"
#include "Command.h"
#include "AppCommandEvent.h"
#include "ScriptCommandRelay.h"

CommandHandler::CommandHandler(AudacityApp &app)
 : mCurrentContext(new CommandExecutionContext(&app, GetActiveProject()))
{ }

CommandHandler::~CommandHandler()
{
   delete mCurrentContext;
}

void CommandHandler::SetProject(AudacityProject *proj)
{
   mCurrentContext->proj = proj;
}

void CommandHandler::OnReceiveCommand(AppCommandEvent &event)
{
   // First retrieve the actual command from the event 'envelope'.
   Command *cmd = event.GetCommand();

   // Then apply it to current application & project.  Note that the
   // command may change the context - for example, switching to a
   // different project.
   cmd->Apply(*mCurrentContext);

   // Done with the command so delete it.
   delete cmd;

   // Redraw the project
   mCurrentContext->proj->RedrawProject();
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
