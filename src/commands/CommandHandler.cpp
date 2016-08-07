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

#include "../Audacity.h"
#include "CommandHandler.h"
#include <wx/event.h>
#include "../Project.h"
#include "Command.h"
#include "AppCommandEvent.h"
#include "ScriptCommandRelay.h"

CommandHandler::CommandHandler(AudacityApp &app)
 : mCurrentContext(std::make_unique<CommandExecutionContext>
                   (&app, GetActiveProject()))
{ }

CommandHandler::~CommandHandler()
{
}

void CommandHandler::SetProject(AudacityProject *)
{
   // TODO:  Review if the extend command handling is ever utilized
   // mCurrentContext->proj = proj;
}

void CommandHandler::OnReceiveCommand(AppCommandEvent &event)
{
   // First retrieve the actual command from the event 'envelope'.
   CommandHolder cmd = event.GetCommand();

   // JKC: In case the user changed the project, let us track that.
   // This saves us the embarrassment (crash) of a NEW project
   // being opened, the old one closed and still trying to act
   // on the old one.
   SetProject( GetActiveProject() );
   // Then apply it to current application & project.  Note that the
   // command may change the context - for example, switching to a
   // different project.
   cmd->Apply(*mCurrentContext);

   // Redraw the project
   mCurrentContext->GetProject()->RedrawProject();
}
