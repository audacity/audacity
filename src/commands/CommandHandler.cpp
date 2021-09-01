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


#include "CommandHandler.h"

#include "../ActiveProject.h"
#include "Project.h"
#include "../ProjectWindow.h"
#include "AppCommandEvent.h"
#include "ScriptCommandRelay.h"
#include "../commands/CommandContext.h"
#include "../commands/Command.h"

CommandHandler::CommandHandler()
{
}

CommandHandler::~CommandHandler()
{
}

void CommandHandler::OnReceiveCommand(AppCommandEvent &event)
{
   // First retrieve the actual command from the event 'envelope'.
   OldStyleCommandPointer cmd = event.GetCommand();

   if (const auto pProject = GetActiveProject().lock()) {
      // Then apply it to current application & project.  Note that the
      // command may change the context - for example, switching to a
      // different project.
      CommandContext context{ *pProject };
      auto result = GuardedCall<bool>( [&] {
         return cmd->Apply( context );
      });
      wxUnusedVar(result);

      // Redraw the project
      ProjectWindow::Get( context.project ).RedrawProject();
   }
}
