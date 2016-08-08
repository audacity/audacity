/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandHandler.h
\brief Contains declarations for the CommandHandler class.

*//******************************************************************/

#ifndef __COMMANDHANDLER__
#define __COMMANDHANDLER__

#include "../MemoryX.h"
#include "../AudacityApp.h"
class AudacityProject;
class AppCommandEvent;
class CommandExecutionContext;

class CommandHandler
{
   private:
      std::unique_ptr<CommandExecutionContext> mCurrentContext;

   public:
      CommandHandler(AudacityApp &app);
      ~CommandHandler();

      // This should only be used during initialization
      void SetProject(AudacityProject *proj);

      // Whenever a command is received, process it.
      void OnReceiveCommand(AppCommandEvent &event);
};

#endif /* End of include guard: __COMMANDHANDLER__ */
