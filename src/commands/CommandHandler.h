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
class AudacityApp;
class AudacityProject;
class AppCommandEvent;
class CommandContext;

class CommandHandler
{
   private:
      std::unique_ptr<const CommandContext> mCurrentContext;

   public:
      CommandHandler();
      ~CommandHandler();

      // This should only be used during initialization
      void SetProject(AudacityProject *proj);

      // Whenever a command is received, process it.
      void OnReceiveCommand(AppCommandEvent &event);
};

#endif /* End of include guard: __COMMANDHANDLER__ */
