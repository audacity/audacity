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

#include "../AudacityApp.h"
class AudacityProject;
class AppCommandEvent;
class CommandExecutionContext;

class CommandHandler
{
   private:
      CommandExecutionContext *mCurrentContext;

   public:
      CommandHandler(AudacityApp &app);
      ~CommandHandler();

      // This should only be used during initialization
      void SetProject(AudacityProject *proj);

      // Whenever a command is recieved, process it.
      void OnReceiveCommand(AppCommandEvent &event);
};

#endif /* End of include guard: __COMMANDHANDLER__ */

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
