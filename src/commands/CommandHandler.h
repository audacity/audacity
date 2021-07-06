/**********************************************************************

   Sneedacity - A Digital Audio Editor
   Copyright 1999-2009 Sneedacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandHandler.h
\brief Contains declarations for the CommandHandler class.

*//******************************************************************/

#ifndef __COMMANDHANDLER__
#define __COMMANDHANDLER__

#include <memory>
class SneedacityApp;
class SneedacityProject;
class AppCommandEvent;
class CommandContext;

class CommandHandler
{
   public:
      CommandHandler();
      ~CommandHandler();

      // Whenever a command is received, process it.
      void OnReceiveCommand(AppCommandEvent &event);
};

#endif /* End of include guard: __COMMANDHANDLER__ */
