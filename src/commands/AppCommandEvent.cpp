/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file AppCommandEvent.cpp
\brief Implements AppCommandEvent.

*//****************************************************************//**

\class AppCommandEvent
\brief An event 'envelope' for sending Command objects through the wxwidgets
event loop.

   This allows commands to be communicated from the script thread to the main
   thread.

*//*******************************************************************/

#include "AppCommandEvent.h"

DEFINE_EVENT_TYPE(wxEVT_APP_COMMAND_RECEIVED)
IMPLEMENT_DYNAMIC_CLASS(AppCommandEvent, wxEvent)

AppCommandEvent::AppCommandEvent(wxEventType commandType, int id)
: wxCommandEvent(commandType, id), mCommand(NULL)
{ }

// Copy constructor
AppCommandEvent::AppCommandEvent(const AppCommandEvent &event) : wxCommandEvent(event)
{
   this->mCommand = event.mCommand;
}

AppCommandEvent::~AppCommandEvent()
{
}

// Clone is required by wxwidgets; implemented via copy constructor
wxEvent *AppCommandEvent::Clone() const
{
   return new AppCommandEvent(*this);
}

/// Store a pointer to a command object
void AppCommandEvent::SetCommand(Command *cmd)
{
   wxASSERT(NULL == mCommand);
   mCommand = cmd;
}

// When the command pointer is retrieved, the caller is responsible for
// deletion.
Command *AppCommandEvent::GetCommand()
{
   Command *tmp = mCommand;
   mCommand     = NULL;
   return tmp;
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
