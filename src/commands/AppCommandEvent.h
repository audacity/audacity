/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file AppCommandEvent.h
\brief Headers and event table macros for AppCommandEvent

*//*******************************************************************/

#ifndef __APPCOMMANDEVENT__
#define __APPCOMMANDEVENT__

#include <wx/event.h>
#include "../Audacity.h"

DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, wxEVT_APP_COMMAND_RECEIVED, -1);

class Command;

class AppCommandEvent : public wxCommandEvent
{
private:
   Command *mCommand;

public:
   AppCommandEvent(wxEventType commandType = wxEVT_APP_COMMAND_RECEIVED, int id = 0);

   AppCommandEvent(const AppCommandEvent &event);
   ~AppCommandEvent();

   virtual wxEvent *Clone() const;
   void SetCommand(Command *cmd);
   Command *GetCommand();

private:
   DECLARE_DYNAMIC_CLASS(AppCommandEvent)
};

typedef void (wxEvtHandler::*wxAppCommandEventFunction)(AppCommandEvent&);

#define wxAppCommandEventHandler(func) \
    (wxObjectEventFunction)(wxEventFunction)wxStaticCastEvent(wxAppCommandEventFunction, &func)

#define EVT_APP_COMMAND(winid, fn) DECLARE_EVENT_TABLE_ENTRY( wxEVT_APP_COMMAND_RECEIVED, winid, wxID_ANY, wxAppCommandEventHandler(fn), (wxObject *) NULL ),

#endif /* End of include guard: __APPCOMMANDEVENT__ */

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
