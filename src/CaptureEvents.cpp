/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2008 Audacity Team.
   License: GPL v2.  See License.txt.

   CaptureEvents.h
   Created by Al Dimond, Oct. 2009 (from code by someone else)

******************************************************************//**

\class CaptureEvents
\brief RAII-style class to work around a bug in wxGTK 2.8.9-?

*//*******************************************************************/

#include "Audacity.h"
#include "CaptureEvents.h"

#if defined(__WXGTK__) && defined(HAVE_GTK)
// As of wxGTK 2.8.9, there is a problem in the wxClipboard class that
// allows recursive event processing.  This problem has been corrected
// by wxWidgets 2.9+.  However, this han't made it into a release yet,
// so we have to work around it.
//
// This is done by pulling/merging in some code from wx29 and creating
// the following class to capture events while accessing the clipboard
// to prevent the asynchronous clipboard access from causing recursive
// event processing.

#include <wx/app.h>
#include <wx/dynarray.h>
#include <wx/log.h>
#include <gtk/gtk.h>

extern GtkWidget *wxGetRootWindow();

static void main_do_event(GdkEvent *event, wxArrayPtrVoid *queue)
{
   switch (event->type)
   {
      case GDK_NOTHING:
         // Ignore it
      break;

      case GDK_SELECTION_REQUEST:
      case GDK_SELECTION_NOTIFY:
      case GDK_SELECTION_CLEAR:
#if GTK_CHECK_VERSION(2,6,0)
      case GDK_OWNER_CHANGE:
#endif
         // process it now
         gtk_main_do_event(event);
      break;

      default:
         // process it later (but make a copy; the caller will free the event pointer)
         queue->Add(gdk_event_copy(event));
      break;
   }

   // don't allow idle callbacks while we're active
   wxTheApp->SuspendIdleCallback();

   return;
}

CaptureEvents::CaptureEvents()
{
#if wxUSE_LOG
   // disable log flushing from here because a call to wxGetApp().Yield() shouldn't
   // normally result in message boxes popping up &c
   wxLog::Suspend();
#endif

   // temporarily replace the global GDK event handler with our function
   gdk_event_handler_set((GdkEventFunc)main_do_event, &queue, NULL);

   // temporarily suspend idle callbacks
   wxTheApp->SuspendIdleCallback();
}

CaptureEvents::~CaptureEvents()
{
   gdk_event_handler_set((GdkEventFunc)gtk_main_do_event, NULL, NULL);

   // put all unprocessed GDK events back in the queue
   GdkDisplay* disp = gtk_widget_get_display(wxGetRootWindow());
   size_t cnt = queue.GetCount();
   for (size_t i = 0; i < cnt; i++) {
      GdkEvent* event = (GdkEvent*)queue[i];
      // NOTE: gdk_display_put_event makes a copy of the event passed to it
      gdk_display_put_event(disp, event);
      gdk_event_free(event);
   }

#if wxUSE_LOG
   // let the logs be flashed again
   wxLog::Resume();
#endif
}


#endif

