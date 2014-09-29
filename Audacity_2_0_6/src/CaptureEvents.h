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

#ifndef _AUDACITY_CAPTURE_EVENTS_
#define _AUDACITY_CAPTURE_EVENTS_

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

#include <wx/dynarray.h>

class CaptureEvents
{
 public:
   CaptureEvents();

   virtual ~CaptureEvents();

 private:
   wxArrayPtrVoid queue;
};
#endif

#endif
