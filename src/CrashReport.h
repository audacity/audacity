/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 CrashReport.h
 
 Paul Licameli
 split from AudacityApp.h
 
 **********************************************************************/

#ifndef __AUDACITY_CRASH_REPORT__
#define __AUDACITY_CRASH_REPORT__

#include "Audacity.h"
#include "Experimental.h"

#if defined(EXPERIMENTAL_CRASH_REPORT)

#include <wx/debugrpt.h>

namespace CrashReport
{
   void Generate(wxDebugReport::Context ctx);
}

#endif

#endif
