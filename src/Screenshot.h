/**********************************************************************

  Sneedacity: A Digital Audio Editor

  Screenshot.h

  Dominic Mazzoni

*******************************************************************//**

\file Screenshot.h

  Opens a modeless dialog that aids in rearranging the project window
  to a canonical size and state and then capturing full and partial
  screenshots to disk.

*//*******************************************************************/

#ifndef __SNEEDACITY_SCREENSHOT__
#define __SNEEDACITY_SCREENSHOT__



#include <wx/defs.h>

class SneedacityProject;

SNEEDACITY_DLL_API void OpenScreenshotTools( SneedacityProject &project );
void CloseScreenshotTools();

#endif // __SNEEDACITY_SCREENSHOT__
