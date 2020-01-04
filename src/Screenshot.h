/**********************************************************************

  Audacity: A Digital Audio Editor

  Screenshot.h

  Dominic Mazzoni

*******************************************************************//**

\file Screenshot.h

  Opens a modeless dialog that aids in rearranging the project window
  to a canonical size and state and then capturing full and partial
  screenshots to disk.

*//*******************************************************************/

#ifndef __AUDACITY_SCREENSHOT__
#define __AUDACITY_SCREENSHOT__

#include "Audacity.h"

#include <wx/defs.h>

class AudacityProject;

void OpenScreenshotTools( AudacityProject &project );
void CloseScreenshotTools();

#endif // __AUDACITY_SCREENSHOT__
