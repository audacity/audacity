/**********************************************************************

  Audacity: A Digital Audio Editor

  PlatformCompatibility.h

  Platform-specific compatibility functions

  This file implements functions needed to work around
  platform-specific problems and which cannot be solved by a simple
  #ifdef/#endif plus two or three lines additional code. Wherever
  possible, the implementation should be such, that the function is
  implemented on every platform, but is a dummy for those platforms
  on which it is not needed, so additional #ifdef's are unnecessary.

  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_COMPATIBILITY__
#define __AUDACITY_COMPATIBILITY__

#include "Identifier.h"

namespace PlatformCompatibility {
//
// On Win32, this function gets the long file name (like
// "C:\Program Files\Project.aup3") from a short file name like
// "C:\PROGRA~1\PROJEC~1.AUP. On other systems, the function
// just returns the exact string it is given.
//
FilePath FILES_API GetLongFileName(const FilePath& shortFileName);

std::string FILES_API GetUserDataDir();
std::string FILES_API GetUserLocalDataDir();
std::string FILES_API GetResourcesDir();
std::string FILES_API GetDataDir();
std::string FILES_API GetPluginsDir();
std::string FILES_API GetDocumentsDir();
std::string FILES_API GetExecutablePath();
std::string FILES_API GetTempDir();
std::string FILES_API GetHomeDir();
}

#endif
