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

#include <wx/string.h>

class PlatformCompatibility
{
public:
   //
   // On Win32, this function gets the long file name (like
   // "C:\Program Files\Project.aup") from a short file name like
   // "C:\PROGRA~1\PROJEC~1.AUP. On other systems, the function
   // just returns the exact string it is given.
   //
   static wxString GetLongFileName(const wxString& shortFileName);

   //
   // Get filename and path of executable (e.g. "/usr/bin/audacity" on
   // Linux or "C:\Program Files\Audacity\Audacity.exe" on Windows)
   // This string is unchanging
   //
   static const wxString &GetExecutablePath();

   //
   // Audacity treats the / as a file seperator always for Mac OS,
   // however /'s are allowed in the filename. In order for /'s to
   // work they muse be treated as :'s. To facilitate this, this
   // function should be called when opening or saving a file on
   // the Mac. It's important to note that if a / is used in a filename
   // and folder exists in the same folder with the same name as the part
   // of the file (before the first /) then the file will be saved inside
   // of that directory. This function also exists in the FileDialogPrivate
   // object
   //
   static wxString ConvertSlashInFileName(const wxString& filePath);
};

#endif
