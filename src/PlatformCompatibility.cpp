/**********************************************************************

  Audacity: A Digital Audio Editor

  PlatformCompatibility.cpp

  Markus Meyer

*******************************************************************//*!

\class PlatformCompatibility
\brief Filename Compatibility utilities.

\see FileNames

*//*******************************************************************/


#include "PlatformCompatibility.h"

#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/stdpaths.h>
#include <wx/app.h>

FilePath PlatformCompatibility::GetLongFileName(const FilePath &shortFileName)
{
   wxFileName fn(shortFileName);

   return fn.GetLongPath();
}

const FilePath &PlatformCompatibility::GetExecutablePath()
{
   static bool found = false;
   static FilePath path;

   if (!found) {
      path = wxStandardPaths::Get().GetExecutablePath();

      found = true;
   }

   return path;
}

