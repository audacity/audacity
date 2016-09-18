/**********************************************************************

  Audacity: A Digital Audio Editor

  PlatformCompatibility.cpp

  Markus Meyer

*******************************************************************//*!

\class PlatformCompatibility
\brief Filename Compatibility utilities.

\see FileNames

*//*******************************************************************/


#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/stdpaths.h>
#include <wx/app.h>

#include "AudacityApp.h"
#include "PlatformCompatibility.h"

wxString PlatformCompatibility::GetLongFileName(const wxString& shortFileName)
{
   wxFileName fn(shortFileName);

   return fn.GetLongPath();
}

const wxString &PlatformCompatibility::GetExecutablePath()
{
   static bool found = false;
   static wxString path;

   if (!found) {
      path = wxStandardPaths::Get().GetExecutablePath();

      found = true;
   }

   return path;
}

wxString PlatformCompatibility::ConvertSlashInFileName(const wxString& filePath)
{
#ifdef __WXMAC__
   wxString path = filePath;
   wxString filename;
   wxString newPath = filePath;
   // int pathLen = 1;
   while (!wxDirExists(wxPathOnly(newPath)) && ! path.IsEmpty()) {
      path = newPath.BeforeLast('/');
      filename = newPath.AfterLast('/');
      newPath = path;
      newPath += ':';
      newPath += filename;
   }
   return newPath;
#else
   return filePath;
#endif
}