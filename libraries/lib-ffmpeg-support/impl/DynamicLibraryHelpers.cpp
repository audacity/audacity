/**********************************************************************

  Audacity: A Digital Audio Editor

  DynamicLibraryHelpers.cpp

  Dmitry Vedenko

**********************************************************************/

#include "DynamicLibraryHelpers.h"

#include <wx/dynlib.h>

#include "FFmpegTypes.h"

using GetVersionFn = unsigned (*)();

bool GetAVVersion(
   const wxDynamicLibrary& lib, const char* name, FFMPegVersion& version)
{
   GetVersionFn versionFn = reinterpret_cast<GetVersionFn>(lib.GetSymbol(name));

   if (nullptr == versionFn)
      return false;

   const unsigned fullVersion = versionFn();

   version.Major = (fullVersion >> 16) & 0xFF;
   version.Minor = (fullVersion >> 8) & 0xFF;
   version.Micro = fullVersion & 0xFF;

   return true;
}
