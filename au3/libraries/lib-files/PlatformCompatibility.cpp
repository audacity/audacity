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

FilePath PlatformCompatibility::GetLongFileName(const FilePath& shortFileName)
{
    wxFileName fn(shortFileName);

    return fn.GetLongPath();
}
