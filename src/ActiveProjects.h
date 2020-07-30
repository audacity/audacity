/**********************************************************************

  Audacity: A Digital Audio Editor

  ActiveProjects.h

**********************************************************************/

#ifndef __AUDACITY_ACTIVE_PROJECTS__
#define __AUDACITY_ACTIVE_PROJECTS__

#include "Audacity.h"
#include "audacity/Types.h"

#include <wx/string.h>

namespace ActiveProjects
{
   FilePaths GetAll();
   void Add(const FilePath &path);
   void Remove(const FilePath &path);
   wxString Find(const FilePath &path);
};

#endif
