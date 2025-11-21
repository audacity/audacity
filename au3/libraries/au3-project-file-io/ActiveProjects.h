/**********************************************************************

  Audacity: A Digital Audio Editor

  ActiveProjects.h

**********************************************************************/

#ifndef __AUDACITY_ACTIVE_PROJECTS__
#define __AUDACITY_ACTIVE_PROJECTS__

#include "Identifier.h"

namespace ActiveProjects {
PROJECT_FILE_IO_API FilePaths GetAll();
PROJECT_FILE_IO_API void Add(const FilePath& path);
PROJECT_FILE_IO_API void Remove(const FilePath& path);
PROJECT_FILE_IO_API wxString Find(const FilePath& path);
}

#endif
