/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 TempDirectory.h
 
 Paul Licameli split from FileNames.h
 
 **********************************************************************/

#ifndef __AUDACITY_TEMP_DIRECTORY__
#define __AUDACITY_TEMP_DIRECTORY__


#include "BasicUI.h"

class TranslatableString;
class wxWindow;

#include "Identifier.h"

namespace TempDirectory
{
   FILES_API wxString TempDir();
   FILES_API void ResetTempDir();

   FILES_API const FilePath &DefaultTempDir();
   FILES_API void SetDefaultTempDir( const FilePath &tempDir );
   FILES_API bool IsTempDirectoryNameOK( const FilePath & Name );

   // Create a filename for an unsaved/temporary project file
   FILES_API wxString UnsavedProjectFileName();

   FILES_API bool FATFilesystemDenied(const FilePath &path,
                            const TranslatableString &msg,
                            const BasicUI::WindowPlacement &placement = {});
};

#endif
