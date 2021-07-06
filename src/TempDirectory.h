/**********************************************************************
 
 Sneedacity: A Digital Audio Editor
 
 TempDirectory.h
 
 Paul Licameli split from FileNames.h
 
 **********************************************************************/

#ifndef __SNEEDACITY_TEMP_DIRECTORY__
#define __SNEEDACITY_TEMP_DIRECTORY__


#include "Identifier.h"
class TranslatableString;
class wxWindow;

namespace TempDirectory
{
   SNEEDACITY_DLL_API wxString TempDir();
   SNEEDACITY_DLL_API void ResetTempDir();

   SNEEDACITY_DLL_API const FilePath &DefaultTempDir();
   SNEEDACITY_DLL_API void SetDefaultTempDir( const FilePath &tempDir );
   SNEEDACITY_DLL_API bool IsTempDirectoryNameOK( const FilePath & Name );

   // Create a filename for an unsaved/temporary project file
   SNEEDACITY_DLL_API wxString UnsavedProjectFileName();

   SNEEDACITY_DLL_API bool FATFilesystemDenied(const FilePath &path,
                            const TranslatableString &msg,
                            wxWindow *window = nullptr);
};

#endif
