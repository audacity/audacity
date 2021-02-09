/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file SelectFile.h
 
 Paul Licameli split from FileNames.h
 
 **********************************************************************/

#ifndef __AUDACITY_SELECT_FILE__
#define __AUDACITY_SELECT_FILE__

#include "Identifier.h" // for FilePath
#include <vector>
class wxWindow;

class TranslatableString;
namespace FileNames {
   enum class Operation;
   struct FileType;
}
using FileTypes = std::vector< FileNames::FileType >;

AUDACITY_DLL_API FilePath
SelectFile(FileNames::Operation op,   // op matters only when default_path is empty
   const TranslatableString& message,
   const FilePath& default_path,
   const FilePath& default_filename,
   const FileExtension& default_extension,
   const FileTypes& fileTypes,
   int flags,
   wxWindow *parent);

/** \brief Protect against Unicode to multi-byte conversion failures
 * on Windows */
#if defined(__WXMSW__)
AUDACITY_DLL_API char *VerifyFilename(const wxString &s, bool input = true);
#endif

// Use this macro to wrap all filenames and pathnames that get
// passed directly to a system call, like opening a file, creating
// a directory, checking to see that a file exists, etc...
#if defined(__WXMSW__)
// Note, on Windows we don't define an OSFILENAME() to prevent accidental use.
// See VerifyFilename() for an explanation.
#define OSINPUT(X) VerifyFilename(X, true)
#define OSOUTPUT(X) VerifyFilename(X, false)
#elif defined(__WXMAC__)
#define OSFILENAME(X) ((char *) (const char *)(X).fn_str())
#define OSINPUT(X) OSFILENAME(X)
#define OSOUTPUT(X) OSFILENAME(X)
#else
#define OSFILENAME(X) ((char *) (const char *)(X).mb_str())
#define OSINPUT(X) OSFILENAME(X)
#define OSOUTPUT(X) OSFILENAME(X)
#endif

#endif
