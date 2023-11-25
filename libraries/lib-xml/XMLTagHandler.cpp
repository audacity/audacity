/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLTagHandler.cpp

  Dominic Mazzoni
  Vaughan Johnson


*//****************************************************************//**

\class XMLTagHandler
\brief This class is an interface which should be implemented by
  classes which wish to be able to load and save themselves
  using XML files.

\class XMLValueChecker
\brief XMLValueChecker implements static bool methods for checking
  input values from XML files.

*//*******************************************************************/

#include "XMLTagHandler.h"

#ifdef _WIN32
   #include <windows.h>
   #include <wx/msw/winundef.h>
#endif

#include <wx/defs.h>
#include <wx/arrstr.h>
#include <wx/filename.h>

#include "FileNames.h"


// "Good" means the name is well-formed and names an existing file or folder.
bool XMLValueChecker::IsGoodFileName(const FilePath & strFileName, const FilePath & strDirName /* = "{} */)
{
   // Test strFileName.
   if (!IsGoodFileString(strFileName) ||
         (strDirName.length() + 1 + strFileName.length() > PLATFORM_MAX_PATH))
      return false;

   // Test the corresponding wxFileName.
   wxFileName fileName(strDirName, strFileName);
   return (fileName.IsOk() && fileName.FileExists());
}

bool XMLValueChecker::IsGoodFileString(const FilePath &str)
{
   return (!str.empty() &&

            // FILENAME_MAX is 260 in MSVC, but inconsistent across platforms,
            // sometimes huge, but we use 260 for all platforms.
            (str.length() <= 260) &&

            (str.Find(wxFileName::GetPathSeparator()) == -1)); // No path separator characters.
}

bool XMLValueChecker::IsGoodSubdirName(const FilePath & strSubdirName, const FilePath & strDirName /* = {} */)
{
   // Test strSubdirName.
   // Note this prevents path separators, and relative path to parents (strDirName),
   // so fixes vulnerability #3 in the NGS report for UmixIt,
   // where an attacker could craft an AUP file with relative pathnames to get to system files, for example.
   if (!IsGoodFileString(strSubdirName) ||
         (strSubdirName == wxT(".")) || (strSubdirName == wxT("..")) ||
         (strDirName.length() + 1 + strSubdirName.length() > PLATFORM_MAX_PATH))
      return false;

   // Test the corresponding wxFileName.
   wxFileName fileName(strDirName, strSubdirName);
   return (fileName.IsOk() && fileName.DirExists());
}

bool XMLValueChecker::IsGoodPathName(const FilePath & strPathName)
{
   // Test the corresponding wxFileName.
   wxFileName fileName(strPathName);
   return XMLValueChecker::IsGoodFileName(fileName.GetFullName(), fileName.GetPath(wxPATH_GET_VOLUME));
}

bool XMLValueChecker::IsGoodPathString(const FilePath &str)
{
   return (!str.empty() &&
            (str.length() <= PLATFORM_MAX_PATH));
}

XMLTagHandlerBase::XMLTagHandlerBase() = default;
XMLTagHandlerBase::~XMLTagHandlerBase() = default;

void XMLTagHandlerBase::HandleXMLEndTag(const std::string_view &tag)
{}

void XMLTagHandlerBase::HandleXMLContent(const std::string_view &content)
{}

void XMLTagHandlerBase::ReadXMLEndTag(const char *tag)
{
   DoHandleXMLEndTag(tag);
}

void XMLTagHandlerBase::ReadXMLContent(const char *s, int len)
{
   DoHandleXMLContent(std::string_view(s, len));
}

XMLTagHandlerBase *XMLTagHandlerBase::ReadXMLChild(const char *tag)
{
   return DoHandleXMLChild(tag);
}
