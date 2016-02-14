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

#include "../Audacity.h"
#include "XMLTagHandler.h"

#include "../MemoryX.h"
#include "../Internat.h"

#ifdef _WIN32
   #include <windows.h>
   #include <wx/msw/winundef.h>
#endif

#include <wx/defs.h>
#include <wx/arrstr.h>
#include <wx/filename.h>

#include "../SampleFormat.h"
#include "../Track.h"

bool XMLValueChecker::IsGoodString(const wxString & str)
{
   size_t len = str.Length();
   int nullIndex = str.Find('\0', false);
   if ((len <= PLATFORM_MAX_PATH) && // Shouldn't be any reason for longer strings, except intentional file corruption.
         (nullIndex == -1)) // No null characters except terminator.
      return true;
   else
      return false; // good place for a breakpoint
}

// "Good" means the name is well-formed and names an existing file or folder.
bool XMLValueChecker::IsGoodFileName(const wxString & strFileName, const wxString & strDirName /* = "" */)
{
   // Test strFileName.
   if (!IsGoodFileString(strFileName) ||
         (strDirName.Length() + 1 + strFileName.Length() > PLATFORM_MAX_PATH))
      return false;

   // Test the corresponding wxFileName.
   wxFileName fileName(strDirName, strFileName);
   return (fileName.IsOk() && fileName.FileExists());
}

bool XMLValueChecker::IsGoodFileString(const wxString &str)
{
   return (IsGoodString(str) &&
            !str.IsEmpty() &&

            // FILENAME_MAX is 260 in MSVC, but inconsistent across platforms,
            // sometimes huge, but we use 260 for all platforms.
            (str.Length() <= 260) &&

            (str.Find(wxFileName::GetPathSeparator()) == -1)); // No path separator characters.
}

bool XMLValueChecker::IsGoodSubdirName(const wxString & strSubdirName, const wxString & strDirName /* = "" */)
{
   // Test strSubdirName.
   // Note this prevents path separators, and relative path to parents (strDirName),
   // so fixes vulnerability #3 in the NGS report for UmixIt,
   // where an attacker could craft an AUP file with relative pathnames to get to system files, for example.
   if (!IsGoodFileString(strSubdirName) ||
         (strSubdirName == wxT(".")) || (strSubdirName == wxT("..")) ||
         (strDirName.Length() + 1 + strSubdirName.Length() > PLATFORM_MAX_PATH))
      return false;

   // Test the corresponding wxFileName.
   wxFileName fileName(strDirName, strSubdirName);
   return (fileName.IsOk() && fileName.DirExists());
}

bool XMLValueChecker::IsGoodPathName(const wxString & strPathName)
{
   // Test the corresponding wxFileName.
   wxFileName fileName(strPathName);
   return XMLValueChecker::IsGoodFileName(fileName.GetFullName(), fileName.GetPath(wxPATH_GET_VOLUME));
}

bool XMLValueChecker::IsGoodPathString(const wxString &str)
{
   return (IsGoodString(str) &&
            !str.IsEmpty() &&
            (str.Length() <= PLATFORM_MAX_PATH));
}


bool XMLValueChecker::IsGoodInt(const wxString & strInt)
{
   if (!IsGoodString(strInt))
      return false;

   // Check that the value won't overflow.
   // Signed long: -2,147,483,648 to +2,147,483,647, i.e., -2^31 to 2^31-1
   // We're strict about disallowing spaces and commas, and requiring minus sign to be first char for negative.
   const size_t lenMAXABS = strlen("2147483647");
   const size_t lenStrInt = strInt.Length();

   if (lenStrInt > (lenMAXABS + 1))
      return false;
   else if ((lenStrInt == (lenMAXABS + 1)) && (strInt[0] == '-'))
   {
      const int digitsMAXABS[] = {2, 1, 4, 7, 4, 8, 3, 6, 4, 9};
      unsigned int i;
      for (i = 0; i < lenMAXABS; i++)
         if (strInt[i+1] < '0' || strInt[i+1] > '9')
            return false; // not a digit

      for (i = 0; i < lenMAXABS; i++)
         if (strInt[i+1] - '0' < digitsMAXABS[i])
            return true; // number is small enough
      return false;
   }
   else if (lenStrInt == lenMAXABS)
   {
      const int digitsMAXABS[] = {2, 1, 4, 7, 4, 8, 3, 6, 4, 8};
      unsigned int i;
      for (i = 0; i < lenMAXABS; i++)
         if (strInt[i] < '0' || strInt[i+1] > '9')
            return false; // not a digit
      for (i = 0; i < lenMAXABS; i++)
         if (strInt[i] - '0' < digitsMAXABS[i])
            return true; // number is small enough
      return false;
   }
   return true;
}

bool XMLValueChecker::IsGoodInt64(const wxString & strInt)
{
   if (!IsGoodString(strInt))
      return false;

   // Check that the value won't overflow.
   // Signed 64-bit: -18446744073709551616 to +18446744073709551615, i.e., -2^64 to 2^64-1
   // We're strict about disallowing spaces and commas, and requiring minus sign to be first char for negative.
   const size_t lenMAXABS = strlen("18446744073709551615");
   const size_t lenStrInt = strInt.Length();

   if (lenStrInt > (lenMAXABS + 1))
      return false;
   else if ((lenStrInt == (lenMAXABS + 1)) && (strInt[0] == '-'))
   {
      const int digitsMAXABS[] = {1, 8, 4, 4, 6, 7, 4, 4, 0, 7, 3, 7, 0, 9, 5, 5, 1, 6, 1, 7};
      unsigned int i;
      for (i = 0; i < lenMAXABS; i++)
         if (strInt[i+1] < '0' || strInt[i+1] > '9')
            return false; // not a digit

      for (i = 0; i < lenMAXABS; i++)
         if (strInt[i+1] - '0' < digitsMAXABS[i])
            return true; // number is small enough
      return false;
   }
   else if (lenStrInt == lenMAXABS)
   {
      const int digitsMAXABS[] = {1, 8, 4, 4, 6, 7, 4, 4, 0, 7, 3, 7, 0, 9, 5, 5, 1, 6, 1, 6};
      unsigned int i;
      for (i = 0; i < lenMAXABS; i++)
         if (strInt[i] < '0' || strInt[i+1] > '9')
            return false; // not a digit
      for (i = 0; i < lenMAXABS; i++)
         if (strInt[i] - '0' < digitsMAXABS[i])
            return true; // number is small enough
      return false;
   }
   return true;
}

bool XMLValueChecker::IsValidChannel(const int nValue)
{
   return (nValue >= Track::LeftChannel) && (nValue <= Track::MonoChannel);
}

#ifdef USE_MIDI
bool XMLValueChecker::IsValidVisibleChannels(const int nValue)
{
    return (nValue >= 0 && nValue < (1 << 16));
}
#endif

bool XMLValueChecker::IsValidSampleFormat(const int nValue)
{
   return (nValue == int16Sample) || (nValue == int24Sample) || (nValue == floatSample);
}

bool XMLTagHandler::ReadXMLTag(const char *tag, const char **attrs)
{
   wxArrayString tmp_attrs;

   while (*attrs) {
      const char *s = *attrs++;
      tmp_attrs.Add(UTF8CTOWX(s));
   }

// JKC: Previously the next line was:
// const char **out_attrs = NEW char (const char *)[tmp_attrs.GetCount()+1];
// however MSVC doesn't like the constness in this position, so this is now
// added by a cast after creating the array of pointers-to-non-const chars.
   auto out_attrs = std::make_unique<const wxChar *[]>(tmp_attrs.GetCount() + 1);
   for (size_t i=0; i<tmp_attrs.GetCount(); i++) {
      out_attrs[i] = tmp_attrs[i].c_str();
   }
   out_attrs[tmp_attrs.GetCount()] = 0;

   bool result = HandleXMLTag(UTF8CTOWX(tag).c_str(), out_attrs.get());

   return result;
}

void XMLTagHandler::ReadXMLEndTag(const char *tag)
{
   HandleXMLEndTag(UTF8CTOWX(tag).c_str());
}

void XMLTagHandler::ReadXMLContent(const char *s, int len)
{
   HandleXMLContent(wxString(s, wxConvUTF8, len));
}

XMLTagHandler *XMLTagHandler::ReadXMLChild(const char *tag)
{
   return HandleXMLChild(UTF8CTOWX(tag).c_str());
}
