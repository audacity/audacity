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

#include "audacity/Types.h"
#include <wx/defs.h>
#include <wx/arrstr.h>
#include <wx/filename.h>

#include "FileNames.h"
#include "SampleFormat.h"

// Length check.  Is in part about not supplying malicious strings to file functions.
bool XMLValueChecker::IsGoodString(const wxString & str)
{
   // Originally based on MAX_PATH, which is way too limiting and just wrong since
   // the length check is for a plain string and not a filename
   if (IsGoodLongString(str) && str.length() <= 4096) // Shouldn't be any reason for longer strings, except intentional file corruption.
   {
      return true;
   }

   return false;
}

// No length check, as e.g. labels could be very long.
bool XMLValueChecker::IsGoodLongString(const wxString & str)
{
   return str.Find('\0', false) == wxNOT_FOUND; // No null characters except terminator.
}


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
   return (IsGoodString(str) &&
            !str.empty() &&

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
   return (IsGoodString(str) &&
            !str.empty() &&
            (str.length() <= PLATFORM_MAX_PATH));
}


bool XMLValueChecker::IsGoodIntForRange(const wxString & strInt, const wxString & strMAXABS)
{
   if (!IsGoodString(strInt))
      return false;

   // Check that the value won't overflow.
   // Must lie between -Range and +Range-1
   // We're strict about disallowing spaces and commas, and requiring minus sign to be first 
   // char for negative. No + sign for positive numbers.  It's disallowed, not optional.

   const size_t lenMAXABS = strMAXABS.length();
   const size_t lenStrInt = strInt.length();

   if( lenStrInt < 1 )
      return false;
   size_t offset = (strInt[0] == '-') ?1:0;
   if( lenStrInt <= offset )
      return false;// string too short, no digits in it.

   if (lenStrInt > (lenMAXABS + offset))
      return false;

   unsigned int i;
   for (i = offset; i < lenStrInt; i++)
      if (strInt[i] < '0' || strInt[i] > '9' )
          return false; // not a digit

   // All chars were digits.
   if( lenStrInt < (lenMAXABS + offset) )
      return true; // too few digits to overflow.

   // Numerical part is same length as strMAXABS
   for (i = 0; i < lenMAXABS; i++)
      if (strInt[i+offset] < strMAXABS[i])
         return true; // number is small enough
      else if (strInt[i+offset] > strMAXABS[i])
         return false; // number is too big.

   // Digits were textually equal to strMAXABS
   // That's OK if negative, but not OK if positive.
   return (strInt[0] == '-');
}


bool XMLValueChecker::IsGoodInt(const wxString & strInt)
{
   // Signed long: -2,147,483,648 to +2,147,483,647, i.e., -2^31 to 2^31-1
   return IsGoodIntForRange( strInt, "2147483648" );
}

bool XMLValueChecker::IsGoodInt64(const wxString & strInt)
{
   // Signed 64-bit:  -9,223,372,036,854,775,808 to +9,223,372,036,854,775,807, i.e., -2^63 to 2^63-1
   return IsGoodIntForRange( strInt, "9223372036854775808" );
}

bool XMLValueChecker::IsValidChannel(const int nValue)
{
   return (nValue >= LeftChannel) && (nValue <= MonoChannel);
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
      tmp_attrs.push_back(UTF8CTOWX(s));
   }

// JKC: Previously the next line was:
// const char **out_attrs = NEW char (const char *)[tmp_attrs.size()+1];
// however MSVC doesn't like the constness in this position, so this is now
// added by a cast after creating the array of pointers-to-non-const chars.
   auto out_attrs = std::make_unique<const wxChar *[]>(tmp_attrs.size() + 1);
   for (size_t i=0; i<tmp_attrs.size(); i++) {
      out_attrs[i] = tmp_attrs[i];
   }
   out_attrs[tmp_attrs.size()] = 0;

   bool result = HandleXMLTag(UTF8CTOWX(tag), out_attrs.get());

   return result;
}

void XMLTagHandler::ReadXMLEndTag(const char *tag)
{
   HandleXMLEndTag(UTF8CTOWX(tag));
}

void XMLTagHandler::ReadXMLContent(const char *s, int len)
{
   HandleXMLContent(wxString(s, wxConvUTF8, len));
}

XMLTagHandler *XMLTagHandler::ReadXMLChild(const char *tag)
{
   return HandleXMLChild(UTF8CTOWX(tag));
}
