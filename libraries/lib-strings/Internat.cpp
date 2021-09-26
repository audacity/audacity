/**********************************************************************

  Audacity: A Digital Audio Editor

  Internat.cpp

  Markus Meyer
  Dominic Mazzoni (Mac OS X code)

*******************************************************************//*!

\class Internat
\brief Internationalisation support.

This class is used to help internationalisation and in general
compatibility with different locales and character sets.
It deals mostly with converting numbers, but also has important
functions to convert to/from UTF-8, which is used in XML files
and on Mac OS X for the filesystem.

*//*******************************************************************/

#include "Internat.h"

#include <wx/log.h>
#include <wx/intl.h>
#include <wx/filename.h>

#include <locale.h>
#include <math.h> // for pow()

// in order for the static member variables to exist, they must appear here
// (_outside_) the class definition, in order to be allocated some storage.
// Otherwise, you get link errors.

wxChar Internat::mDecimalSeparator = wxT('.'); // default
// exclude is used by SanitiseFilename.
wxArrayString Internat::exclude;

// DA: Use tweaked translation mechanism to replace 'Audacity' by 'DarkAudacity'.
#ifdef EXPERIMENTAL_DA
// This function allows us to replace Audacity by DarkAudacity without peppering 
// the source code with changes.  We split out this step, the customisation, as 
// it is used on its own (without translation) in the wxTS macro.
STRINGS_API const wxString& GetCustomSubstitution(const wxString& str2)
{
   // If contains 'DarkAudacity, already converted.
   if( str2.Contains( "DarkAudacity" ))
      return str2;
   // If does not contain 'Audacity', nothing to do.
   if( !str2.Contains( "Audacity" ))
      return str2;
   wxString str3 = str2;
   str3.Replace( "Audacity", "DarkAudacity" );
   str3.Replace( " an DarkAudacity", " a DarkAudacity" );
   // DA also renames sync-lock(ed) as time-lock(ed).
   str3.Replace( "Sync-Lock", "Time-Lock" );
   str3.Replace( "Sync-&Lock", "Time-&Lock" );
   str3.Replace( "Sync Lock", "Time Lock" );
   return wxTranslations::GetUntranslatedString(str3);
}
#else 
STRINGS_API const wxString& GetCustomSubstitution(const wxString& str1)
{
   return str1 ;
}
#endif

// In any translated string, we can replace the name 'Audacity' by 'DarkAudacity'
// without requiring translators to see extra strings for the two versions.
STRINGS_API const wxString& GetCustomTranslation(const wxString& str1)
{
   const wxString& str2 = wxGetTranslation( str1 );
   return GetCustomSubstitution( str2 );
}


void Internat::Init()
{
   // Save decimal point character
   struct lconv * localeInfo = localeconv();
   if (localeInfo)
      mDecimalSeparator = wxString(wxSafeConvertMB2WX(localeInfo->decimal_point)).GetChar(0);

//   wxLogDebug(wxT("Decimal separator set to '%c'"), mDecimalSeparator);

   // Setup list of characters that aren't allowed in file names
   // Hey!  The default wxPATH_NATIVE does not do as it should.
#if defined(__WXMAC__)
   wxPathFormat format = wxPATH_MAC;
#elif defined(__WXGTK__)
   wxPathFormat format = wxPATH_UNIX;
#elif defined(__WXMSW__)
   wxPathFormat format = wxPATH_WIN;
#endif

   // This is supposed to return characters not permitted in paths to files
   // or to directories
   auto forbid = wxFileName::GetForbiddenChars(format);

   for (auto cc: forbid) {
#if defined(__WXGTK__)
      if (cc == wxT('*') || cc == wxT('?')) {
         continue;
      }
#endif
      exclude.push_back(wxString{ cc });
   }

   // The path separators may not be forbidden, so add them
   //auto separators = wxFileName::GetPathSeparators(format);

   // Bug 1441 exclude all separators from filenames on all platforms.
   auto separators = wxString("\\/");

   for(auto cc: separators) {
      if (forbid.Find(cc) == wxNOT_FOUND)
         exclude.push_back(wxString{ cc });
   }
}

void Internat::SetCeeNumberFormat()
{
   wxSetlocale( LC_NUMERIC, "C" );
   mDecimalSeparator = '.';
}


wxChar Internat::GetDecimalSeparator()
{
   return mDecimalSeparator;
}

bool Internat::CompatibleToDouble(const wxString& stringToConvert, double* result)
{
   // Regardless of the locale, always respect comma _and_ point
   wxString s = stringToConvert;
   // Convert to C locale decimal point for stable parsing.
   s.Replace(wxT(","), wxT("."));
   s.Replace(wxString(GetDecimalSeparator()), wxT("."));
   return s.ToCDouble(result);
}

double Internat::CompatibleToDouble(const wxString& stringToConvert)
{
   double result = 0;
   Internat::CompatibleToDouble(stringToConvert, &result);
   return result;
}

wxString Internat::ToString(double numberToConvert,
                     int digitsAfterDecimalPoint /* = -1 */)
{
   wxString result = ToDisplayString(
      numberToConvert, digitsAfterDecimalPoint);

   result.Replace(wxString(GetDecimalSeparator()), wxT("."));

   return result;
}

wxString Internat::ToDisplayString(double numberToConvert,
                                    int digitsAfterDecimalPoint /* = -1 */)
{
   wxString decSep = wxString(GetDecimalSeparator());
   wxString result;

   if (digitsAfterDecimalPoint == -1)
   {
      result.Printf(wxT("%f"), numberToConvert);

      // Not all libcs respect the decimal separator, so always convert
      // any dots found to the decimal separator.
      result.Replace(wxT("."), decSep);

      if (result.Find(decSep) != -1)
      {
         // Strip trailing zeros, but leave one, and decimal separator.
         int pos = result.length() - 1;
         while ((pos > 1) &&
                  (result.GetChar(pos) == wxT('0')) &&
                  (result.GetChar(pos - 1) != decSep))
            pos--;
         // (Previous code removed all of them and decimal separator.)
         //    if (result.GetChar(pos) == decSep)
         //       pos--; // strip point before empty fractional part
         result = result.Left(pos+1);
      }
   }
   else
   {
      wxString format;
      format.Printf(wxT("%%.%if"), digitsAfterDecimalPoint);
      result.Printf(format, numberToConvert);

      // Not all libcs respect the decimal separator, so always convert
      // any dots found to the decimal separator
      result.Replace(wxT("."), decSep);
   }

   return result;
}

TranslatableString Internat::FormatSize(wxLongLong size)
{
   /* wxLongLong contains no built-in conversion to double */
   double dSize = size.GetHi() * pow(2.0, 32);  // 2 ^ 32
   dSize += size.GetLo();

   return FormatSize(dSize);
}

TranslatableString Internat::FormatSize(double size)
{
   TranslatableString sizeStr;

   if (size == -1)
      sizeStr = XO("Unable to determine");
   else {
      /* make it look nice, by formatting into k, MB, etc */
      if (size < 1024.0)
         sizeStr = XO("%s bytes").Format( ToDisplayString(size) );
      else if (size < 1024.0 * 1024.0) {
         /* i18n-hint: Abbreviation for Kilo bytes */
         sizeStr = XO("%s KB").Format( ToDisplayString(size / 1024.0, 1) );
      }
      else if (size < 1024.0 * 1024.0 * 1024.0) {
         /* i18n-hint: Abbreviation for Mega bytes */
         sizeStr = XO("%s MB").Format( ToDisplayString(size / (1024.0 * 1024.0), 1) );
      }
      else {
         /* i18n-hint: Abbreviation for Giga bytes */
         sizeStr = XO("%s GB").Format( ToDisplayString(size / (1024.0 * 1024.0 * 1024.0), 1) );
      }
   }

   return sizeStr;
}

bool Internat::SanitiseFilename(wxString &name, const wxString &sub)
{
   bool result = false;
   for(const auto &item : exclude)
   {
      if(name.Contains(item))
      {
         name.Replace(item, sub);
         result = true;
      }
   }

#ifdef __WXMAC__
   // Special Mac stuff
   // '/' is permitted in file names as seen in dialogs, even though it is
   // the path separator.  The "real" filename as seen in the terminal has ':'.
   // Do NOT return true if this is the only substitution.
   name.Replace(wxT("/"), wxT(":"));
#endif

   return result;
}
