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

#include <wx/msgdlg.h>
#include <wx/log.h>
#include <wx/intl.h>
#include <wx/filename.h>

#include <locale.h>
#include <math.h> // for pow()

#include "Internat.h"
#include "FileDialog.h"

// in order for the static member variables to exist, they must appear here
// (_outside_) the class definition, in order to be allocated some storage.
// Otherwise, you get link errors.

wxChar Internat::mDecimalSeparator = wxT('.'); // default
wxString Internat::forbid;
wxArrayString Internat::exclude;
wxCharBuffer Internat::mFilename;

void Internat::Init()
{
   // Save decimal point character
   struct lconv * localeInfo = localeconv();
   if (localeInfo)
      mDecimalSeparator = wxString(localeInfo->decimal_point, wxConvLocal).GetChar(0);

//   wxLogDebug(wxT("Decimal separator set to '%c'"), mDecimalSeparator);

   // Setup list of characters that aren't allowed in file names
   wxFileName tmpFile;
   forbid = tmpFile.GetForbiddenChars();
   for(unsigned int i=0; i < forbid.Length(); i++)
      exclude.Add( forbid.Mid(i, 1) );
}

wxChar Internat::GetDecimalSeparator()
{
   return mDecimalSeparator;
}

bool Internat::CompatibleToDouble(const wxString& stringToConvert, double* result)
{
   // Regardless of the locale, always respect comma _and_ point
   wxString s = stringToConvert;
   s.Replace(wxT(","), wxString(GetDecimalSeparator()));
   s.Replace(wxT("."), wxString(GetDecimalSeparator()));
   return s.ToDouble(result);
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
         int pos = result.Length() - 1;
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

wxString Internat::FormatSize(wxLongLong size)
{
   /* wxLongLong contains no built-in conversion to double */
   double dSize = size.GetHi() * pow(2.0, 32);  // 2 ^ 32
   dSize += size.GetLo();

   return FormatSize(dSize);
}

wxString Internat::FormatSize(double size)
{
   wxString sizeStr;

   if (size == -1)
      sizeStr = _("Unable to determine");
   else {
      /* make it look nice, by formatting into k, MB, etc */
      if (size < 1024.0)
         sizeStr = ToDisplayString(size) + wxT(" ") + _("bytes");
      else if (size < 1024.0 * 1024.0) {
         /* i18n-hint: Abbreviation for Kilo bytes */
         sizeStr = ToDisplayString(size / 1024.0, 1) + wxT(" ") + _("KB");
      }
      else if (size < 1024.0 * 1024.0 * 1024.0) {
         /* i18n-hint: Abbreviation for Mega bytes */
         sizeStr = ToDisplayString(size / (1024.0 * 1024.0), 1) + wxT(" ") + _("MB");
      }
      else {
         /* i18n-hint: Abbreviation for Giga bytes */
         sizeStr = ToDisplayString(size / (1024.0 * 1024.0 * 1024.0), 1) + wxT(" ") + _("GB");
      }
   }

   return sizeStr;
}

#if defined(__WXMSW__)
//
// On Windows, wxString::mb_str() can return a NULL pointer if the
// conversion to multi-byte fails.  So, based on direction intent,
// returns a pointer to an empty string or prompts for a new name.
//
char *Internat::VerifyFilename(const wxString &s, bool input)
{
   static wxCharBuffer buf;
   wxString name = s;

   if (input) {
      if ((char *) (const char *)name.mb_str() == NULL) {
         name = wxEmptyString;
      }
   }
   else {
      wxFileName f(name);
      while ((char *) (const char *)name.mb_str() == NULL) {
         wxMessageBox(_("The specified filename could not be converted due to Unicode character use."));

         name = FileSelector(_("Specify New Filename:"),
                             NULL,
                             name,
                             f.GetExt(),
                             wxT("*.") + f.GetExt(),
                             wxFD_SAVE | wxRESIZE_BORDER,
                             wxGetTopLevelParent(NULL));
      }
   }

   mFilename = name.mb_str();

   return (char *) (const char *) mFilename;
}
#endif

wxString Internat::SanitiseFilename(const wxString &name, const wxString &sub)
{
   wxString temp = name;
   for(unsigned i=0; i<exclude.Count(); i++)
   {
      if(temp.Contains(exclude.Item(i)))
      {
         temp.Replace(exclude.Item(i),sub);
      }
   }
   return temp;
}

wxString Internat::StripAccelerators(const wxString &s)
{
   wxString result;
   result.Alloc(s.Length());
   for(size_t i = 0; i < s.Length(); i++) {
      if (s[i] == '\t')
         break;
      if (s[i] != '&' && s[i] != '.')
         result += s[i];
   }
   return result;
}
