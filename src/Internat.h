/**********************************************************************

  Audacity: A Digital Audio Editor

  Internat.h

  Markus Meyer
  Dominic Mazzoni (Mac OS X code)

**********************************************************************/

#ifndef __AUDACITY_INTERNAT__
#define __AUDACITY_INTERNAT__

#include <wx/arrstr.h>
#include <wx/string.h>
#include <wx/longlong.h>

class Internat
{
public:
   /** \brief Initialize internationalisation support. Call this once at
    * program start. */
   static void Init();

   /** \brief Get the decimal separator for the current locale.
    *
    * Normally, this is a decimal point ('.'), but e.g. Germany uses a
    * comma (',').*/
   static wxChar GetDecimalSeparator();

   /** \brief Convert a string to a number.
    *
    * This function will accept BOTH point and comma as a decimal separator,
    * regardless of the current locale.
    * Returns 'true' on success, and 'false' if an error occurs. */
   static bool CompatibleToDouble(const wxString& stringToConvert, double* result);

   // Function version of above.
   static double CompatibleToDouble(const wxString& stringToConvert);

   /** \brief Convert a number to a string, always uses the dot as decimal
    * separator*/
   static wxString ToString(double numberToConvert,
                     int digitsAfterDecimalPoint = -1);

   /** \brief Convert a number to a string, uses the user's locale's decimal
    * separator */
   static wxString ToDisplayString(double numberToConvert,
                     int digitsAfterDecimalPoint = -1);

   /** \brief Convert a number to a string while formatting it in bytes, KB,
    * MB, GB */
   static wxString FormatSize(wxLongLong size);
   static wxString FormatSize(double size);

   /** \brief Protect against Unicode to multi-byte conversion failures
    * on Windows */
#if defined(__WXMSW__)
   static char *VerifyFilename(const wxString &s, bool input = true);
#endif

   /** \brief Check a proposed file name string for illegal characters and
    * remove them
    * return true iff name is "visibly" changed (not necessarily equivalent to
    * character-wise changed)
    */
   static bool SanitiseFilename(wxString &name, const wxString &sub);

   /** \brief Remove accelerator charactors from strings
    *
    * Utility function - takes a translatable string to be used as a menu item,
    * for example _("&Splash...\tAlt+S"), and strips all of the menu
    * accelerator stuff from it, to make "Splash".  That way the same
    * translatable string can be used both when accelerators are needed and
    * when they aren't, saving translators effort. */
   static wxString StripAccelerators(const wxString& str);

   static wxString Parenthesize(const wxString &str);

   static const wxArrayString &GetExcludedCharacters()
   { return exclude; }

private:
   static wxChar mDecimalSeparator;

   // stuff for file name sanitisation
   static wxArrayString exclude;

   static wxCharBuffer mFilename;
};

#define _NoAcc(X) Internat::StripAccelerators(_(X))

// Use this macro to wrap all filenames and pathnames that get
// passed directly to a system call, like opening a file, creating
// a directory, checking to see that a file exists, etc...
#if defined(__WXMSW__)
// Note, on Windows we don't define an OSFILENAME() to prevent accidental use.
// See VerifyFilename() for an explanation.
#define OSINPUT(X) Internat::VerifyFilename(X, true)
#define OSOUTPUT(X) Internat::VerifyFilename(X, false)
#elif defined(__WXMAC__)
#define OSFILENAME(X) ((char *) (const char *)(X).fn_str())
#define OSINPUT(X) OSFILENAME(X)
#define OSOUTPUT(X) OSFILENAME(X)
#else
#define OSFILENAME(X) ((char *) (const char *)(X).mb_str())
#define OSINPUT(X) OSFILENAME(X)
#define OSOUTPUT(X) OSFILENAME(X)
#endif

// Convert C strings to wxString
#define UTF8CTOWX(X) wxString((X), wxConvUTF8)
#define LAT1CTOWX(X) wxString((X), wxConvISO8859_1)

#endif
