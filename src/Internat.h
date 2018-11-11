/**********************************************************************

  Audacity: A Digital Audio Editor

  Internat.h

  Markus Meyer
  Dominic Mazzoni (Mac OS X code)

**********************************************************************/

#ifndef __AUDACITY_INTERNAT__
#define __AUDACITY_INTERNAT__

#include "Audacity.h"

#include <wx/longlong.h>

#include <algorithm>

#ifndef IN_RC
#include "audacity/Types.h"

class wxArrayString;
class wxArrayStringEx;
class wxString;

extern AUDACITY_DLL_API const wxString& GetCustomTranslation(const wxString& str1 );
extern AUDACITY_DLL_API const wxString& GetCustomSubstitution(const wxString& str1 );

// Marks string for substitution only.
#define _TS( s ) GetCustomSubstitution( s )

// Marks strings for extraction only...must use wxGetTranslation() to translate.
#define XO(s)  wxT(s)

#ifdef _
   #undef _
#endif

#if defined( __WXDEBUG__ )
   // Force a crash if you misuse _ in a static initializer, so that translation
   // is looked up too early and not found.

   #ifdef _MSC_VER

   #define _(s) ((wxTranslations::Get() || (DebugBreak(), true)), \
                GetCustomTranslation((s)))

   #else

   #include <signal.h>
   // Raise a signal because it's even too early to use wxASSERT for this.
   #define _(s) ((wxTranslations::Get() || raise(SIGTRAP)), \
                GetCustomTranslation((s)))

   #endif

#else
   #define _(s) GetCustomTranslation((s))
#endif

#ifdef wxPLURAL
   #undef wxPLURAL
#endif


// The two string arugments will go to the .pot file, as
// msgid sing
// msgid_plural plural
//
// (You must use plain string literals.  Do not use _() or wxT() or L prefix,
//  which (intentionally) will fail to compile.  The macro inserts wxT).
//
// Note too:  it seems an i18n-hint comment is not extracted if it precedes
// wxPLURAL directly.  A workaround:  after the comment, insert a line
// _("dummyStringXXXX");
// where for XXXX subsitute something making this dummy string unique in the
// program.  Then check in your generated audacity.pot that the dummy is
// immediately before the singular/plural entry.
//
// Your i18n-comment should therefore say something like,
// "In the string after this one, ..."
#define wxPLURAL(sing, plur, n)  wxGetTranslation( wxT(sing), wxT(plur), n)

#endif

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

class ComponentInterfaceSymbol;
wxArrayStringEx LocalizedStrings(
   const EnumValueSymbol strings[], size_t nStrings);

// This object pairs an internal string, maybe empty, with a translated string.
// Any internal string may be written to configuration or other files and,
// for compatibility, should not vary between Audacity versions.
// The translated string may be shown to users and may vary with locale, and
// Audacity version if it is decided to use a different user-visible message.
// Sometimes the translated string is derived from a msgid identical
// to the internal string.  The translated string is not meant to persist.
class TranslatedInternalString
{
public:

   using ID = CommandID;

   TranslatedInternalString() =  default;

   // One-argument constructor from a msgid
   explicit TranslatedInternalString( const wxString &internal )
   : mInternal{ internal }, mTranslated{ GetCustomTranslation( internal ) }
   {}

   // Two-argument version, when translated does not derive from internal
   TranslatedInternalString( const ID &internal,
                             const wxString &translated )
   : mInternal{ internal }, mTranslated{ translated }
   {}

   const ID &Internal() const { return mInternal; }
   const wxString Translated() const 
   {  
      wxString Temp = mTranslated;
      Temp.Replace( "&","" );
      return Temp;
   }

private:
   ID mInternal;
   wxString mTranslated;
};

#endif
