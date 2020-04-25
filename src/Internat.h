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

#ifndef IN_RC
#include "audacity/Types.h"

class wxArrayString;
class wxArrayStringEx;

extern AUDACITY_DLL_API const wxString& GetCustomTranslation(const wxString& str1 );
extern AUDACITY_DLL_API const wxString& GetCustomSubstitution(const wxString& str1 );

// Marks string for substitution only.
#define _TS( s ) GetCustomSubstitution( s )

// Marks strings for extraction only... use .Translate() to translate.
#define XO(s)  (TranslatableString{ wxT(s), {} })
// XXO is used instead of XO in some places, for reasons that are
// no longer important.  The two are equivalent now.
#define XXO(s)  XO(s)

#ifdef _
   #undef _
#endif

#if defined( _DEBUG )
   // Force a crash if you misuse _ in a static initializer, so that translation
   // is looked up too early and not found.

   #ifdef __WXMSW__

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


// The two string arguments will go to the .pot file, as
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
//
// The macro call is then followed by a sequence of format arguments in
// parentheses.  The third argument of the macro call is the zero-based index
// of the format argument that selects singular or plural
#define wxPLURAL(sing, plur, n) \
   TranslatableString{ wxT(sing), {} }.Plural<(n)>( wxT(plur) )

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

   /** \brief Check a proposed file name string for illegal characters and
    * remove them
    * return true iff name is "visibly" changed (not necessarily equivalent to
    * character-wise changed)
    */
   static bool SanitiseFilename(wxString &name, const wxString &sub);

   /** \brief Remove accelerator characters from strings
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

   static wxArrayString exclude;
};

#define _NoAcc(X) Internat::StripAccelerators(_(X))

// Convert C strings to wxString
#define UTF8CTOWX(X) wxString((X), wxConvUTF8)
#define LAT1CTOWX(X) wxString((X), wxConvISO8859_1)

class ComponentInterfaceSymbol;
TranslatableStrings Msgids(
   const EnumValueSymbol strings[], size_t nStrings);
TranslatableStrings Msgids( const std::vector<EnumValueSymbol> &strings );

#endif
