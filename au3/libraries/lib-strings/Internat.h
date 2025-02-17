/**********************************************************************

  Audacity: A Digital Audio Editor

  Internat.h

  Markus Meyer
  Dominic Mazzoni (Mac OS X code)

**********************************************************************/

#ifndef __AUDACITY_INTERNAT__
#define __AUDACITY_INTERNAT__

#include <vector>
#include <wx/longlong.h>

#include "TranslatableString.h"

class wxArrayString;
class wxArrayStringEx;

extern STRINGS_API const wxString& GetCustomTranslation(const wxString& str1);
extern STRINGS_API const wxString& GetCustomSubstitution(const wxString& str1);

// Marks string for substitution only.
#define _TS(s) GetCustomSubstitution(s)

// Marks strings for extraction only... use .Translate() to translate.
// '&', preceding menu accelerators, should NOT occur in the argument.
#define XO(s)  (TranslatableString{ wxT(s), {} })

// Alternative taking a second context argument.  A context is a string literal,
// which is not translated, but serves to disambiguate uses of the first string
// that might need differing translations, such as "Light" meaning not-heavy in
// one place but not-dark elsewhere.
#define XC(s, c)  (TranslatableString{ wxT(s), {} }.Context(c))

// Marks strings for extraction only, where '&', preceding menu accelerators, MAY
// occur.
// For now, expands exactly as macro XO does, but in future there will be a
// type distinction - for example XXO should be used for menu item names that
// might use the & character for shortcuts.
#define XXO(s)  XO(s)

// Corresponds to XC as XXO does to XO
#define XXC(s, c) XC(s, c)

#ifdef _
   #undef _
#endif

#if defined(_DEBUG)
// Force a crash if you misuse _ in a static initializer, so that translation
// is looked up too early and not found.

   #ifdef __WXMSW__

extern "C" __declspec(dllimport) void __stdcall DebugBreak();
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

#ifdef XP
   #undef XP
#endif

// The two string arguments will go to the .pot file, as
// msgid sing
// msgid_plural plural
//
// (You must use plain string literals.  Do not use _() or wxT() or L prefix,
//  which (intentionally) will fail to compile.  The macro inserts wxT).
//
// Note too:  The i18n-hint comment must be on the line preceding the first
// string.  That might be inside the parentheses of the macro call.
//
// The macro call is then followed by a sequence of format arguments in
// parentheses.  The third argument of the macro call is the zero-based index
// of the format argument that selects singular or plural
#define XP(sing, plur, n) \
    TranslatableString{ wxT(sing), {} }.Plural<(n)>(wxT(plur))

// Like XP but with an additional context argument, as for XC
#define XPC(sing, plur, n, c) \
    TranslatableString{ wxT(sing), {} }.Context(c).Plural<(n)>(wxT(plur))

class STRINGS_API Internat
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
    static void SetCeeNumberFormat();

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
    static wxString ToString(double numberToConvert, int digitsAfterDecimalPoint = -1);

    /** \brief Convert a number to a string, uses the user's locale's decimal
     * separator */
    static wxString ToDisplayString(double numberToConvert, int digitsAfterDecimalPoint = -1);

    /** \brief Convert a number to a string while formatting it in bytes, KB,
     * MB, GB */
    static TranslatableString FormatSize(wxLongLong size);
    static TranslatableString FormatSize(double size);

    /** \brief Check a proposed file name string for illegal characters and
     * remove them
     * return true iff name is "visibly" changed (not necessarily equivalent to
     * character-wise changed)
     */
    static bool SanitiseFilename(wxString& name, const wxString& sub);

    static const wxArrayString& GetExcludedCharacters()
    { return exclude; }

private:
    static wxChar mDecimalSeparator;

    static wxArrayString exclude;
};

// Convert C strings to wxString
#define UTF8CTOWX(X) wxString((X), wxConvUTF8)
#define LAT1CTOWX(X) wxString((X), wxConvISO8859_1)

// Whether disambiguationg contexts are supported
// If not, then the program builds and runs, but strings in the catalog with
// contexts will fail to translate
#define HAS_I18N_CONTEXTS wxCHECK_VERSION(3, 1, 1)

#endif
