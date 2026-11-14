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

// Legacy XO/XC/XXO/XP family. The macros wrap an untranslatable string, so
// any surviving call site is NOT translated. Two kinds of code still use them:
//   - effect-host libs not yet migrated (vst, lv2, ladspa, ...) -- these
//     should be rewritten to explicit ::TranslatableString("ctx", s) so
//     lupdate can extract the catalogue key;
//   - legacy wx-based UI (wx dialogs, ShuttleGui, ...) that au4 no longer
//     uses and will not be migrated.
#define XO(s)        ::TranslatableString::untranslatable(s)
#define XC(s, c)     ::TranslatableString::untranslatable(s)
#define XXO(s)       XO(s)
#define XXC(s, c)    XC(s, c)
#define XP(sing, plur, n)        ::TranslatableString::untranslatable(sing)
#define XPC(sing, plur, n, c)    ::TranslatableString::untranslatable(sing)

// Legacy Verbatim() free function. Wraps a string as a no-context
// TranslatableString. Overloaded for the actual call-site types.
inline ::TranslatableString Verbatim(const char* s)
{
    return ::TranslatableString::untranslatable(s);
}
inline ::TranslatableString Verbatim(const wxString& s)
{
    return au3::untranslatable(s);
}
inline ::TranslatableString Verbatim(const std::string& s)
{
    return ::TranslatableString::untranslatable(QString::fromStdString(s));
}

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
    static ::TranslatableString FormatSize(wxLongLong size);
    static ::TranslatableString FormatSize(double size);

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
