/////////////////////////////////////////////////////////////////////////////
//
// Backport from wxWidgets-3.0-rc1
//
/////////////////////////////////////////////////////////////////////////////
// Name:        src/common/numformatter.cpp
// Purpose:     NumberFormatter
// Author:      Fulvio Senore, Vadim Zeitlin
// Created:     2010-11-06
// Copyright:   (c) 2010 wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// ----------------------------------------------------------------------------
// headers
// ----------------------------------------------------------------------------


#include "numformatter.h"

// For compilers that support precompilation, includes "wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifdef __BORLANDC__
    #pragma hdrstop
#endif

#ifdef _WIN32
    #include <wx/msw/private.h>

#endif


#include "Internat.h"
#include <wx/intl.h>

#include <locale.h> // for setlocale and LC_ALL
#include <cmath>
#include <limits>
#include <wx/log.h>

// ----------------------------------------------------------------------------
// local helpers
// ----------------------------------------------------------------------------

// ============================================================================
// NumberFormatter implementation
// ============================================================================

// ----------------------------------------------------------------------------
// Locale information accessors
// ----------------------------------------------------------------------------

wxChar NumberFormatter::GetDecimalSeparator()
{
#if wxUSE_INTL
   struct lconv *info = localeconv();
   wxString s = info ? wxString::FromUTF8(info->decimal_point) : wxString(".");
   if (s.empty())
   {
      // We really must have something for decimal separator, so fall
      // back to the C locale default.
      s = wxT(".");
   }

   return s[0];
#else // !wxUSE_INTL
   return wxT('.');
#endif // wxUSE_INTL/!wxUSE_INTL
}

bool NumberFormatter::GetThousandsSeparatorIfUsed(wxChar *sep)
{
#if wxUSE_INTL
   struct lconv *info = localeconv();
   wxString s = info ? wxString::FromUTF8(info->thousands_sep) : wxString{};

   if (s.empty())
   {
      return false;
   }

   *sep = s[0];
   return true;
#else // !wxUSE_INTL
    wxUnusedVar(sep);
    return false;
#endif // wxUSE_INTL/!wxUSE_INTL
}

// ----------------------------------------------------------------------------
// Conversion to string and helpers
// ----------------------------------------------------------------------------

wxString NumberFormatter::PostProcessIntString(const wxString &sArg, int style)
{
   wxString s(sArg);

    if ( style & Style_WithThousandsSep )
        AddThousandsSeparators(s);

    wxASSERT_MSG( !(style & Style_NoTrailingZeroes),
                  wxT("Style_NoTrailingZeroes can't be used with integer values") );
    wxASSERT_MSG( !(style & Style_OneTrailingZero),
                  wxT("Style_OneTrailingZero can't be used with integer values") );
    wxASSERT_MSG( !(style & Style_TwoTrailingZeroes),
                  wxT("Style_TwoTrailingZeroes can't be used with integer values") );
    wxASSERT_MSG( !(style & Style_ThreeTrailingZeroes),
                  wxT("Style_ThreeTrailingZeroes can't be used with integer values") );

    return s;
}

wxString NumberFormatter::ToString(long val, int style)
{
    return PostProcessIntString(wxString::Format(wxT("%ld"), val), style);
}

#ifdef HAS_LONG_LONG_T_DIFFERENT_FROM_LONG

wxString NumberFormatter::ToString(wxLongLong_t val, int style)
{
   return PostProcessIntString(wxString::Format("%" wxLongLongFmtSpec "d", val),
                                style);
}

#endif // HAS_LONG_LONG_T_DIFFERENT_FROM_LONG

wxString NumberFormatter::ToString(double val, int precision, int style)
{
    wxString format;
    if ( precision == -1 )
    {
        format = wxT("%g");
    }
    else // Use fixed precision.
    {
        format.Printf(wxT("%%.%df"), precision);
    }

    if (std::isnan(val))
    {
        return _("NaN");
    }
    if (std::isinf(val))
    {
       if (val == std::numeric_limits<double>::infinity())
       {
          return _("Infinity");
       }
       else
       {
          return _("-Infinity");
       }
    }
    wxString s = wxString::Format(format, val);

    if ( style & Style_WithThousandsSep )
        AddThousandsSeparators(s);

    if ( precision != -1 )
    {
        if ( style & Style_NoTrailingZeroes )
            RemoveTrailingZeroes(s, 0);

        if ( style & Style_OneTrailingZero )
            RemoveTrailingZeroes(s, 1);

        if ( style & Style_TwoTrailingZeroes )
            RemoveTrailingZeroes(s, 2);

        if ( style & Style_ThreeTrailingZeroes )
            RemoveTrailingZeroes(s, 3);
    }
    return s;
}

void NumberFormatter::AddThousandsSeparators(wxString& s)
{
    wxChar thousandsSep;
    if ( !GetThousandsSeparatorIfUsed(&thousandsSep) )
        return;

    size_t pos = s.find(GetDecimalSeparator());
    if ( pos == wxString::npos )
    {
        // Start grouping at the end of an integer number.
        pos = s.length();
    }

    // End grouping at the beginning of the digits -- there could be at a sign
    // before their start.
    const size_t start = s.find_first_of(wxT("0123456789"));

    // We currently group digits by 3 independently of the locale. This is not
    // the right thing to do and we should use lconv::grouping (under POSIX)
    // and GetLocaleInfo(LOCALE_SGROUPING) (under MSW) to get information about
    // the correct grouping to use. This is something that needs to be done at
    // wxLocale level first and then used here in the future (TODO).
    const size_t GROUP_LEN = 3;

    while ( pos > start + GROUP_LEN )
    {
        pos -= GROUP_LEN;
        s.insert(pos, thousandsSep);
    }
}

void NumberFormatter::RemoveTrailingZeroes(wxString& s, size_t retain /* = 0 */)
{
   const size_t posDecSep = s.find(GetDecimalSeparator());
   wxCHECK_RET( posDecSep != wxString::npos,
               wxString::Format(wxT("No decimal separator in \"%s\""), s) );
   wxCHECK_RET( posDecSep, wxT("Can't start with decimal separator" ));

   // Find the last character to keep.
   size_t posLastCharacterToKeep = s.find_last_not_of(wxT("0"));

   // If it's the decimal separator itself, remove it.
   if ((posLastCharacterToKeep == posDecSep) && (retain == 0)) {
      posLastCharacterToKeep--;
   } else if ((posLastCharacterToKeep - posDecSep) < retain) {
      posLastCharacterToKeep = retain + posDecSep;
   }

   s.erase(posLastCharacterToKeep + 1);
}

// ----------------------------------------------------------------------------
// Conversion from strings
// ----------------------------------------------------------------------------

void NumberFormatter::RemoveThousandsSeparators(wxString& s)
{
    wxChar thousandsSep;
    if ( !GetThousandsSeparatorIfUsed(&thousandsSep) )
        return;

    s.Replace(wxString(thousandsSep), wxString());
}

bool NumberFormatter::FromString(const wxString &sArg, long *val)
{
   wxString s(sArg);
   RemoveThousandsSeparators(s);
   return s.ToLong(val);
}

#ifdef HAS_LONG_LONG_T_DIFFERENT_FROM_LONG

bool NumberFormatter::FromString(const wxString &sArg, wxLongLong_t *val)
{
   wxString s(sArg);

   RemoveThousandsSeparators(s);
   return s.ToLongLong(val);
}

#endif // HAS_LONG_LONG_T_DIFFERENT_FROM_LONG

bool NumberFormatter::FromString(const wxString &sArg, double *val)
{
   wxString s(sArg);

   RemoveThousandsSeparators(s);
   return s.ToDouble(val);
}
