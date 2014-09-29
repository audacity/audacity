/////////////////////////////////////////////////////////////////////////////
//
// Backport from wxWidgets-3.0-rc1
//
/////////////////////////////////////////////////////////////////////////////
// Name:        wx/numformatter.h
// Purpose:     wxNumberFormatter class
// Author:      Fulvio Senore, Vadim Zeitlin
// Created:     2010-11-06
// Copyright:   (c) 2010 wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#ifndef _WX_NUMFORMATTER_H_
#define _WX_NUMFORMATTER_H_

#include "wx/string.h"

#define wxHAS_LONG_LONG_T_DIFFERENT_FROM_LONG 1

// Helper class for formatting numbers with thousands separators which also
// supports parsing the numbers formatted by it.
class wxNumberFormatter
{
public:
    // Bit masks for ToString()
    enum Style
    {
        Style_None                = 0x00,
        Style_WithThousandsSep    = 0x01,
        Style_NoTrailingZeroes    = 0x02,     // Only for floating point numbers
        Style_OneTrailingZero     = 0x04,     // Only for floating point numbers
        Style_TwoTrailingZeroes   = 0x08,     // Only for floating point numbers
        Style_ThreeTrailingZeroes = 0x10      // Only for floating point numbers
    };

    // Format a number as a string. By default, the thousands separator is
    // used, specify Style_None to prevent this. For floating point numbers,
    // precision can also be specified.
    static wxString ToString(long val,
                             int style = Style_WithThousandsSep);
#ifdef wxHAS_LONG_LONG_T_DIFFERENT_FROM_LONG
    static wxString ToString(wxLongLong_t val,
                             int style = Style_WithThousandsSep);
#endif // wxHAS_LONG_LONG_T_DIFFERENT_FROM_LONG
    static wxString ToString(double val,
                             int precision,
                             int style = Style_WithThousandsSep);

    // Parse a string representing a number, possibly with thousands separator.
    //
    // Return true on success and stores the result in the provided location
    // which must be a valid non-NULL pointer.
    static bool FromString(wxString s, long *val);
#ifdef wxHAS_LONG_LONG_T_DIFFERENT_FROM_LONG
    static bool FromString(wxString s, wxLongLong_t *val);
#endif // wxHAS_LONG_LONG_T_DIFFERENT_FROM_LONG
    static bool FromString(wxString s, double *val);


    // Get the decimal separator for the current locale. It is always defined
    // and we fall back to returning '.' in case of an error.
    static wxChar GetDecimalSeparator();

    // Get the thousands separator if grouping of the digits is used by the
    // current locale. The value returned in sep should be only used if the
    // function returns true.
    static bool GetThousandsSeparatorIfUsed(wxChar *sep);

private:
    // Post-process the string representing an integer.
    static wxString PostProcessIntString(wxString s, int style);

    // Add the thousands separators to a string representing a number without
    // the separators. This is used by ToString(Style_WithThousandsSep).
    static void AddThousandsSeparators(wxString& s);

    // Remove trailing zeroes and, if there is nothing left after it, the
    // decimal separator itself from a string representing a floating point
    // number. Also used by ToString().
    static void RemoveTrailingZeroes(wxString& s, size_t retain = 0);

    // Remove all thousands separators from a string representing a number.
    static void RemoveThousandsSeparators(wxString& s);
};

#endif // _WX_NUMFORMATTER_H_
