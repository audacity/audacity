/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterFormatter.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "NumericConverterFormatter.h"

#include <cmath>

namespace {
size_t CalculateDigits(size_t rangeEnd)
{
    if (rangeEnd == 0) {
        return 0;
    }

    --rangeEnd;

    size_t digitsCount = 0;

    while (rangeEnd > 0)
    {
        rangeEnd /= 10;
        ++digitsCount;
    }

    return digitsCount;
}
}

NumericField::NumericField(size_t _digits, bool zeropad)
    : digits{_digits}
{
    if (zeropad && digits > 1) {
        formatStr.Printf(wxT("%%0%zud"), digits); // ex. "%03d" if digits is 3
    } else {
        formatStr = "%d";
    }
}

NumericField NumericField::ForRange(size_t range, bool zeropad, size_t minDigits)
{
    // Previously, Audacity used 5 digits by default (why?)
    return NumericField(
        range > 1 ? std::max(minDigits, CalculateDigits(range)) : 5, zeropad);
}

NumericField NumericField::WithDigits(size_t digits, bool zeropad)
{
    return NumericField(digits, zeropad);
}

NumericConverterFormatter::~NumericConverterFormatter()
{
}

void NumericConverterFormatter::UpdateFormatForValue(double, bool)
{
}

const wxString& NumericConverterFormatter::GetPrefix() const
{
    return mPrefix;
}

const NumericFields& NumericConverterFormatter::GetFields() const
{
    return mFields;
}

const DigitInfos& NumericConverterFormatter::GetDigitInfos() const
{
    return mDigits;
}
