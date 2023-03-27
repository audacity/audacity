/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterFormatter.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "NumericConverterFormatter.h"

#include <cmath>

namespace
{
size_t CalculateDigits(size_t rangeEnd)
{
   if (rangeEnd == 0)
      return 0;

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

NumericField::NumericField(size_t _range, bool _zeropad) noexcept
    : range { _range }
    , zeropad { _zeropad }
{
   CreateDigitFormatStr();
}

void NumericField::CreateDigitFormatStr()
{
   if (range > 1)
      digits = CalculateDigits(range);
   else
      digits = 5; // hack: default

   if (zeropad && range > 1)
      formatStr.Printf(wxT("%%0%zud"), digits); // ex. "%03d" if digits is 3
   else
      formatStr = "%d";
}

NumericConverterFormatter::~NumericConverterFormatter()
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
