/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterFormatter.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "NumericConverterFormatter.h"

#include <cmath>

NumericField::NumericField(int _range, bool _zeropad) noexcept
    : range { _range }
    , zeropad { _zeropad }
{
   CreateDigitFormatStr();
}

void NumericField::CreateDigitFormatStr()
{
   if (range > 1)
      digits = (int)ceil(log10(range - 1.0));
   else
      digits = 5; // hack: default

   if (zeropad && range > 1)
      formatStr.Printf(wxT("%%0%dd"), digits); // ex. "%03d" if digits is 3
   else
   {
      formatStr.Printf(wxT("%%0%dd"), digits);
   }
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
