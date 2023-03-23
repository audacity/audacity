/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterFormatter.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include <optional>
#include <vector>

#include <wx/string.h>

struct NUMERIC_FORMATS_API NumericField final
{
public:
   NumericField(bool _frac, int _base, int _range, bool _zeropad) noexcept;

   NumericField(const NumericField&) = default;
   NumericField& operator=(const NumericField&) = default;
   // NumericField( NumericField && ) = default;
   // NumericField &operator = ( NumericField && ) = default;

   bool frac; // is it a fractional field
   int base;  // divide by this (multiply, after decimal point)
   int range; // then take modulo this
   int digits { 0 };

   wxString label;
   wxString formatStr;

   int pos { -1 }; // Index of this field in the ValueString
   bool zeropad;

private:
   void CreateDigitFormatStr();
};

using NumericFields = std::vector<NumericField>;

struct NUMERIC_FORMATS_API DigitInfo final
{
   int field; // Which field
   int index; // Index of this digit within the field
   int pos;   // Position in the ValueString
};

using DigitInfos = std::vector<DigitInfo>;

struct NUMERIC_FORMATS_API NumericConverterFormatter /* not final */
{
   virtual ~NumericConverterFormatter();

   struct NUMERIC_FORMATS_API ConversionResult final
   {
      wxString valueString;
      std::vector<wxString> fieldValueStrings;
   };

   //! @post result: `GetFields().size() == result.fieldValueStrings.size()`
   virtual ConversionResult
   ValueToString(double value, bool nearest) const = 0;

   virtual std::optional<double>
   StringToValue(const wxString& value) const = 0;

   virtual double SingleStep(double value, int digitIndex, bool upwards) const = 0;

   const wxString& GetPrefix() const;
   const NumericFields& GetFields() const;
   const DigitInfos& GetDigitInfos() const;

protected:
   wxString mPrefix;

   NumericFields mFields;
   DigitInfos mDigits;
};
