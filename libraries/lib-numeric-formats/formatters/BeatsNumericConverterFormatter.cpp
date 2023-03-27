/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file BeatsNumericConverterFormatter.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "BeatsNumericConverterFormatter.h"

#include <algorithm>
#include <array>
#include <cmath>

#include "NumericConverterRegistry.h"

#include "SampleCount.h"

namespace
{

class BeatsFormatter final :
    public NumericConverterFormatter
{
public:
   BeatsFormatter(
      double tempo, int upperTimeSignature, int lowerTimeSignature,
      int fracPart)
       : mTempo { tempo }
       , mUpperTimeSignature { upperTimeSignature }
       , mLowerTimeSignature { lowerTimeSignature }
       , mFracPart { fracPart }
   {
      mFields.reserve(mFracPart > 0 ? 3 : 2);

      // Range is assumed to allow 999 bars.
      auto& barsField = mFields.emplace_back(NumericField { 1000, true });
      barsField.label = L" " + XO("bar").Translation() + L" ";

      // Beats format is 1 based. For the time point "0" the expected output is "1 bar 1 beat [1]"
      // For this reason we use (uts + 1) as the "range".
      // On top of that, we want at least two digits to be shown. Audacity has a bug with the range of 11,
      // where on digit will be calculated instead of 2.

      auto& beatsField = mFields.emplace_back(
         NumericField { std::max(12, mUpperTimeSignature + 1), true });
      beatsField.label = L" " + XO("beat").Translation();

      if (mFracPart > 0)
      {
         beatsField.label += L" ";
         // See the reasoning above about the range
         auto& fracField = mFields.emplace_back(
            NumericField { std::max(12, mFracPart / mLowerTimeSignature + 1), true });
      }

      // Fill the aux mDigits structure
      size_t pos = 0;
      for (size_t i = 0; i < mFields.size(); i++)
      {
         mFields[i].pos = pos;

         for (size_t j = 0; j < mFields[i].digits; j++)
         {
            mDigits.push_back(DigitInfo { i, j, pos });
            pos++;
         }

         pos += mFields[i].label.length();
      }

      // 1/4 = BPM is used for now
      const auto quarterLength = 60.0 / mTempo;
      const auto beatLength = quarterLength * 4.0 / mLowerTimeSignature;
      const auto barLength = mUpperTimeSignature * beatLength;
      const auto fracLength = beatLength * mLowerTimeSignature / mFracPart;

      mFieldLengths[0] = barLength;
      mFieldLengths[1] = beatLength;
      mFieldLengths[2] = fracLength;
   }

   void UpdateResultString(ConversionResult& result) const
   {
      for (size_t fieldIndex = 0; fieldIndex < mFields.size(); ++fieldIndex)
      {
         result.valueString +=
            result.fieldValueStrings[fieldIndex] + mFields[fieldIndex].label;
      }
   }

   ConversionResult ValueToString(double value, bool) const override
   {
      ConversionResult result;
      result.fieldValueStrings.resize(mFields.size());

      if (value < 0)
      {
         for (size_t fieldIndex = 0; fieldIndex < mFields.size (); ++fieldIndex)
         {
            const auto digitsCount = mFields[fieldIndex].digits;
            auto& fieldValue = result.fieldValueStrings[fieldIndex];
            for (int digitIndex = 0; digitIndex < digitsCount; ++digitIndex)
               fieldValue += L"-";
         }

         UpdateResultString(result);

         return result;
      }

      for (size_t fieldIndex = 0; fieldIndex < mFields.size(); ++fieldIndex)
      {
         const auto fieldLength = mFieldLengths[fieldIndex];
         const auto fieldValue = static_cast<int>(std::floor(value / fieldLength));

         result.fieldValueStrings[fieldIndex] =
            wxString::Format(mFields[fieldIndex].formatStr, fieldValue + 1);

         value = value - fieldValue * fieldLength;
      }

      UpdateResultString(result);
      return result;
   }

   std::optional<double> StringToValue(const wxString& valueString) const override
   {
      if (
         mFields.size() > 0 &&
         valueString.Mid(mFields[0].pos, 1) == wxChar('-'))
         return std::nullopt;

      double t = 0.0;

      for (size_t i = 0; i < mFields.size(); i++)
      {
         const auto pos = mFields[i].pos;
         const auto digits = mFields[i].digits;

         if (pos >= valueString.size() || pos + digits > valueString.size())
            return std::nullopt;

         long val;

         const auto fieldStringValue =
            valueString.Mid(mFields[i].pos, mFields[i].digits);

         if (!fieldStringValue.ToLong(&val))
            return std::nullopt;

         t += (val - 1) * mFieldLengths[i];
      }

      return t;
   }

   double SingleStep(double value, int digitIndex, bool upwards) const override
   {
      if (digitIndex < 0 || size_t(digitIndex) >= mDigits.size())
         return value;

      const auto& digit = mDigits[digitIndex];
      const auto& fieldIndex = digit.field;
      const auto& field = mFields[fieldIndex];

      const auto stepSize = mFieldLengths[fieldIndex] *
                            std::pow(10, field.digits - digit.index - 1);

      return upwards ? value + stepSize : value - stepSize;
   }

private:
   double mTempo;

   int mUpperTimeSignature;
   int mLowerTimeSignature;

   int mFracPart;

   std::array<double, 3> mFieldLengths;
};

bool IsValidTimeSignatureDenom (int value)
{
   return value >= 1 && ((value & (value - 1)) == 0);
}

NumericConverterItemRegistrator beatsTime
{
   Registry::Placement { {}, { Registry::OrderingHint::After, L"parsedTime" } },
      NumericConverterFormatterGroup(
         "beats", NumericConverterType_TIME,
         NumericConverterFormatterItem(
            "beats", XO("beats"),
         [](const auto& config)
         {
            return CreateBeatsNumericConverterFormatter(
               config.tempo, config.upperTimeSignature,
               config.lowerTimeSignature);
         }),
      NumericConverterFormatterItem(
         "beats16", XO("beats and 16th"),
         [](const auto& config)
         {
            return CreateBeatsNumericConverterFormatter(
               config.tempo, config.upperTimeSignature,
               config.lowerTimeSignature, 16);
         }))
};
} // namespace

std::unique_ptr<NumericConverterFormatter>
CreateBeatsNumericConverterFormatter(
   double tempo, int upperTimeSignature, int lowerTimeSignature,
   int fracPart /*= 0*/)
{
   if (tempo <= 0 ||
      upperTimeSignature < 1 ||
      !IsValidTimeSignatureDenom(lowerTimeSignature))
      return {};

   // Fraction should be "less" than beat size, i. e.
   // 1 / fracPart < 1 / lts.
   // Otherwise, we will pass 0 to the formatter and no fraction will be shown.
   if (fracPart < lowerTimeSignature)
      fracPart = 0;
   else if (!IsValidTimeSignatureDenom(fracPart))
      return {};

   return std::make_unique<BeatsFormatter>(
      tempo, upperTimeSignature, lowerTimeSignature, fracPart);
}
