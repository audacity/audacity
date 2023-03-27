/**********************************************************************

  Audacity: A Digital Audio Editor

  NumericConverter.cpp

  Dominic Mazzoni

  Paul Licameli split from NumericTextCtrl.cpp


********************************************************************//**

\class NumericConverter
\brief NumericConverter has all the time conversion and snapping
functionality that used to live in NumericTextCtrl.  The idea is to have
a GUI-less class which can do the conversions, so that we can use it
in sanpping without having a window created each time.

*//****************************************************************//**

\class BuiltinFormatString
\brief BuiltinFormatString is a structure used in the NumericTextCtrl
and holds both a descriptive name for the string format and a
wxPrintf inspired style format string, optimised for displaying time in
different formats.

**********************************************************************/
#include "NumericConverter.h"
#include "NumericConverterRegistry.h"
#include "SampleCount.h"
#include "Beats.h"

#include "formatters/ParsedNumericConverterFormatter.h"

#include <cmath>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/wx.h>

static const TranslatableString BuildBeatsFormat() {
   double bpm = BeatsPerMinute.Read();
   int uts = UpperTimeSignature.Read();
   int lts = LowerTimeSignature.Read();

   // Check that all data is positive
   if (bpm <= 0) return XO("Invalid tempo");
   if (uts <= 0 || lts <= 0) return XO("Invalid time signature");
   // Also check that the lower time signature is valid (power of 2)
   if (lts & (lts - 1)) return XO("Invalid time signature");

   return XO("01000 bars 0%d beats|%f").Format(
      uts, (((double)lts / 4) * bpm) / 60
   );
}

//
// ----------------------------------------------------------------------------
// NumericConverter Class
// ----------------------------------------------------------------------------
//
NumericFormatSymbol NumericConverter::DefaultSelectionFormat()
{ return L"hh:mm:ss + milliseconds"; }
NumericFormatSymbol NumericConverter::TimeAndSampleFormat()
{ return L"hh:mm:ss + samples"; }
NumericFormatSymbol NumericConverter::SecondsFormat()
{ return L"seconds"; }
NumericFormatSymbol NumericConverter::HoursMinsSecondsFormat()
{ return L"hh:mm:ss"; }
NumericFormatSymbol NumericConverter::HundredthsFormat()
{ return L"hh:mm:ss + hundredths"; }

NumericFormatSymbol NumericConverter::HertzFormat()
{ return L"Hz"; }

NumericFormatSymbol NumericConverter::DefaultFormat(NumericConverterType type)
{
   if (type == NumericConverterType_TIME)
      return DefaultSelectionFormat();
   else if (type == NumericConverterType_FREQUENCY)
      return HertzFormat();
   else if (type == NumericConverterType_BANDWIDTH)
      return L"octaves";
   else
      return DefaultSelectionFormat();
}

NumericFormatSymbol
NumericConverter::LookupFormat(NumericConverterType type, const wxString& id)
{
   if (id.empty() || !NumericConverterRegistry::Find(type, id))
      return DefaultFormat(type);

   return id;
}

NumericConverter::NumericConverter(NumericConverterType type,
                                   const NumericFormatSymbol & formatName,
                                   double value,
                                   double sampleRate)
{
   ResetMinValue();
   ResetMaxValue();

   mInvalidValue = -1.0;
   mType = type;
   mValue = value; // used in SetSampleRate, reassigned later

   SetSampleRate(sampleRate);
   SetTimeSignature(
      BeatsPerMinute.Read(), UpperTimeSignature.Read(),
      LowerTimeSignature.Read());
   SetFormatName(formatName);
   SetValue(value); // mValue got overridden to -1 in ControlsToValue(), reassign
}

void NumericConverter::ParseFormatString(
   const TranslatableString & untranslatedFormat)
{
   mFormatter =
      CreateParsedNumericConverterFormatter(mType, untranslatedFormat.Translation(), mSampleRate);
}

NumericConverter::~NumericConverter()
{
}

void NumericConverter::ValueToControls()
{
   ValueToControls(mValue);
}

void NumericConverter::ValueToControls(double rawValue, bool nearest /* = true */)
{
   if (!mFormatter)
      return;

   auto result = mFormatter->ValueToString(rawValue, nearest);

   mValueString = std::move(result.valueString);
   mFieldValueStrings = std::move(result.fieldValueStrings);
}

void NumericConverter::ControlsToValue()
{
   if (!mFormatter)
   {
      mValue = mInvalidValue;
      return;
   }

   auto result = mFormatter->StringToValue(mValueString);

   mValue = result.has_value() ?
               std::max(mMinValue, std::min(mMaxValue, *result)) :
               mInvalidValue;
}

bool NumericConverter::SetFormatName(const NumericFormatSymbol& formatName)
{
   if (mFormatSymbol == formatName && !formatName.empty())
      return false;

   const auto newFormat = LookupFormat(mType, formatName.Internal());

   if (mFormatSymbol == newFormat)
      return false;

   mFormatSymbol = newFormat;
   mCustomFormat = {};

   UpdateFormatter();

   return true;
}

NumericFormatSymbol NumericConverter::GetFormatName() const
{
   return mFormatSymbol;
}

bool NumericConverter::SetCustomFormat(const TranslatableString& customFormat)
{
   if (mCustomFormat == customFormat)
      return false;

   if (!CreateParsedNumericConverterFormatter(
          mType, customFormat.Translation(), mSampleRate))
      return false;

   mFormatSymbol = {};
   mCustomFormat = customFormat;

   UpdateFormatter();

   return true;
}

void NumericConverter::SetSampleRate(double sampleRate)
{
   mSampleRate = sampleRate;
   UpdateFormatter();
   ValueToControls();
   ControlsToValue();
}

void NumericConverter::SetTimeSignature(double tempo, int upper, int lower)
{
   mTempo = tempo;
   mUpperTimeSignature = upper;
   mLowerTimeSignature = lower;

   UpdateFormatter();
   ValueToControls();
   ControlsToValue();
}

void NumericConverter::SetValue(double newValue)
{
   mValue = newValue;
   ValueToControls();
   ControlsToValue();
}

void NumericConverter::SetMinValue(double minValue)
{
   mMinValue = minValue;
   if (mMaxValue < minValue)
      mMaxValue = minValue;
   if (mValue < minValue)
      SetValue(minValue);
}

void NumericConverter::ResetMinValue()
{
   mMinValue = 0.0;
}

void NumericConverter::SetMaxValue(double maxValue)
{
   mMaxValue = maxValue;
   if (mMinValue > maxValue) {
      mMinValue = maxValue;
   }
   if (mValue > maxValue)
      SetValue(maxValue);
}

void NumericConverter::ResetMaxValue()
{
   mMaxValue = std::numeric_limits<double>::max();
}

double NumericConverter::GetValue()
{
   ControlsToValue();
   return mValue;
}

wxString NumericConverter::GetString()
{
   ValueToControls();
   return mValueString;
}

int NumericConverter::GetSafeFocusedDigit(int focusedDigit) const noexcept
{
   if (focusedDigit < 0)
      return int(mFormatter->GetDigitInfos().size() - 1);
   else
      return std::clamp<int>(
         focusedDigit, 0, mFormatter->GetDigitInfos().size() - 1);
}

void NumericConverter::Increment(int focusedDigit)
{
   Adjust(1, 1, focusedDigit);
}

void NumericConverter::Decrement(int focusedDigit)
{
   Adjust(1, -1, focusedDigit);
}

bool NumericConverter::UpdateFormatter()
{
   if (!mFormatSymbol.empty())
   {
      auto formatterItem = NumericConverterRegistry::Find(mType, mFormatSymbol);

      if (formatterItem == nullptr)
      {
         assert(formatterItem != nullptr);
         return false;
      }

      mFormatter = formatterItem->factory({ mSampleRate, mTempo, mLowerTimeSignature, mUpperTimeSignature });
   }
   else if (!mCustomFormat.empty ())
   {
      auto formatter = CreateParsedNumericConverterFormatter(
         mType, mCustomFormat.Translation(), mSampleRate);

      if (formatter == nullptr)
      {
         assert(formatter != nullptr);
         return false;
      }

      mFormatter = std::move(formatter);
   }

   return mFormatter != nullptr;
}

void NumericConverter::Adjust(int steps, int dir, int focusedDigit)
{
   if (!mFormatter || mFormatter->GetDigitInfos().empty())
      return;
   // It is possible and "valid" for steps to be zero if a
   // high precision device is being used and wxWidgets supports
   // reporting a higher precision...Mac wx3 does.
   if (steps == 0)
      return;

   focusedDigit = GetSafeFocusedDigit(focusedDigit);

   wxASSERT(dir == -1 || dir == 1);
   wxASSERT(steps > 0);
   if (steps < 0)
      steps = -steps;

   while (steps != 0)
   {
      mValue = mFormatter->SingleStep(mValue, focusedDigit, dir > 0);
      steps--;
   }

   mValue = std::clamp(mValue, mMinValue, mMaxValue);

   ValueToControls();
}
