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
#include "SampleCount.h"
#include "Beats.h"

#include "formatters/ParsedNumericConverterFormatter.h"

#include <cmath>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/wx.h>

//
// ----------------------------------------------------------------------------
// BuiltinFormatString Struct
// ----------------------------------------------------------------------------
//
/** \brief struct to hold a formatting control string and its user facing name
 * Used in an array to hold the built-in time formats that are always available
 * to the user */
struct BuiltinFormatString
{
   NumericFormatSymbol name;
   NumericConverter::FormatStrings formatStrings;

   friend inline bool operator ==
      (const BuiltinFormatString &a, const BuiltinFormatString &b)
         { return a.name == b.name; }
};

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

namespace {

/** \brief array of formats the control knows about internally
 *  array of string pairs for name of the format and the format string
 *  needed to create that format output. This is used for the pop-up
 *  list of formats to choose from in the control.          */
static BuiltinFormatString TimeConverterFormats_[] =  {
   {
   /* i18n-hint: Name of time display format that shows time in seconds */
   { XO("seconds") },
   /* i18n-hint: Format string for displaying time in seconds. Change the comma
    * in the middle to the 1000s separator for your locale, and the 'seconds'
    * on the end to the word for seconds. Don't change the numbers. */
   XO("01000,01000 seconds")
   },

   {
   /* i18n-hint: Name of time display format that shows time in seconds
    * and milliseconds (1/1000 second) */
   { XO("seconds + milliseconds") },
   /* i18n-hint: Format string for displaying time in seconds and milliseconds 
    * as fractional seconds. Change the comma in the middle to the 1000s separator
    * for your locale, and the 'seconds' on the end to the word for seconds.
    * Don't change the numbers. The decimal separator is specified using '<' if
    * your languages uses a ',' or to '>' if your language uses a '.'. */
   XO("01000,01000>01000 seconds")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes
    * and seconds */
   { XO("hh:mm:ss") },
   /* i18n-hint: Format string for displaying time in hours, minutes and
    * seconds. Change the 'h' to the abbreviation for hours, 'm' to the
    * abbreviation for minutes and 's' to the abbreviation for seconds. Don't
    * change the numbers unless there aren't 60 seconds in a minute in your
    * locale */
   XO("0100 h 060 m 060 s")
   },

   {
   /* i18n-hint: Name of time display format that shows time in days, hours,
    * minutes and seconds */
   { XO("dd:hh:mm:ss") },
   /* i18n-hint: Format string for displaying time in days, hours, minutes and
    * seconds. Change the 'days' to the word for days, 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes and 's' to the
    * abbreviation for seconds. Don't change the numbers unless there aren't
    * 24 hours in a day in your locale */
   XO("0100 days 024 h 060 m 060 s")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours,
    * minutes, seconds and hundredths of a second (1/100 second) */
   { XO("hh:mm:ss + hundredths") },
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and hundredths of a second. Change the 'h' to the abbreviation for hours,
    * 'm' to the abbreviation for minutes and 's' to the abbreviation for seconds
    * (the hundredths are shown as decimal seconds). Don't change the numbers
    * unless there aren't 60 minutes in an hour in your locale.
    * The decimal separator is specified using '<' if your language uses a ',' or
    * to '>' if your language uses a '.'. */
   XO("0100 h 060 m 060>0100 s")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours,
    * minutes, seconds and milliseconds (1/1000 second) */
   { XO("hh:mm:ss + milliseconds") },
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and milliseconds. Change the 'h' to the abbreviation for hours, 'm' to the
    * abbreviation for minutes and 's' to the abbreviation for seconds (the
    * milliseconds are shown as decimal seconds) . Don't change the numbers
    * unless there aren't 60 minutes in an hour in your locale.
    * The decimal separator is specified using '<' if your language uses a ',' or
    * to '>' if your language uses a '.'. */
   XO("0100 h 060 m 060>01000 s")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours,
    * minutes, seconds and samples (at the current project sample rate) */
   { XO("hh:mm:ss + samples") },
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and samples. Change the 'h' to the abbreviation for hours, 'm' to the
    * abbreviation for minutes, 's' to the abbreviation for seconds and
    * translate samples . Don't change the numbers
    * unless there aren't 60 seconds in a minute in your locale.
    * The decimal separator is specified using '<' if your language uses a ',' or
    * to '>' if your language uses a '.'. */
   XO("0100 h 060 m 060 s+># samples")
   },

   {
   /* i18n-hint: Name of time display format that shows time in samples (at the
    * current project sample rate).  For example the number of a sample at 1
    * second into a recording at 44.1KHz would be 44,100.
    */
   { XO("samples") },
   /* i18n-hint: Format string for displaying time in samples (lots of samples).
    * Change the ',' to the 1000s separator for your locale, and translate
    * samples. If 1000s aren't a base multiple for your number system, then you
    * can change the numbers to an appropriate one, and put a 0 on the front */
   XO("01000,01000,01000 samples|#")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at 24 frames per second (commonly used for films) */
   { XO("hh:mm:ss + film frames (24 fps)") },
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames at 24 frames per second. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames' . Don't change the numbers
    * unless there aren't 60 seconds in a minute in your locale.
    * The decimal separator is specified using '<' if your language uses a ',' or
    * to '>' if your language uses a '.'. */
   XO("0100 h 060 m 060 s+>24 frames")
   },

   {
   /* i18n-hint: Name of time display format that shows time in frames (lots of
    * frames) at 24 frames per second (commonly used for films) */
   { XO("film frames (24 fps)") },
   /* i18n-hint: Format string for displaying time in frames at 24 frames per
    * second. Change the comma
    * in the middle to the 1000s separator for your locale,
    * translate 'frames' and leave the rest alone */
   XO("01000,01000 frames|24")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at NTSC TV drop-frame rate (used for American /
    * Japanese TV, and very odd) */
   { XO("hh:mm:ss + NTSC drop frames") },
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with NTSC drop frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. Leave the |N alone, it's important!
    * The decimal separator is specified using '<' if your language uses a ',' or
    * to '>' if your language uses a '.'. */
   XO("0100 h 060 m 060 s+>30 frames|N")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at NTSC TV non-drop-frame rate (used for American /
    * Japanese TV, and doesn't quite match wall time */
   { XO("hh:mm:ss + NTSC non-drop frames") },
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with NTSC drop frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. Leave the | .999000999 alone,
    * the whole things really is slightly off-speed!
    * The decimal separator is specified using '<' if your language uses a ',' or
    * to '>' if your language uses a '.'. */
   XO("0100 h 060 m 060 s+>030 frames| .999000999")
   },

   {
   /* i18n-hint: Name of time display format that shows time in frames at NTSC
    * TV frame rate (used for American / Japanese TV */
   { XO("NTSC frames") },
   /* i18n-hint: Format string for displaying time in frames with NTSC frames.
    * Change the comma
    * in the middle to the 1000s separator for your locale, 
    * translate 'frames' and leave the rest alone. That really is the frame
    * rate! */
   XO("01000,01000 frames|29.97002997")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at PAL TV frame rate (used for European TV) */
   { XO("hh:mm:ss + PAL frames (25 fps)") },
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with PAL TV frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. Nice simple time code!
    * The decimal separator is specified using '<' if your language uses a ',' or
    * to '>' if your language uses a '.'. */
   XO("0100 h 060 m 060 s+>25 frames")
   },

   {
   /* i18n-hint: Name of time display format that shows time in frames at PAL
    * TV frame rate (used for European TV) */
   { XO("PAL frames (25 fps)") },
   /* i18n-hint: Format string for displaying time in frames with NTSC frames.
    * Change the comma
    * in the middle to the 1000s separator for your locale, 
    * translate 'frames' and leave the rest alone. */
   XO("01000,01000 frames|25")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at CD Audio frame rate (75 frames per second) */
   { XO("hh:mm:ss + CDDA frames (75 fps)") },
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with CD Audio frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'.
    * The decimal separator is specified using '<' if your language uses a ',' or
    * to '>' if your language uses a '.'. */
   XO("0100 h 060 m 060 s+>75 frames")
   },

   {
   /* i18n-hint: Name of time display format that shows time in frames at CD
    * Audio frame rate (75 frames per second) */
   { XO("CDDA frames (75 fps)") },
   /* i18n-hint: Format string for displaying time in frames with CD Audio
    * frames. Change the comma
    * in the middle to the 1000s separator for your locale, 
    * translate 'frames' and leave the rest alone */
   XO("01000,01000 frames|75")
   },

   // Beats and Measures must be the last format for UpdatePrefs to work
   {
   /* i18n-hint: Name of time display format that shows time beats and measures */
   { XO("beats and measures") },
   /* i18n-hint: Format string for displaying time in beats and measures.
      * Look at the function for more detail */
   BuildBeatsFormat()
   },
};

namespace {
struct BeatsUpdater : PrefsListener {
   void UpdatePrefs() override
   {
      TimeConverterFormats_[WXSIZEOF(TimeConverterFormats_) - 1] =
      {
         { XO("beats and measures") },
         BuildBeatsFormat()
      };
   }
   BeatsUpdater() { UpdatePrefs(); }
} theBeatsUpdater;
}

/** \brief array of formats the control knows about internally
 *  array of string pairs for name of the format and the format string
 *  needed to create that format output. This is used for the pop-up
 *  list of formats to choose from in the control. */
static const BuiltinFormatString FrequencyConverterFormats_[] = {
   {
      /* i18n-hint: Name of display format that shows frequency in hertz */
      { XO("Hz") },
      {
         /* i18n-hint: Format string for displaying frequency in hertz. Change
         * the decimal point for your locale. Don't change the numbers.
         * The decimal separator is specified using '<' if your language uses a ',' or
         * to '>' if your language uses a '.'. */
         XO("010,01000>0100 Hz")
         , XO("centihertz")
      }
   },

   {
      /* i18n-hint: Name of display format that shows frequency in kilohertz */
      { XO("kHz") },
      {
         /* i18n-hint: Format string for displaying frequency in kilohertz. Change
         * the decimal point for your locale. Don't change the numbers.
         * The decimal separator is specified using '<' if your language uses a ',' or
         * to '>' if your language uses a '.'. */
         XO("01000>01000 kHz|0.001")
         , XO("hertz")
      }
   },
};

/** \brief array of formats the control knows about internally
 *  array of string pairs for name of the format and the format string
 *  needed to create that format output. This is used for the pop-up
 *  list of formats to choose from in the control. */
static const BuiltinFormatString BandwidthConverterFormats_[] = {
   {
   /* i18n-hint: Name of display format that shows log of frequency
    * in octaves */
   { XO("octaves") },
   {
      /* i18n-hint: Format string for displaying log of frequency in octaves.
       * Change the decimal points for your locale. Don't change the numbers.
       * The decimal separator is specified using '<' if your language uses a ',' or
       * to '>' if your language uses a '.'. */
      XO("100>01000 octaves|1.442695041"),    // Scale factor is 1 / ln (2)
      /* i18n-hint: an octave is a doubling of frequency */
      XO("thousandths of octaves")
   }
   },

   {
   /* i18n-hint: Name of display format that shows log of frequency
    * in semitones and cents */
   { XO("semitones + cents") },
   {
      /* i18n-hint: Format string for displaying log of frequency in semitones
       * and cents.
       * Change the decimal points for your locale. Don't change the numbers.
       * The decimal separator is specified using '<' if your language uses a ',' or
       * to '>' if your language uses a '.'. */
      XO("1000 semitones >0100 cents|17.312340491"),   // Scale factor is 12 / ln (2)
      /* i18n-hint: a cent is a hundredth of a semitone (which is 1/12 octave) */
      XO("hundredths of cents")
   }
   },
   
   {
   /* i18n-hint: Name of display format that shows log of frequency
    * in decades */
   { XO("decades") },
   {
      /* i18n-hint: Format string for displaying log of frequency in decades.
       * Change the decimal points for your locale. Don't change the numbers. */
      XO("10>01000 decades|0.434294482"),   // Scale factor is 1 / ln (10)
      /* i18n-hint: a decade is a tenfold increase of frequency */
      XO("thousandths of decades")
   }
   },
};

   const BuiltinFormatString *ChooseBuiltinFormatStrings
      (NumericConverterType type)
   {
      switch (type) {
         default:
      case NumericConverterType::TIME:
            return TimeConverterFormats_;
      case NumericConverterType::FREQUENCY:
            return FrequencyConverterFormats_;
      case NumericConverterType::BANDWIDTH:
            return BandwidthConverterFormats_;
      }
   }

   size_t ChooseNBuiltinFormatStrings
      (NumericConverterType type)
   {
      switch (type) {
         default:
      case NumericConverterType::TIME:
            return WXSIZEOF(TimeConverterFormats_);
      case NumericConverterType::FREQUENCY:
            return WXSIZEOF(FrequencyConverterFormats_);
      case NumericConverterType::BANDWIDTH:
            return WXSIZEOF(BandwidthConverterFormats_);
      }
   }
}

//
// ----------------------------------------------------------------------------
// NumericConverter Class
// ----------------------------------------------------------------------------
//
NumericFormatSymbol NumericConverter::DefaultSelectionFormat()
{ return TimeConverterFormats_[5].name; }
NumericFormatSymbol NumericConverter::TimeAndSampleFormat()
{ return TimeConverterFormats_[6].name; }
NumericFormatSymbol NumericConverter::SecondsFormat()
{ return TimeConverterFormats_[0].name; }
NumericFormatSymbol NumericConverter::HoursMinsSecondsFormat()
{ return TimeConverterFormats_[1].name; }
NumericFormatSymbol NumericConverter::HundredthsFormat()
{ return TimeConverterFormats_[3].name; }

NumericFormatSymbol NumericConverter::HertzFormat()
{ return FrequencyConverterFormats_[0].name; }

NumericFormatSymbol
NumericConverter::LookupFormat(NumericConverterType type, const wxString& id)
{
   if (id.empty()) {
            if (type == NumericConverterType::TIME)
         return DefaultSelectionFormat();
      else
         return ChooseBuiltinFormatStrings(type)[0].name;
   }
   else {
      auto begin = ChooseBuiltinFormatStrings(type);
      auto end = begin + ChooseNBuiltinFormatStrings(type);
      auto iter = std::find( begin, end, BuiltinFormatString{ id, {} } );
      if (iter == end)
         iter = begin;
      return iter->name;
   }
}

NumericConverter::NumericConverter(NumericConverterType type,
                                   const NumericFormatSymbol & formatName,
                                   double value,
                                   double sampleRate)
   : mBuiltinFormatStrings( ChooseBuiltinFormatStrings( type ) )
   , mNBuiltins( ChooseNBuiltinFormatStrings( type ) )
{
   ResetMinValue();
   ResetMaxValue();
   mInvalidValue = -1.0;

   mDefaultNdx = 0;

   mType = type;

   if (type == NumericConverterType::TIME )
      mDefaultNdx = 5; // Default to "hh:mm:ss + milliseconds".

   mValue = value; // used in SetSampleRate, reassigned later

   SetSampleRate(sampleRate);
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

bool NumericConverter::SetFormatName(const NumericFormatSymbol & formatName)
{
   return
      SetFormatString(GetBuiltinFormat(formatName));
}

bool NumericConverter::SetFormatString(const FormatStrings & formatString)
{
   if (mFormatString != formatString) {
      mFormatString = formatString;
      ParseFormatString(mFormatString.formatStr);
      ValueToControls();
      ControlsToValue();
      return true;
   }
   else
      return false;
}

void NumericConverter::SetSampleRate(double sampleRate)
{
   mSampleRate = sampleRate;
   ParseFormatString(mFormatString.formatStr);
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

int NumericConverter::GetFormatIndex()
{
   // int ndx = 1;
   int ndx = std::min(1, GetNumBuiltins() - 1);
   int i;

   for (i = 0; i < GetNumBuiltins(); i++) {
      if (mFormatString == GetBuiltinFormat(i)) {
         ndx = i;
         break;
      }
   }

   return ndx;
}

int NumericConverter::GetNumBuiltins()
{
   return mNBuiltins;
}

NumericFormatSymbol NumericConverter::GetBuiltinName(const int index)
{
   if (index >= 0 && index < GetNumBuiltins())
      return mBuiltinFormatStrings[index].name;

   return {};
}

auto NumericConverter::GetBuiltinFormat(const int index) -> FormatStrings
{
   if (index >= 0 && index < GetNumBuiltins())
      return mBuiltinFormatStrings[index].formatStrings;

   return {};
}

auto NumericConverter::GetBuiltinFormat(
   const NumericFormatSymbol &name) -> FormatStrings
{
   int ndx =
      std::find( mBuiltinFormatStrings, mBuiltinFormatStrings + mNBuiltins,
         BuiltinFormatString{ name, {} } )
            - mBuiltinFormatStrings;
   if (ndx == (int)mNBuiltins)
      ndx = mDefaultNdx;

   return GetBuiltinFormat(ndx);
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

void NumericConverter::Adjust(int steps, int dir, int focusedDigit)
{
   if (!mFormatter)
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
