/**********************************************************************

  Audacity: A Digital Audio Editor

  NumericConverter.cpp

  Dominic Mazzoni

  Paul Licameli split from NumericTextCtrl.cpp


********************************************************************//**
NumericConverter
\class NumericConverter
\brief NumericConverter provides the advanced formatting control used
in the selection bar of Audacity.

  Any negative value given to the converter is considered invalid and
  all digit positions of the resulting string will be filled with hyphens.
  Otherwise:

  The NumericConverter makes use of a format string to specify the
  exact way that a single value is split into several fields,
  such as the hh:mm:ss format.  The advantage of this format string
  is that it is very small and compact, but human-readable and
  somewhat intuitive, so that it's easy to add NEW layouts
  in the future.  It's also designed to make it easier to add
  i18n support, since the way that numbers are displayed in different
  languages could conceivably vary a lot.

  The number to be formatted may be expressed in seconds, so the format
  string can specify the relationship of each field to the number of
  seconds.

  The class is also reused to format some non-time values such as
  frequency and log of frequency.

  Let's start by considering an example: here's the format string
  that prints an integer number of seconds in the hour minute
  second h:m:s format:

    *:60:60

  The "*" is a wildcard, saying that the leftmost field can contain
  numbers of arbitrary magnitude.  The next character, ':', since it
  is not a digit or a wildcard, is interpreted as a delimiter, and
  will be displayed between those fields.  The next number, 60,
  indicates that the range of the next field (minutes) is 60.
  Then there's another ':' delimiter, and finally the last field
  (seconds) is 60.  So, if you give it a number like 3758
  it is formatted as:

    3758 seconds, "*:60:60" -> "1:2:38"

  Note that 3758 = 1*60*60 + 2*60 + 38.

  When NumericConverter formats an integer, you can think of its process
  as working from right to left.  Given the value "3758", it fills
  in the seconds by dividing by 60, sticking the remainder in the
  seconds field and then passing the quotient to the next field to
  the left.

  In order to format a field with leading zeros, simply add a leading
  zero to that field, like this:

    3758 seconds, "*:060:060" -> "1:02:38"

  In order to format fractions, simply include a field delimiter
  ending with a decimal point.  If the delimiter is simply '.' with
  nothing else, then the '.' is actually displayed.  Otherwise the
  '.' is dropped, and the other characters in the delimiter are
  displayed instead.

  Here's how we'd display hours, minutes, and seconds with three
  decimal places after the seconds:

    3758.5 seconds, "*:060:060.01000" -> "1:02:38.500"

  Similarly, here's how we'd display the fractional part of
  seconds as film frames (24 per second) instead of milliseconds:

    3758.5 seconds, "*:060:060 and .24 frames" -> "1:02:38 and 12 frames"

  Note that the decimal '.' is associated with the delimiter, not
  with the 24.

  Additionally, the special character '#' can be used in place of a number
  to represent the current sample rate.  Use '0#' to add leading
  zeros to that field.  For example:

    3758.5 seconds, "*:060:060+.#samples" -> "1:02:38+22050samples"

  (Almost) Finally, there is a rule that allows you to change the units into
  something other than seconds.  To do this, put a "|" character on
  the far right, followed by a number specifying the scaling factor.
  As an exception to previous rules, decimal points are allowed
  in the final scaling factor - the period is not interpreted as it
  would be before the "|" character.  (This is fine, because all
  previous fields must be integers to make sense.)  Anyway, if you
  include a scaling factor after a "|", the number will be
  multiplied by this factor before it is formatted.  For example, to
  express the current time in NTSC frames (~29.97 fps), you could
  use the following formatting:

    3758.5 seconds, "*.01000 frames|29.97002997" -> "112642.358 frames"

  Finally there is a further special character that can be used after a "|"
  and that is "N".  This applies special rule for NTSC drop-frame timecode.

  Summary of format string rules:

  - The characters '0-9', '*', and '#' are numeric.  Any sequence of
    these characters is treated as defining a NEW field by specifying
    its range.  All other characters become delimiters between fields.
    (The one exception is that '.' is treated as numeric after the
    optional '|'.)
  - A field with a range of '*', which only makes sense as the
    leftmost field, means the field should display as large a number
    as necessary. (Note: this no longer makes sense here and applies to a
    previous version).
  - The character '#' represents the current sample rate.
  - If a field specifier beings with a leading zero, it will be formatted
    with leading zeros, too - enough to display the maximum value
    that field can display.  So the number 7 in a field specified
    as '01000' would be formatted as '007'.  Bond.  James Bond.
  - Any non-numeric characters before the first field are treated
    as a prefix, and will be displayed to the left of the first field.
  - A delimiter ending in '.' is treated specially.  All fields after
    this delimiter are fractional fields, after the decimal point.
  - The '|' character is treated as a special delimiter.  The number
    to the right of this character (which is allowed to contain a
    decimal point) is treated as a scaling factor.  The number is
    multiplied by this factor before converting.
  - The special character 'N' after '|' is only used for NTSC drop-frame.

*******************************************************************//**

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

void NumericField::CreateDigitFormatStr()
{
   if (range > 1)
      digits = (int)ceil(log10(range-1.0));
   else
      digits = 5; // hack: default
   if (zeropad && range>1)
      formatStr.Printf(wxT("%%0%dd"), digits); // ex. "%03d" if digits is 3
   else {
      formatStr.Printf(wxT("%%0%dd"), digits);
   }
}

namespace {

/** \brief array of formats the control knows about internally
 *  array of string pairs for name of the format and the format string
 *  needed to create that format output. This is used for the pop-up
 *  list of formats to choose from in the control.          */
static const BuiltinFormatString TimeConverterFormats_[] =  {
   {
   /* i18n-hint: Name of time display format that shows time in seconds */
   { XO("seconds") },
   /* i18n-hint: Format string for displaying time in seconds. Change the comma
    * in the middle to the 1000s separator for your locale, and the 'seconds'
    * on the end to the word for seconds. Don't change the numbers. */
   XO("01000,01000 seconds")
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
};

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
      (NumericConverter::Type type)
   {
      switch (type) {
         default:
         case NumericConverter::TIME:
            return TimeConverterFormats_;
         case NumericConverter::FREQUENCY:
            return FrequencyConverterFormats_;
         case NumericConverter::BANDWIDTH:
            return BandwidthConverterFormats_;
      }
   }

   size_t ChooseNBuiltinFormatStrings
      (NumericConverter::Type type)
   {
      switch (type) {
         default:
         case NumericConverter::TIME:
            return WXSIZEOF(TimeConverterFormats_);
         case NumericConverter::FREQUENCY:
            return WXSIZEOF(FrequencyConverterFormats_);
         case NumericConverter::BANDWIDTH:
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
{ return TimeConverterFormats_[4].name; }
NumericFormatSymbol NumericConverter::TimeAndSampleFormat()
{ return TimeConverterFormats_[5].name; }
NumericFormatSymbol NumericConverter::SecondsFormat()
{ return TimeConverterFormats_[0].name; }
NumericFormatSymbol NumericConverter::HoursMinsSecondsFormat()
{ return TimeConverterFormats_[1].name; }
NumericFormatSymbol NumericConverter::HundredthsFormat()
{ return TimeConverterFormats_[3].name; }

NumericFormatSymbol NumericConverter::HertzFormat()
{ return FrequencyConverterFormats_[0].name; }

NumericFormatSymbol NumericConverter::LookupFormat( Type type, const wxString& id)
{
   if (id.empty()) {
      if (type == TIME)
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

NumericConverter::NumericConverter(Type type,
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

   if (type == NumericConverter::TIME )
      mDefaultNdx = 4; // Default to "hh:mm:ss + milliseconds".

   mScalingFactor = 1.0f;
   mSampleRate = 1.0f;
   mNtscDrop = false;

   mFocusedDigit = 0;

   mValue = value; // used in SetSampleRate, reassigned later

   SetSampleRate(sampleRate);
   SetFormatName(formatName);
   SetValue(value); // mValue got overridden to -1 in ControlsToValue(), reassign
}

void NumericConverter::ParseFormatString(
   const TranslatableString & untranslatedFormat)
{
   auto format = untranslatedFormat.Translation();

   mPrefix = wxT("");
   mFields.clear();
   mDigits.clear();
   mScalingFactor = 1.0;

   // We will change inFrac to true when we hit our first decimal point.
   bool inFrac = false;
   int fracMult = 1;
   int numWholeFields = 0;
   int numFracFields = 0;
   wxString numStr;
   wxString delimStr;
   unsigned int i;

   mNtscDrop = false;
   for(i=0; i<format.length(); i++) {
      bool handleDelim = false;
      bool handleNum = false;

      if (format[i] == '|') {
         wxString remainder = format.Right(format.length() - i - 1);
         // For languages which use , as a separator.
         remainder.Replace(wxT(","), wxT("."));

         if (remainder == wxT("#"))
            mScalingFactor = mSampleRate;
         else if (remainder == wxT("N")) {
            mNtscDrop = true;
         }
         else
            // Use the C locale here for string to number.
            // Translations are often incomplete.
            // We can't rely on the correct ',' or '.' in the 
            // translation, so we work based on '.' for decimal point.
            remainder.ToCDouble(&mScalingFactor);
         i = format.length()-1; // force break out of loop
         if (!delimStr.empty())
            handleDelim = true;
         if (!numStr.empty())
            handleNum = true;
      }
      else if ((format[i] >= '0' && format[i] <='9') ||
          format[i] == wxT('*') || format[i] == wxT('#')) {
         numStr += format[i];
         if (!delimStr.empty())
            handleDelim = true;
      }
      else {
         delimStr += format[i];
         if (!numStr.empty())
            handleNum = true;
      }

      if (i == format.length() - 1) {
         if (!numStr.empty())
            handleNum = true;
         if (!delimStr.empty())
            handleDelim = true;
      }

      if (handleNum) {
         bool zeropad = false;
         long range = 0;

         if (numStr.Right(1) == wxT("#"))
            range = (long int)mSampleRate;
         else if (numStr.Right(1) != wxT("*")) {
            numStr.ToLong(&range);
         }
         if (numStr.GetChar(0)=='0' && numStr.length()>1)
            zeropad = true;

         // Hack: always zeropad
         zeropad = true;

         if (inFrac) {
            int base = fracMult * range;
            mFields.push_back(NumericField(inFrac, base, range, zeropad));
            fracMult *= range;
            numFracFields++;
         }
         else {
            unsigned int j;
            for(j=0; j<mFields.size(); j++)
               mFields[j].base *= range;
            mFields.push_back(NumericField(inFrac, 1, range, zeropad));
            numWholeFields++;
         }
         numStr = wxT("");
      }

      if (handleDelim) {
         bool goToFrac = false;

         if (!inFrac) {
            wxChar delim = delimStr[delimStr.length()-1];
            if (delim=='<' || delim=='>') {
               goToFrac = true;
               if (delimStr.length() > 1)
                  delimStr = delimStr.BeforeLast(delim);
            }
         }

         if (inFrac) {
            if (numFracFields == 0) {
               // Should never happen
               return;
            }
            if (handleNum && numFracFields > 1)
               mFields[mFields.size()-2].label = delimStr;
            else
               mFields[mFields.size()-1].label = delimStr;
         }
         else {
            if (numWholeFields == 0)
               mPrefix = delimStr;
            else {
               delimStr.Replace(wxT("<"), wxT(","));
               delimStr.Replace(wxT(">"), wxT("."));
               mFields[numWholeFields-1].label = delimStr;
            }
         }

         if (goToFrac)
            inFrac = true;
         delimStr = wxT("");
      }
   }

   for(i = 0; i < mFields.size(); i++) {
      mFields[i].CreateDigitFormatStr();
   }

   int pos = 0;
   int j;
   mValueMask = wxT("");
   mValueTemplate = wxT("");

   mValueTemplate += mPrefix;
   for(j=0; j<(int)mPrefix.length(); j++)
      mValueMask += wxT(".");
   pos += mPrefix.length();

   for(i = 0; i < mFields.size(); i++) {
      mFields[i].pos = pos;

      for(j=0; j<mFields[i].digits; j++) {
         mDigits.push_back(DigitInfo(i, j, pos));
         mValueTemplate += wxT("0");
         mValueMask += wxT("0");
         pos++;
      }

      pos += mFields[i].label.length();
      mValueTemplate += mFields[i].label;
      for(j=0; j<(int)mFields[i].label.length(); j++)
         mValueMask += wxT(".");
   }
}

void NumericConverter::PrintDebugInfo()
{
   unsigned int i;

   wxPrintf("%s", (const char *)mPrefix.mb_str());

   for(i = 0; i < mFields.size(); i++) {
      if (mFields[i].frac) {
         wxPrintf("(t * %d) %% %d '%s' ",
                mFields[i].base,
                mFields[i].range,
                (const char *)mFields[i].label.mb_str());

      }
      else {
         wxPrintf("(t / %d) %% %d '%s' ",
                mFields[i].base,
                mFields[i].range,
                (const char *)mFields[i].label.mb_str());
      }
   }

   wxPrintf("\n");
}

NumericConverter::NumericConverter(const NumericConverter&) = default;

NumericConverter::~NumericConverter()
{
}

void NumericConverter::ValueToControls()
{
   ValueToControls(mValue);
}

void NumericConverter::ValueToControls(double rawValue, bool nearest /* = true */)
{
   //rawValue = 4.9995f; Only for testing!
   if (mType == TIME)
      rawValue =
         floor(rawValue * mSampleRate + (nearest ? 0.5f : 0.0f))
            / mSampleRate; // put on a sample
   double theValue =
      rawValue * mScalingFactor
         // PRL:  what WAS this .000001 for?  Nobody could explain.
         // + .000001
      ;
   sampleCount t_int;
   bool round = true;
   // We round on the last field.  If we have a fractional field we round using it.
   // Otherwise we round to nearest integer.
   for(unsigned int i = 0; i < mFields.size(); i++) {
      if (mFields[i].frac)
         round = false;
   }
   if (theValue < 0)
      t_int = -1;
   else if(round)
      t_int = sampleCount(theValue + (nearest ? 0.5f : 0.0f));
   else
   {
      wxASSERT( mFields.back().frac );
      theValue += (nearest ? 0.5f : 0.0f) / mFields.back().base;
      t_int = sampleCount(theValue);
   }
   double t_frac;
   if (theValue < 0)
      t_frac = -1;
   else
      t_frac = (theValue - t_int.as_double() );
   unsigned int i;
   int tenMins;
   int mins;
   int addMins;
   int secs;
   int frames;

   mValueString = mPrefix;

   if(mNtscDrop && theValue >= 0) {
      frames = (int)(theValue*30./1.001 + (nearest ? 0.5f : 0.0f));
      tenMins = frames/17982;
      frames -= tenMins*17982;
      mins = tenMins * 10;
      if(frames >= 1800) {
         frames -= 1800;
         mins++;
         addMins = frames/1798;
         frames -= addMins*1798;
         mins += addMins;
         secs = frames/30;
         frames -= secs*30;
         frames += 2;
         if( frames >= 30 ) {
            secs++;
            frames -= 30;
         }
      }
      else {
         secs = frames/30;
         frames -= secs*30;
      }
      t_int = mins * 60 + secs;
      t_frac = frames / 30.;
   }

   for(i = 0; i < mFields.size(); i++) {
      long long value = -1;

      if (mFields[i].frac) {
         // JKC: This old code looks bogus to me.
         // The rounding is not propagating to earlier fields in the frac case.
         //value = (int)(t_frac * mFields[i].base + 0.5);  // +0.5 as rounding required
         // I did the rounding earlier.
         if (t_frac >= 0)
            value = t_frac * mFields[i].base;
         // JKC: TODO: Find out what the range is supposed to do.
         // It looks bogus too.
         //if (mFields[i].range > 0)
         //   value = value % mFields[i].range;
      }
      else {
         if (t_int >= 0) {
            value = t_int.as_long_long() / mFields[i].base;
            if (mFields[i].range > 0)
               value = value % mFields[i].range;
         }
      }

      wxString field;
      if (value < 0) {
         for (int ii = 0; ii < mFields[i].digits; ++ii)
            field += wxT("-");
      }
      else
         field = wxString::Format(mFields[i].formatStr, (int) value);
      mValueString += field;
      mValueString += mFields[i].label;
   }
}

void NumericConverter::ControlsToValue()
{
   unsigned int i;
   double t = 0.0;

   if (mFields.size() > 0 &&
      mValueString.Mid(mFields[0].pos, 1) == wxChar('-')) {
      mValue = mInvalidValue;
      return;
   }

   for(i = 0; i < mFields.size(); i++) {
      long val;
      mFields[i].str = mValueString.Mid(mFields[i].pos,
                                        mFields[i].digits);
      mFields[i].str.ToLong(&val);
      if (mFields[i].frac)
         t += (val / (double)mFields[i].base);
      else
         t += (val * (double)mFields[i].base);
   }

   t /= mScalingFactor;
   if(mNtscDrop) {
      int t_int = (int)(t + .000000001);
      double t_frac = (t - t_int);
      int tenMins = t_int/600;
      double frames = tenMins*17982;
      t_int -= tenMins*600;
      int mins = t_int/60;
      int addMins = 0;
      if( mins > 0 ) {
         frames += 1800;
         addMins = mins - 1;
      }
      frames += addMins * 1798;
      t_int -= mins*60;
      if( mins == 0 )   //first min of a block of 10, don't drop frames 0 and 1
         frames += t_int * 30 + t_frac*30.;
      else {   //drop frames 0 and 1 of first seconds of these minutes
         if( t_int > 0 )
            frames += 28 + (t_int-1)*30 + t_frac*30.;
         else
            frames += t_frac*30. -2.;
      }
      t = frames * 1.001 / 30.;
   }

   mValue = std::max(mMinValue, std::min(mMaxValue, t));
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

void NumericConverter::Increment()
{
   mFocusedDigit = mDigits.size() - 1;
   Adjust(1, 1);
}

void NumericConverter::Decrement()
{
   mFocusedDigit = mDigits.size() - 1;
   Adjust(1, -1);
}

void NumericConverter::Adjust(int steps, int dir)
{
   // It is possible and "valid" for steps to be zero if a
   // high precision device is being used and wxWidgets supports
   // reporting a higher precision...Mac wx3 does.
   if (steps == 0)
      return;

   wxASSERT(dir == -1 || dir == 1);
   wxASSERT(steps > 0);
   if (steps < 0)
      steps = -steps;

   while (steps != 0)
   {
      for (size_t i = 0; i < mFields.size(); i++)
      {
         if ((mDigits[mFocusedDigit].pos >= mFields[i].pos) &&
             (mDigits[mFocusedDigit].pos < mFields[i].pos + mFields[i].digits))
         {   //it's this field
            if (!mNtscDrop)
            {
               ControlsToValue();
            }
            else
            {
               mNtscDrop = false;
               ControlsToValue();
               mNtscDrop = true;
            }

            if (mValue < 0)
               mValue = 0;

            mValue *= mScalingFactor;

            double mult = pow(10., mFields[i].digits - (mDigits[mFocusedDigit].pos - mFields[i].pos) - 1);
            if (mFields[i].frac)
            {
               mValue += ((mult / (double)mFields[i].base) * dir);
            }
            else
            {
               mValue += ((mult * (double)mFields[i].base) * dir);
            }

            if (mNtscDrop)
            {
               if ((mValue - (int)mValue) * 30 < 2)
               {
                  if ((((int)mValue) % 60 == 0) && (((int)mValue) % 600 != 0))
                  {
                     mValue = (int)mValue + (dir > 0 ? 2. : -1.) / 30.;
                  }
               }
            }

            if (mValue < 0.)
            {
               mValue = 0.;
            }

            mValue = std::max(mMinValue, std::min(mMaxValue, mValue));

            mValue /= mScalingFactor;

            if (!mNtscDrop)
            {
               ValueToControls();
            }
            else
            {
               mNtscDrop = false;
               ValueToControls();
               mNtscDrop = true;
               ControlsToValue();
            }
            break;
         }
      }
      steps--;
   }

   ControlsToValue();
}
