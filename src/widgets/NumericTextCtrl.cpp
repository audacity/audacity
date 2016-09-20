/**********************************************************************

  Audacity: A Digital Audio Editor

  NumericTextCtrl.cpp

  Dominic Mazzoni


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

  Note that the decimal '.' is associated with the delimeter, not
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
    this delimeter are fractional fields, after the decimal point.
  - The '|' character is treated as a special delimiter.  The number
    to the right of this character (which is allowed to contain a
    decimal point) is treated as a scaling factor.  The number is
    multiplied by this factor before converting.
  - The special character 'N' after '|' is only used for NTSC drop-frame.

*******************************************************************//**

\class NumericTextCtrlAx
\brief NumericTextCtrlAx gives the NumericTextCtrl Accessibility.

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
printf inspired style format string, optimised for displaying time in
different formats.

*//****************************************************************//**

\class NumericField
\brief NumericField is a class used in NumericTextCtrl

*//****************************************************************//**

\class DigitInfo
\brief DigitInfo is a class used in NumericTextCtrl

**********************************************************************/


#include "../Audacity.h"
#include "NumericTextCtrl.h"
#include "audacity/Types.h"
#include "../AudacityApp.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../AColor.h"
#include "../Project.h"

#include <algorithm>
#include <math.h>
#include <limits>

#include <wx/wx.h>
#include <wx/dcmemory.h>
#include <wx/font.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/tooltip.h>
#include <wx/toplevel.h>

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
   wxString name;
   wxString formatStr;
};

//
// ----------------------------------------------------------------------------
// NumericField Class
// ----------------------------------------------------------------------------
//
class NumericField
{
public:
   NumericField(bool _frac, int _base, int _range, bool _zeropad)
   {
      frac = _frac;
      base = _base;
      range = _range;
      zeropad = _zeropad;
      digits = 0;
   }
   void CreateDigitFormatStr()
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
   bool frac; // is it a fractional field
   int base;  // divide by this (multiply, after decimal point)
   int range; // then take modulo this
   int digits;
   int pos;   // Index of this field in the ValueString
   int fieldX; // x-position of the field on-screen
   int fieldW; // width of the field on-screen
   int labelX; // x-position of the label on-screen
   bool zeropad;
   wxString label;
   wxString formatStr;
   wxString str;
};

//
// ----------------------------------------------------------------------------
// DigitInfo Class
// ----------------------------------------------------------------------------
//
class DigitInfo
{
public:
   DigitInfo(int _field, int _index, int _pos, wxRect _box)
   {
      field = _field;
      index = _index;
      pos = _pos;
      digitBox = _box;
   }
   int field; // Which field
   int index; // Index of this digit within the field
   int pos;   // Position in the ValueString
   wxRect digitBox;
};

#include <wx/arrimpl.cpp>
WX_DEFINE_OBJARRAY(NumericFieldArray);
WX_DEFINE_OBJARRAY(DigitInfoArray);

namespace {

/** \brief array of formats the control knows about internally
 *  array of string pairs for name of the format and the format string
 *  needed to create that format output. This is used for the pop-up
 *  list of formats to choose from in the control.          */
const BuiltinFormatString TimeConverterFormats[] =  {
   {
   /* i18n-hint: Name of time display format that shows time in seconds */
   _("seconds"),
   /* i18n-hint: Format string for displaying time in seconds. Change the comma
    * in the middle to the 1000s separator for your locale, and the 'seconds'
    * on the end to the word for seconds. Don't change the numbers. */
   _("01000,01000 seconds")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes
    * and seconds */
   _("hh:mm:ss"),
   /* i18n-hint: Format string for displaying time in hours, minutes and
    * seconds. Change the 'h' to the abbreviation for hours, 'm' to the
    * abbreviation for minutes and 's' to the abbreviation for seconds. Don't
    * change the numbers unless there aren't 60 seconds in a minute in your
    * locale */
   _("0100 h 060 m 060 s")
   },

   {
   /* i18n-hint: Name of time display format that shows time in days, hours,
    * minutes and seconds */
   _("dd:hh:mm:ss"),
   /* i18n-hint: Format string for displaying time in days, hours, minutes and
    * seconds. Change the 'days' to the word for days, 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes and 's' to the
    * abbreviation for seconds. Don't change the numbers unless there aren't
    * 24 hours in a day in your locale */
   _("0100 days 024 h 060 m 060 s")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours,
    * minutes, seconds and hundredths of a second (1/100 second) */
   _("hh:mm:ss + hundredths"),
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and hundredths of a second. Change the 'h' to the abbreviation for hours,
    * 'm' to the abbreviation for minutes and 's' to the abbreviation for seconds
    * (the hundredths are shown as decimal seconds) . Don't change the numbers
    * unless there aren't 60 minutes in an hour in your locale */
   _("0100 h 060 m 060.0100 s")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours,
    * minutes, seconds and milliseconds (1/1000 second) */
   _("hh:mm:ss + milliseconds"),
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and milliseconds. Change the 'h' to the abbreviation for hours, 'm' to the
    * abbreviation for minutes and 's' to the abbreviation for seconds (the
    * milliseconds are shown as decimal seconds) . Don't change the numbers
    * unless there aren't 60 minutes in an hour in your locale */
   _("0100 h 060 m 060.01000 s")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours,
    * minutes, seconds and samples (at the current project sample rate) */
   _("hh:mm:ss + samples"),
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and samples. Change the 'h' to the abbreviation for hours, 'm' to the
    * abbreviation for minutes, 's' to the abbreviation for seconds and
    * translate samples . Don't change the numbers
    * unless there aren't 60 seconds in a minute in your locale */
   _("0100 h 060 m 060 s+.# samples")
   },

   {
   /* i18n-hint: Name of time display format that shows time in samples (at the
    * current project sample rate).  For example the number of a sample at 1
    * second into a recording at 44.1KHz would be 44,100.
    */
   _("samples"),
   /* i18n-hint: Format string for displaying time in samples (lots of samples).
    * Change the ',' to the 1000s separator for your locale, and translate
    * samples. If 1000s aren't a base multiple for your number system, then you
    * can change the numbers to an appropriate one, and put a 0 on the front */
   _("01000,01000,01000 samples|#")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at 24 frames per second (commonly used for films) */
   _("hh:mm:ss + film frames (24 fps)"),
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames at 24 frames per second. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames' . Don't change the numbers
    * unless there aren't 60 seconds in a minute in your locale */
   _("0100 h 060 m 060 s+.24 frames")
   },

   {
   /* i18n-hint: Name of time display format that shows time in frames (lots of
    * frames) at 24 frames per second (commonly used for films) */
   _("film frames (24 fps)"),
   /* i18n-hint: Format string for displaying time in frames at 24 frames per
    * second. Change the comma
    * in the middle to the 1000s separator for your locale,
    * translate 'frames' and leave the rest alone */
   _("01000,01000 frames|24")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at NTSC TV drop-frame rate (used for American /
    * Japanese TV, and very odd) */
   _("hh:mm:ss + NTSC drop frames"),
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with NTSC drop frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. Leave the |N alone, it's important! */
   _("0100 h 060 m 060 s+.30 frames|N")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at NTSC TV non-drop-frame rate (used for American /
    * Japanese TV, and doesn't quite match wall time */
   _("hh:mm:ss + NTSC non-drop frames"),
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with NTSC drop frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. Leave the | .999000999 alone,
    * the whole things really is slightly off-speed! */
   _("0100 h 060 m 060 s+.030 frames| .999000999")
   },

   {
   /* i18n-hint: Name of time display format that shows time in frames at NTSC
    * TV frame rate (used for American / Japanese TV */
   _("NTSC frames"),
   /* i18n-hint: Format string for displaying time in frames with NTSC frames.
    * Change the comma
    * in the middle to the 1000s separator for your locale, 
    * translate 'frames' and leave the rest alone. That really is the frame
    * rate! */
   _("01000,01000 frames|29.97002997")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at PAL TV frame rate (used for European TV) */
   _("hh:mm:ss + PAL frames (25 fps)"),
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with PAL TV frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. Nice simple time code! */
   _("0100 h 060 m 060 s+.25 frames")
   },

   {
   /* i18n-hint: Name of time display format that shows time in frames at PAL
    * TV frame rate (used for European TV) */
   _("PAL frames (25 fps)"),
   /* i18n-hint: Format string for displaying time in frames with NTSC frames.
    * Change the comma
    * in the middle to the 1000s separator for your locale, 
    * translate 'frames' and leave the rest alone. */
   _("01000,01000 frames|25")
   },

   {
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at CD Audio frame rate (75 frames per second) */
   _("hh:mm:ss + CDDA frames (75 fps)"),
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with CD Audio frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. */
   _("0100 h 060 m 060 s+.75 frames")
   },

   {
   /* i18n-hint: Name of time display format that shows time in frames at CD
    * Audio frame rate (75 frames per second) */
   _("CDDA frames (75 fps)"),
   /* i18n-hint: Format string for displaying time in frames with CD Audio
    * frames. Change the comma
    * in the middle to the 1000s separator for your locale, 
    * translate 'frames' and leave the rest alone */
   _("01000,01000 frames|75")
   },
};

/** \brief array of formats the control knows about internally
 *  array of string pairs for name of the format and the format string
 *  needed to create that format output. This is used for the pop-up
 *  list of formats to choose from in the control. */
const BuiltinFormatString FrequencyConverterFormats[] =  {
   /* i18n-hint: Name of display format that shows frequency in hertz */
   {
      _("Hz"),
         /* i18n-hint: Format string for displaying frequency in hertz. Change 
         * the decimal point for your locale. Don't change the numbers. */
         _("0100000.0100 Hz")
   },

   {
      _("kHz"),
         /* i18n-hint: Format string for displaying frequency in kilohertz. Change 
         * the decimal point for your locale. Don't change the numbers. */
         _("0100.01000 kHz|0.001")
   },
};

/** \brief array of formats the control knows about internally
 *  array of string pairs for name of the format and the format string
 *  needed to create that format output. This is used for the pop-up
 *  list of formats to choose from in the control. */
const BuiltinFormatString BandwidthConverterFormats[] = {
   {
   /* i18n-hint: Name of display format that shows log of frequency
    * in octaves */
   _("octaves"),
   /* i18n-hint: Format string for displaying log of frequency in octaves.
    * Change the decimal points for your locale. Don't change the numbers. */
   // Scale factor is 1 / ln (2)
   _("100.01000 octaves|1.442695041")
   },

   {
   /* i18n-hint: Name of display format that shows log of frequency
    * in semitones and cents */
   _("semitones + cents"),
   /* i18n-hint: Format string for displaying log of frequency in semitones
    * and cents.
    * Change the decimal points for your locale. Don't change the numbers. */
   // Scale factor is 12 / ln (2)
   _("1000 semitones .0100 cents|17.312340491")
   },
   
   {
   /* i18n-hint: Name of display format that shows log of frequency
    * in decades */
   _("decades"),
   /* i18n-hint: Format string for displaying log of frequency in decades.
    * Change the decimal points for your locale. Don't change the numbers. */
   // Scale factor is 1 / ln (10)
   _("10.01000 decades|0.434294482")
   },
};

}

//
// ----------------------------------------------------------------------------
// NumericConverter Class
// ----------------------------------------------------------------------------
//
NumericConverter::NumericConverter(Type type,
                                   const wxString & formatName,
                                   double value,
                                   double sampleRate)
{
   ResetMinValue();
   ResetMaxValue();
   mInvalidValue = -1.0;

   mDefaultNdx = 0;

   mType = type;
   switch (type) {
      case TIME:
         mBuiltinFormatStrings = TimeConverterFormats;
         mNBuiltins = sizeof(TimeConverterFormats) /
            sizeof (TimeConverterFormats[0]);
         mDefaultNdx = 4; // Default to "hh:mm:ss + milliseconds".
         break;
      case FREQUENCY:
         mBuiltinFormatStrings = FrequencyConverterFormats;
         mNBuiltins = sizeof(FrequencyConverterFormats) /
            sizeof (FrequencyConverterFormats[0]);
         break;
      case BANDWIDTH:
         mBuiltinFormatStrings = BandwidthConverterFormats;
         mNBuiltins = sizeof(BandwidthConverterFormats) /
            sizeof(BandwidthConverterFormats[0]);
      default:
         break;
   }

   mPrefix = wxT("");
   mValueTemplate = wxT("");
   mValueMask = wxT("");
   mValueString = wxT("");

   mScalingFactor = 1.0f;
   mSampleRate = 1.0f;
   mNtscDrop = false;

   mFocusedDigit = 0;

   mValue = value; // used in SetSampleRate, reassigned later

   SetSampleRate(sampleRate);
   SetFormatName(formatName);
   SetValue(value); // mValue got overridden to -1 in ControlsToValue(), reassign
}

void NumericConverter::ParseFormatString( const wxString & format)
{
   mPrefix = wxT("");
   mFields.Clear();
   mDigits.Clear();
   mScalingFactor = 1.0;

   bool inFrac = false;
   int fracMult = 1;
   int numWholeFields = 0;
   int numFracFields = 0;
   wxString numStr;
   wxString delimStr;
   unsigned int i;

   mNtscDrop = false;
   for(i=0; i<format.Length(); i++) {
      bool handleDelim = false;
      bool handleNum = false;

      if (format[i] == '|') {
         wxString remainder = format.Right(format.Length() - i - 1);

         if (remainder == wxT("#"))
            mScalingFactor = mSampleRate;
         else if (remainder == wxT("N")) {
            mNtscDrop = true;
         }
         else
            remainder.ToDouble(&mScalingFactor);
         i = format.Length()-1; // force break out of loop
         if (delimStr != wxT(""))
            handleDelim = true;
         if (numStr != wxT(""))
            handleNum = true;
      }
      else if ((format[i] >= '0' && format[i] <='9') ||
          format[i] == wxT('*') || format[i] == wxT('#')) {
         numStr += format[i];
         if (delimStr != wxT(""))
            handleDelim = true;
      }
      else {
         delimStr += format[i];
         if (numStr != wxT(""))
            handleNum = true;
      }

      if (i == format.Length() - 1) {
         if (numStr != wxT(""))
            handleNum = true;
         if (delimStr != wxT(""))
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
         if (numStr.GetChar(0)=='0' && numStr.Length()>1)
            zeropad = true;

         // Hack: always zeropad
         zeropad = true;

         if (inFrac) {
            int base = fracMult * range;
            mFields.Add(NumericField(inFrac, base, range, zeropad));
            fracMult *= range;
            numFracFields++;
         }
         else {
            unsigned int j;
            for(j=0; j<mFields.GetCount(); j++)
               mFields[j].base *= range;
            mFields.Add(NumericField(inFrac, 1, range, zeropad));
            numWholeFields++;
         }
         numStr = wxT("");
      }

      if (handleDelim) {
         bool goToFrac = false;

         if (!inFrac && delimStr[delimStr.Length()-1]=='.') {
            goToFrac = true;
            if (delimStr.Length() > 1)
               delimStr = delimStr.BeforeLast('.');
         }

         if (inFrac) {
            if (numFracFields == 0) {
               // Should never happen
               return;
            }
            if (handleNum && numFracFields > 1)
               mFields[mFields.GetCount()-2].label = delimStr;
            else
               mFields[mFields.GetCount()-1].label = delimStr;
         }
         else {
            if (numWholeFields == 0)
               mPrefix = delimStr;
            else {
               mFields[numWholeFields-1].label = delimStr;
            }
         }

         if (goToFrac)
            inFrac = true;
         delimStr = wxT("");
      }
   }

   for(i=0; i<mFields.GetCount(); i++) {
      mFields[i].CreateDigitFormatStr();
   }

   int pos = 0;
   int j;
   mValueMask = wxT("");
   mValueTemplate = wxT("");

   mValueTemplate += mPrefix;
   for(j=0; j<(int)mPrefix.Length(); j++)
      mValueMask += wxT(".");
   pos += mPrefix.Length();

   for(i=0; i<mFields.GetCount(); i++) {
      mFields[i].pos = pos;

      for(j=0; j<mFields[i].digits; j++) {
         mDigits.Add(DigitInfo(i, j, pos, wxRect()));
         mValueTemplate += wxT("0");
         mValueMask += wxT("0");
         pos++;
      }

      pos += mFields[i].label.Length();
      mValueTemplate += mFields[i].label;
      for(j=0; j<(int)mFields[i].label.Length(); j++)
         mValueMask += wxT(".");
   }
}

void NumericConverter::PrintDebugInfo()
{
   unsigned int i;

   printf("%s", (const char *)mPrefix.mb_str());

   for(i=0; i<mFields.GetCount(); i++) {
      if (mFields[i].frac) {
         printf("(t * %d) %% %d '%s' ",
                mFields[i].base,
                mFields[i].range,
                (const char *)mFields[i].label.mb_str());

      }
      else {
         printf("(t / %d) %% %d '%s' ",
                mFields[i].base,
                mFields[i].range,
                (const char *)mFields[i].label.mb_str());
      }
   }

   printf("\n");
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
   //rawValue = 4.9995f; Only for testing!
   if (mType == TIME)
      rawValue =
         floor(rawValue * mSampleRate + (nearest ? 0.5f : 0.0f))
            / mSampleRate; // put on a sample
   double theValue =
      rawValue * mScalingFactor + .000001; // what's this .000001 for? // well, no log of 0
   sampleCount t_int;
   bool round = true;
   // We round on the last field.  If we have a fractional field we round using it.
   // Otherwise we round to nearest integer.
   for(unsigned int i=0; i<mFields.GetCount(); i++) {
      if (mFields[i].frac)
         round = false;
   }
   if (theValue < 0)
      t_int = -1;
   else if(round)
      t_int = sampleCount(theValue + (nearest ? 0.5f : 0.0f));
   else
   {
      wxASSERT( mFields[mFields.GetCount()-1].frac );
      theValue += (nearest ? 0.5f : 0.0f) / mFields[mFields.GetCount()-1].base;
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

   for(i=0; i<mFields.GetCount(); i++) {
      int value = -1;

      if (mFields[i].frac) {
         // JKC: This old code looks bogus to me.
         // The rounding is not propogating to earlier fields in the frac case.
         //value = (int)(t_frac * mFields[i].base + 0.5);  // +0.5 as rounding required
         // I did the rounding earlier.
         if (t_frac >= 0)
            value = (int)(t_frac * mFields[i].base);
         // JKC: TODO: Find out what the range is supposed to do.
         // It looks bogus too.
         //if (mFields[i].range > 0)
         //   value = value % mFields[i].range;
      }
      else {
         if (t_int >= 0) {
            // UNSAFE_SAMPLE_COUNT_TRUNCATION
            // truncation danger!
            value = (t_int.as_long_long() / mFields[i].base);
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
         field = wxString::Format(mFields[i].formatStr, value);
      mValueString += field;
      mValueString += mFields[i].label;
   }
}

void NumericConverter::ControlsToValue()
{
   unsigned int i;
   double t = 0.0;

   if (mFields.GetCount() > 0 &&
      mValueString.Mid(mFields[0].pos, 1) == wxChar('-')) {
      mValue = mInvalidValue;
      return;
   }

   for(i=0; i<mFields.GetCount(); i++) {
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

void NumericConverter::SetFormatName(const wxString & formatName)
{
   SetFormatString(GetBuiltinFormat(formatName));
}

void NumericConverter::SetFormatString(const wxString & formatString)
{
   mFormatString = formatString;
   ParseFormatString(mFormatString);
   ValueToControls();
   ControlsToValue();
}

void NumericConverter::SetSampleRate(double sampleRate)
{
   mSampleRate = sampleRate;
   ParseFormatString(mFormatString);
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

wxString NumericConverter::GetFormatString()
{
   return mFormatString;
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

wxString NumericConverter::GetBuiltinName(const int index)
{
   if (index >= 0 && index < GetNumBuiltins())
      return mBuiltinFormatStrings[index].name;

   return wxEmptyString;
}

wxString NumericConverter::GetBuiltinFormat(const int index)
{
   if (index >= 0 && index < GetNumBuiltins())
      return mBuiltinFormatStrings[index].formatStr;

   return wxEmptyString;
}

wxString NumericConverter::GetBuiltinFormat(const wxString &name)
{
   int ndx = mDefaultNdx;
   int i;

   for (i = 0; i < GetNumBuiltins(); i++) {
      if (name == GetBuiltinName(i)) {
         ndx = i;
         break;
      }
   }

   return GetBuiltinFormat(ndx);
}

wxString NumericConverter::GetString()
{
   ValueToControls();

   return mValueString;
}

void NumericConverter::Increment()
{
   mFocusedDigit = mDigits.GetCount() - 1;
   Adjust(1, 1);
}

void NumericConverter::Decrement()
{
   mFocusedDigit = mDigits.GetCount() - 1;
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
      for (size_t i = 0; i < mFields.GetCount(); i++)
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

#define ID_MENU 9800

// Custom events

DEFINE_EVENT_TYPE(EVT_TIMETEXTCTRL_UPDATED)
DEFINE_EVENT_TYPE(EVT_FREQUENCYTEXTCTRL_UPDATED)
DEFINE_EVENT_TYPE(EVT_BANDWIDTHTEXTCTRL_UPDATED)

BEGIN_EVENT_TABLE(NumericTextCtrl, wxControl)
   EVT_ERASE_BACKGROUND(NumericTextCtrl::OnErase)
   EVT_PAINT(NumericTextCtrl::OnPaint)
   EVT_CONTEXT_MENU(NumericTextCtrl::OnContext)
   EVT_MOUSE_EVENTS(NumericTextCtrl::OnMouse)
   EVT_KEY_DOWN(NumericTextCtrl::OnKeyDown)
   EVT_KEY_UP(NumericTextCtrl::OnKeyUp)
   EVT_SET_FOCUS(NumericTextCtrl::OnFocus)
   EVT_KILL_FOCUS(NumericTextCtrl::OnFocus)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, NumericTextCtrl::OnCaptureKey)
END_EVENT_TABLE()

IMPLEMENT_CLASS(NumericTextCtrl, wxControl)

NumericTextCtrl::NumericTextCtrl(NumericConverter::Type type,
                           wxWindow *parent,
                           wxWindowID id,
                           const wxString &formatName,
                           double timeValue,
                           double sampleRate,
                           const wxPoint &pos,
                           const wxSize &size,
                           bool autoPos):
   wxControl(parent, id, pos, size, wxSUNKEN_BORDER | wxWANTS_CHARS),
   NumericConverter(type, formatName, timeValue, sampleRate),
   mBackgroundBitmap{},
   mDigitFont{},
   mLabelFont{},
   mLastField(1),
   mAutoPos(autoPos)
   , mType(type)
{
   mAllowInvalidValue = false;

   mDigitBoxW = 10;
   mDigitBoxH = 16;

   mReadOnly = false;
   mMenuEnabled = true;
   mButtonWidth = 9;

   Layout();
   Fit();
   ValueToControls();
   //PRL -- would this fix the following?
   //ValueToControls();

   //mchinen - aug 15 09 - this seems to put the mValue back to zero, and do nothing else.
   //ControlsToValue();

   mScrollRemainder = 0.0;

#if wxUSE_ACCESSIBILITY
   SetLabel(wxT(""));
   SetName(wxT(""));
   SetAccessible(safenew NumericTextCtrlAx(this));
#endif
}

NumericTextCtrl::~NumericTextCtrl()
{
}

// Set the focus to the first (left-most) non-zero digit
// If all digits are zero, the right-most position is focused
// If all digits are hyphens (invalid), the left-most position is focused
void NumericTextCtrl::UpdateAutoFocus()
{
   if (!mAutoPos)
      return;

   mFocusedDigit = 0;
   while (mFocusedDigit < ((int)mDigits.GetCount() - 1)) {
      wxChar dgt = mValueString[mDigits[mFocusedDigit].pos];
      if (dgt != '0') {
         break;
      }
      mFocusedDigit++;
   }
}

void NumericTextCtrl::SetFormatName(const wxString & formatName)
{
   SetFormatString(GetBuiltinFormat(formatName));
}

void NumericTextCtrl::SetFormatString(const wxString & formatString)
{
   NumericConverter::SetFormatString(formatString);
   Layout();
   Fit();
   ValueToControls();
   ControlsToValue();
   UpdateAutoFocus();
}

void NumericTextCtrl::SetSampleRate(double sampleRate)
{
   NumericConverter::SetSampleRate(sampleRate);
   Layout();
   Fit();
   ValueToControls();
   ControlsToValue();
}

void NumericTextCtrl::SetValue(double newValue)
{
   NumericConverter::SetValue(newValue);
   ValueToControls();
   ControlsToValue();
}

void NumericTextCtrl::SetReadOnly(bool readOnly)
{
   mReadOnly = readOnly;
}

void NumericTextCtrl::EnableMenu(bool enable)
{
#if wxUSE_TOOLTIPS
   wxString tip(_("(Use context menu to change format.)"));
   if (enable)
      SetToolTip(tip);
   else {
      wxToolTip *tt = GetToolTip();
      if (tt && tt->GetTip() == tip)
         SetToolTip(NULL);
   }
#endif
   mMenuEnabled = enable;
   mButtonWidth = enable ? 9 : 0;
   Layout();
   Fit();
}

void NumericTextCtrl::SetInvalidValue(double invalidValue)
{
   const bool wasInvalid = mAllowInvalidValue && (mValue == mInvalidValue);
   mAllowInvalidValue = true;
   mInvalidValue = invalidValue;
   if (wasInvalid)
      SetValue(invalidValue);
}

bool NumericTextCtrl::Layout()
{
   unsigned int i, j;
   int x, pos;

   wxMemoryDC memDC;

   // Placeholder bitmap so the memDC has something to reference
   mBackgroundBitmap = std::make_unique<wxBitmap>(1, 1);
   memDC.SelectObject(*mBackgroundBitmap);

   mDigits.Clear();

   mBorderLeft = 1;
   mBorderTop = 1;
   mBorderRight = 1;
   mBorderBottom = 1;

   int fontSize = 4;
   wxCoord strW, strH;
   wxString exampleText = wxT("0");

   // Keep making the font bigger until it's too big, then subtract one.
   memDC.SetFont(wxFont(fontSize, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
   memDC.GetTextExtent(exampleText, &strW, &strH);
   while (strW <= mDigitBoxW && strH <= mDigitBoxH) {
      fontSize++;
      memDC.SetFont(wxFont(fontSize, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
      memDC.GetTextExtent(exampleText, &strW, &strH);
   }
   fontSize--;

   mDigitFont = std::make_unique<wxFont>(fontSize, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   memDC.SetFont(*mDigitFont);
   memDC.GetTextExtent(exampleText, &strW, &strH);
   mDigitW = strW;
   mDigitH = strH;

   // The label font should be a little smaller
   fontSize--;
   mLabelFont = std::make_unique<wxFont>(fontSize, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);

   // Figure out the x-position of each field and label in the box
   x = mBorderLeft;
   pos = 0;

   memDC.SetFont(*mLabelFont);
   memDC.GetTextExtent(mPrefix, &strW, &strH);
   x += strW;
   pos += mPrefix.Length();

   for(i=0; i<mFields.GetCount(); i++) {
      mFields[i].fieldX = x;
      for(j=0; j<(unsigned int)mFields[i].digits; j++) {
         mDigits.Add(DigitInfo(i, j, pos, wxRect(x, mBorderTop,
                                                 mDigitBoxW, mDigitBoxH)));
         x += mDigitBoxW;
         pos++;
      }

      mFields[i].labelX = x;
      memDC.GetTextExtent(mFields[i].label, &strW, &strH);
      pos += mFields[i].label.Length();
      x += strW;
      mFields[i].fieldW = x;
   }

   mWidth = x + mBorderRight;
   mHeight = mDigitBoxH + mBorderTop + mBorderBottom;

   // Draw the background bitmap - it contains black boxes where
   // all of the digits go and all of the other text

   wxBrush Brush;

   mBackgroundBitmap = std::make_unique<wxBitmap>(mWidth + mButtonWidth, mHeight);
   memDC.SelectObject(*mBackgroundBitmap);

   memDC.SetBrush(*wxLIGHT_GREY_BRUSH);
   memDC.SetPen(*wxTRANSPARENT_PEN);
   memDC.DrawRectangle(0, 0, mWidth + mButtonWidth, mHeight);

   int numberBottom = mBorderTop + (mDigitBoxH - mDigitH)/2 + mDigitH;

   memDC.GetTextExtent(wxT("0"), &strW, &strH);
   int labelTop = numberBottom - strH;

   memDC.SetTextForeground(*wxBLACK);
   memDC.SetTextBackground(*wxLIGHT_GREY);
   memDC.DrawText(mPrefix, mBorderLeft, labelTop);

   theTheme.SetBrushColour( Brush, clrTimeBack );
   memDC.SetBrush(Brush);
   memDC.SetBrush(*wxLIGHT_GREY_BRUSH);
   for(i=0; i<mDigits.GetCount(); i++)
      memDC.DrawRectangle(mDigits[i].digitBox);
   memDC.SetBrush( wxNullBrush );

   for(i=0; i<mFields.GetCount(); i++)
      memDC.DrawText(mFields[i].label,
                     mFields[i].labelX, labelTop);

   if (mMenuEnabled) {
      wxRect r(mWidth, 0, mButtonWidth - 1, mHeight - 1);
      AColor::Bevel(memDC, true, r);
      memDC.SetBrush(*wxBLACK_BRUSH);
      memDC.SetPen(*wxBLACK_PEN);
      AColor::Arrow(memDC,
                    mWidth + 1,
                    (mHeight / 2) - 2,
                    mButtonWidth - 2);
   }
   return true;
}

void NumericTextCtrl::Fit()
{
   wxSize sz = GetSize();
   wxSize csz = GetClientSize();

   sz.x = mButtonWidth + mWidth + (sz.x - csz.x);
   sz.y = mHeight + (sz.y - csz.y);

   SetInitialSize(sz);
}

void NumericTextCtrl::OnErase(wxEraseEvent & WXUNUSED(event))
{
   // Ignore it to prevent flashing
}

void NumericTextCtrl::OnPaint(wxPaintEvent & WXUNUSED(event))
{
   wxPaintDC dc(this);
   bool focused = (FindFocus() == this);

   dc.DrawBitmap(*mBackgroundBitmap, 0, 0);

   wxPen   Pen;
   wxBrush Brush;
   if (focused) {
      theTheme.SetPenColour( Pen, clrTimeFontFocus );
      dc.SetPen(Pen);
      dc.SetBrush(*wxTRANSPARENT_BRUSH);
      dc.DrawRectangle(0, 0, mWidth, mHeight);
      dc.SetPen( wxNullPen );
   }

   dc.SetFont(*mDigitFont);
   dc.SetTextForeground(theTheme.Colour( clrTimeFont ));
   dc.SetTextBackground(theTheme.Colour( clrTimeBack ));

   dc.SetPen(*wxTRANSPARENT_PEN);
   theTheme.SetBrushColour( Brush , clrTimeBackFocus );
   dc.SetBrush( Brush );

   int i;
   for(i=0; i<(int)mDigits.GetCount(); i++) {
      wxRect box = mDigits[i].digitBox;
      if (focused && mFocusedDigit == i) {
         dc.DrawRectangle(box);
         dc.SetTextForeground(theTheme.Colour( clrTimeFontFocus ));
         dc.SetTextBackground(theTheme.Colour( clrTimeBackFocus ));
      }
      int pos = mDigits[i].pos;
      wxString digit = mValueString.Mid(pos, 1);
      int x = box.x + (mDigitBoxW - mDigitW)/2;
      int y = box.y + (mDigitBoxH - mDigitH)/2;
      dc.DrawText(digit, x, y);
      if (focused && mFocusedDigit == i) {
         dc.SetTextForeground(theTheme.Colour( clrTimeFont ));
         dc.SetTextBackground(theTheme.Colour( clrTimeBack ));
      }
   }
   dc.SetPen( wxNullPen );
   dc.SetBrush( wxNullBrush );
}

void NumericTextCtrl::OnContext(wxContextMenuEvent &event)
{
   wxMenu menu;
   int i;

   if (!mMenuEnabled) {
      event.Skip();
      return;
   }

   SetFocus();

   int currentSelection = -1;
   for (i = 0; i < GetNumBuiltins(); i++) {
      menu.AppendRadioItem(ID_MENU + i, GetBuiltinName(i));
      if (mFormatString == GetBuiltinFormat(i)) {
         menu.Check(ID_MENU + i, true);
         currentSelection = i;
      }
   }

   PopupMenu(&menu, wxPoint(0, 0));

   // This used to be in an EVT_MENU() event handler, but GTK
   // is sensitive to what is done within the handler if the
   // user happens to check the first menuitem and then is 
   // moving down the menu when the ...CTRL_UPDATED event
   // handler kicks in.
   for (i = 0; i < GetNumBuiltins(); i++) {
      if (menu.IsChecked(ID_MENU + i) && i != currentSelection) {
         SetFormatString(GetBuiltinFormat(i));
      
         int eventType = 0;
         switch (mType) {
            case NumericConverter::TIME:
               eventType = EVT_TIMETEXTCTRL_UPDATED;
               break;
            case NumericConverter::FREQUENCY:
               eventType = EVT_FREQUENCYTEXTCTRL_UPDATED;
               break;
            case NumericConverter::BANDWIDTH:
               eventType = EVT_BANDWIDTHTEXTCTRL_UPDATED;
               break;
            default:
               wxASSERT(false);
               break;
         }
      
         wxCommandEvent e(eventType, GetId());
         e.SetInt(i);
         e.SetString(GetBuiltinName(i));
         GetParent()->GetEventHandler()->AddPendingEvent(e);
      }
   }

}

void NumericTextCtrl::OnMouse(wxMouseEvent &event)
{
   if (event.LeftDown() && event.GetX() >= mWidth) {
      wxContextMenuEvent e;
      OnContext(e);
   }
   else if (event.LeftDown()) {
      SetFocus();

      int bestDist = 9999;
      unsigned int i;

      mFocusedDigit = 0;
      for(i=0; i<mDigits.GetCount(); i++) {
         int dist = abs(event.m_x - (mDigits[i].digitBox.x +
                                     mDigits[i].digitBox.width/2));
         if (dist < bestDist) {
            mFocusedDigit = i;
            bestDist = dist;
         }
      }

      Refresh(false);
   }
   else if (event.RightDown() && mMenuEnabled) {
      wxContextMenuEvent e;
      OnContext(e);
   }
   else if(!mReadOnly && event.m_wheelRotation != 0 ) {
      double steps = event.m_wheelRotation /
         (event.m_wheelDelta > 0 ? (double)event.m_wheelDelta : 120.0) +
         mScrollRemainder;
      mScrollRemainder = steps - floor(steps);
      steps = floor(steps);

      Adjust((int)fabs(steps), steps < 0.0 ? -1 : 1);
      Updated();
   }
}

void NumericTextCtrl::OnFocus(wxFocusEvent &event)
{
   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      AudacityProject::ReleaseKeyboard(this);
   }
   else {
      AudacityProject::CaptureKeyboard(this);
   }

   Refresh(false);
}

void NumericTextCtrl::OnCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   int keyCode = kevent->GetKeyCode();

   // Convert numeric keypad entries.
   if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9))
      keyCode -= WXK_NUMPAD0 - '0';

   switch (keyCode)
   {
      case WXK_BACK:
      case WXK_LEFT:
      case WXK_RIGHT:
      case WXK_HOME:
      case WXK_END:
      case WXK_UP:
      case WXK_DOWN:
      case WXK_TAB:
      case WXK_RETURN:
      case WXK_NUMPAD_ENTER:
      case WXK_DELETE:
         return;

      default:
         if (keyCode >= '0' && keyCode <= '9')
            return;
   }

   event.Skip();

   return;
}

void NumericTextCtrl::OnKeyUp(wxKeyEvent &event)
{
   int keyCode = event.GetKeyCode();

   event.Skip(true);

   if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9))
      keyCode -= WXK_NUMPAD0 - '0';

   if ((keyCode >= '0' && keyCode <= '9') ||
       (keyCode == WXK_DELETE) ||
       (keyCode == WXK_BACK) ||
       (keyCode == WXK_UP) ||
       (keyCode == WXK_DOWN)) {
      Updated(true);
   }
}

void NumericTextCtrl::OnKeyDown(wxKeyEvent &event)
{
   if (mDigits.GetCount() == 0)
   {
      mFocusedDigit = 0;
      return;
   }

   event.Skip(false);

   int keyCode = event.GetKeyCode();
   int digit = mFocusedDigit;

   if (mFocusedDigit < 0)
      mFocusedDigit = 0;
   if (mFocusedDigit >= (int)mDigits.GetCount())
      mFocusedDigit = mDigits.GetCount()-1;

   // Convert numeric keypad entries.
   if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9))
      keyCode -= WXK_NUMPAD0 - '0';

   if (!mReadOnly && (keyCode >= '0' && keyCode <= '9')) {
      int digitPosition = mDigits[mFocusedDigit].pos;
      if (mValueString[digitPosition] == wxChar('-')) {
         mValue = std::max(mMinValue, std::min(mMaxValue, 0.0));
         ValueToControls();
         // Beware relocation of the string
         digitPosition = mDigits[mFocusedDigit].pos;
      }
      mValueString[digitPosition] = wxChar(keyCode);
      ControlsToValue();
      Refresh();// Force an update of the control. [Bug 1497]
      ValueToControls();
      mFocusedDigit = (mFocusedDigit + 1) % (mDigits.GetCount());
      Updated();
   }

   else if (!mReadOnly && keyCode == WXK_DELETE) {
      if (mAllowInvalidValue)
         SetValue(mInvalidValue);
   }

   else if (!mReadOnly && keyCode == WXK_BACK) {
      // Moves left, replaces that char with '0', stays there...
      mFocusedDigit--;
      mFocusedDigit += mDigits.GetCount();
      mFocusedDigit %= mDigits.GetCount();
      wxString::reference theDigit = mValueString[mDigits[mFocusedDigit].pos];
      if (theDigit != wxChar('-'))
         theDigit = '0';
      ControlsToValue();
      Refresh();// Force an update of the control. [Bug 1497]
      ValueToControls();
      Updated();
   }

   else if (keyCode == WXK_LEFT) {
      mFocusedDigit--;
      mFocusedDigit += mDigits.GetCount();
      mFocusedDigit %= mDigits.GetCount();
      Refresh();
   }

   else if (keyCode == WXK_RIGHT) {
      mFocusedDigit++;
      mFocusedDigit %= mDigits.GetCount();
      Refresh();
   }

   else if (keyCode == WXK_HOME) {
      mFocusedDigit = 0;
      Refresh();
   }

   else if (keyCode == WXK_END) {
      mFocusedDigit = mDigits.GetCount() - 1;
      Refresh();
   }

   else if (!mReadOnly && keyCode == WXK_UP) {
      Adjust(1, 1);
      Updated();
   }

   else if (!mReadOnly && keyCode == WXK_DOWN) {
      Adjust(1, -1);
      Updated();
   }

   else if (keyCode == WXK_TAB) {
      Navigate(event.ShiftDown()
               ? wxNavigationKeyEvent::IsBackward
               : wxNavigationKeyEvent::IsForward);
   }

   else if (keyCode == WXK_RETURN || keyCode == WXK_NUMPAD_ENTER) {
      wxTopLevelWindow *tlw = wxDynamicCast(wxGetTopLevelParent(this), wxTopLevelWindow);
      wxWindow *def = tlw->GetDefaultItem();
      if (def && def->IsEnabled()) {
         wxCommandEvent cevent(wxEVT_COMMAND_BUTTON_CLICKED,
                               def->GetId());
         GetParent()->GetEventHandler()->ProcessEvent(cevent);
      }
   }

   else {
      event.Skip();
      return;
   }

   if (digit != mFocusedDigit) {
      SetFieldFocus(mFocusedDigit);
   }
}

void NumericTextCtrl::SetFieldFocus(int  digit)
{
#if wxUSE_ACCESSIBILITY
   if (mDigits.GetCount() == 0)
   {
      mFocusedDigit = 0;
      return;
   }
   mFocusedDigit = digit;
   mLastField = mDigits[mFocusedDigit].field + 1;

   // This looks strange (and it is), but it was the only way I could come
   // up with that allowed Jaws, Window-Eyes, and NVDA to read the control
   // somewhat the same.  See NumericTextCtrlAx below for even more odd looking
   // hackery.
   //
   // If you change SetFieldFocus(), Updated(), or NumericTextCtrlAx, make sure
   // you test with Jaws, Window-Eyes, and NVDA.
   GetAccessible()->NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
                                this,
                                wxOBJID_CLIENT,
                                0);
   GetAccessible()->NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
                                this,
                                wxOBJID_CLIENT,
                                mFocusedDigit + 1);
#endif
}

void NumericTextCtrl::Updated(bool keyup /* = false */)
{
   wxCommandEvent event(wxEVT_COMMAND_TEXT_UPDATED, GetId());

   // This will give listeners the ability to do tasks when the
   // update has been completed, like when the UP ARROW has been
   // held down and is finally released.
   event.SetInt(keyup);
   event.SetEventObject(this);
   GetEventHandler()->ProcessEvent(event);

#if wxUSE_ACCESSIBILITY
   if (!keyup) {
      GetAccessible()->NotifyEvent(wxACC_EVENT_OBJECT_NAMECHANGE,
                                   this,
                                   wxOBJID_CLIENT,
                                   mFocusedDigit + 1);
      SetFieldFocus(mFocusedDigit);
   }
#endif
}

void NumericTextCtrl::ValueToControls()
{
   const wxString previousValueString = mValueString;
   NumericConverter::ValueToControls(mValue);
   if (mValueString != previousValueString) {
      // Doing this only when needed is an optimization.
      // NumerixTextCtrls are used in the selection bar at the bottom
      // of Audacity, and are updated at high frequency through
      // SetValue() when Audacity is playing. This consumes a
      // significant amount of CPU. Typically, when a track is
      // playing, only one of the NumericTextCtrl actually changes
      // (the audio position). We save CPU by updating the control
      // only when needed.
      Refresh(false);
   }
}


void NumericTextCtrl::ControlsToValue()
{
   NumericConverter::ControlsToValue();
}

#if wxUSE_ACCESSIBILITY

NumericTextCtrlAx::NumericTextCtrlAx(NumericTextCtrl *ctrl)
:  wxWindowAccessible(ctrl)
{
   mCtrl = ctrl;
   mLastField = -1;
   mLastDigit = -1;
}

NumericTextCtrlAx::~NumericTextCtrlAx()
{
}

// Performs the default action. childId is 0 (the action for this object)
// or > 0 (the action for a child).
// Return wxACC_NOT_SUPPORTED if there is no default action for this
// window (e.g. an edit control).
wxAccStatus NumericTextCtrlAx::DoDefaultAction(int WXUNUSED(childId))
{
   return wxACC_NOT_SUPPORTED;
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus NumericTextCtrlAx::GetChild(int childId, wxAccessible **child)
{
   if (childId == wxACC_SELF) {
      *child = this;
   }
   else {
      *child = NULL;
   }

   return wxACC_OK;
}

// Gets the number of children.
wxAccStatus NumericTextCtrlAx::GetChildCount(int *childCount)
{
   *childCount = mCtrl->mDigits.GetCount();

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for
// a child).  Return wxACC_OK even if there is no action. actionName
// is the action, or the empty string if there is no action.  The
// retrieved string describes the action that is performed on an
// object, not what the object does as a result. For example, a
// toolbar button that prints a document has a default action of
// "Press" rather than "Prints the current document."
wxAccStatus NumericTextCtrlAx::GetDefaultAction(int WXUNUSED(childId), wxString *actionName)
{
   actionName->Clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus NumericTextCtrlAx::GetDescription(int WXUNUSED(childId), wxString *description)
{
   description->Clear();

   return wxACC_OK;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus NumericTextCtrlAx::GetFocus(int *childId, wxAccessible **child)
{
   *childId = mCtrl->GetFocusedDigit();
   *child = this;

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus NumericTextCtrlAx::GetHelpText(int WXUNUSED(childId), wxString *helpText)
{
// removed help text, as on balance it's more of an irritation than useful
#if 0    // was #if wxUSE_TOOLTIPS
   wxToolTip *pTip = mCtrl->GetToolTip();
   if (pTip) {
      *helpText = pTip->GetTip();
   }

   return wxACC_OK;
#else
   helpText->Clear();

   return wxACC_NOT_SUPPORTED;
#endif
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus NumericTextCtrlAx::GetKeyboardShortcut(int WXUNUSED(childId), wxString *shortcut)
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus NumericTextCtrlAx::GetLocation(wxRect & rect, int elementId)
{
   if ((elementId != wxACC_SELF) &&
         // We subtract 1, below, and need to avoid neg index to mDigits.
         (elementId > 0))
   {
//      rect.x += mCtrl->mFields[elementId - 1].fieldX;
//      rect.width =  mCtrl->mFields[elementId - 1].fieldW;
        rect = mCtrl->mDigits[elementId - 1].digitBox;
        rect.SetPosition(mCtrl->ClientToScreen(rect.GetPosition()));
   }
   else
   {
      rect = mCtrl->GetRect();
      rect.SetPosition(mCtrl->GetParent()->ClientToScreen(rect.GetPosition()));
   }

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus NumericTextCtrlAx::GetName(int childId, wxString *name)
{
   // Slightly messy trick to save us some prefixing.
   NumericFieldArray & mFields = mCtrl->mFields;

   wxString value = mCtrl->GetString();
   int field = mCtrl->GetFocusedField();

   // Return the entire string including the control label
   // when the requested child ID is wxACC_SELF.  (Mainly when
   // the control gets the focus.)
   if ((childId == wxACC_SELF) ||
         // We subtract 1 from childId in the other cases below, and
         // need to avoid neg index to mDigits, so funnel into this clause.
         (childId < 1))
   {
      *name = mCtrl->GetName();
      if (name->IsEmpty()) {
         *name = mCtrl->GetLabel();
      }

      *name += wxT(" ") +
               mCtrl->GetString();
   }
   // The user has moved from one field of the time to another so
   // report the value of the field and the field's label.
   else if (mLastField != field) {
      wxString label = mFields[field - 1].label;
      int cnt = mFields.GetCount();
      wxString decimal = wxLocale::GetInfo(wxLOCALE_DECIMAL_POINT, wxLOCALE_CAT_NUMBER);

      // If the NEW field is the last field, then check it to see if
      // it represents fractions of a second.
      // PRL: click a digit of the control and use left and right arrow keys
      // to exercise this code
      const bool isTime = (mCtrl->mType == NumericTextCtrl::TIME);
      if (field > 1 && field == cnt) {
         if (mFields[field - 2].label == decimal) {
            int digits = mFields[field - 1].digits;
            if (digits == 2) {
               if (isTime)
                  label = _("centiseconds");
               else {
                  // other units
                  // PRL:  does this create translation problems?
                  label = _("hundredths of ");
                  label += mFields[field - 1].label;
               }
            }
            else if (digits == 3) {
               if (isTime)
                  label = _("milliseconds");
               else {
                  // other units
                  // PRL:  does this create translation problems?
                  label = _("thousandths of ");
                  label += mFields[field - 1].label;
               }
            }
         }
      }
      // If the field following this one represents fractions of a
      // second then use that label instead of the decimal point.
      else if (label == decimal && field == cnt - 1) {
         label = mFields[field].label;
      }

      *name = mFields[field - 1].str +
              wxT(" ") +
              label +
              wxT(", ") +     // comma inserts a slight pause
              mCtrl->GetString().at(mCtrl->mDigits[childId - 1].pos);
      mLastField = field;
      mLastDigit = childId;
   }
   // The user has moved from one digit to another within a field so
   // just report the digit under the cursor.
   else if (mLastDigit != childId) {
      *name = mCtrl->GetString().at(mCtrl->mDigits[childId - 1].pos);
      mLastDigit = childId;
   }
   // The user has updated the value of a field, so report the field's
   // value only.
   else if (field > 0)
   {
      *name = mFields[field - 1].str;
   }

   return wxACC_OK;
}

// Returns a role constant.
wxAccStatus NumericTextCtrlAx::GetRole(int WXUNUSED(childId), wxAccRole *role)
{
   *role = wxROLE_SYSTEM_STATICTEXT;
   return wxACC_OK;
}

// Gets a variant representing the selected children
// of this object.
// Acceptable values:
// - a null variant (IsNull() returns TRUE)
// - a list variant (GetType() == wxT("list"))
// - an integer representing the selected child element,
//   or 0 if this object is selected (GetType() == wxT("long"))
// - a "void*" pointer to a wxAccessible child object
wxAccStatus NumericTextCtrlAx::GetSelections(wxVariant * WXUNUSED(selections))
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus NumericTextCtrlAx::GetState(int WXUNUSED(childId), long *state)
{
   *state = wxACC_STATE_SYSTEM_FOCUSABLE;
   *state |= (mCtrl == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0);

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus NumericTextCtrlAx::GetValue(int WXUNUSED(childId), wxString * WXUNUSED(strValue))
{
   return wxACC_NOT_IMPLEMENTED;
}

#endif

