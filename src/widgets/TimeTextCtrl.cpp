/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeTextCtrl.cpp

  Dominic Mazzoni


********************************************************************//**

\class TimeTextCtrl 
\brief TimeTextCtrl provides the advanced time formatting control used
in the status bar of Audacity.

  The TimeTextCtrl makes use of a format string to specify the
  exact way that a single time value is split into several fields,
  such as the hh:mm:ss format.  The advantage of this format string
  is that it is very small and compact, but human-readable and
  somewhat intuitive, so that it's easy to add new time layouts
  in the future.  It's also designed to make it easier to add
  i18n support, since the way that times are displayed in different
  languages could conceivably vary a lot.

  The time to be formatted is expressed in seconds, so the format
  string specifies the relationship of each field to the number of
  seconds.

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
  (seconds) is 60.  So, if you give it a number like 3758 (note
  format it as:

    3758 seconds, "*:60:60" -> "1:2:38"

  Note that 3758 = 1*60*60 + 2*60 + 38.

  When TimeTextCtrl formats an integer, you can think of its process
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
  include a scaling factor after a "|", the time will be
  multiplied by this factor before it is formatted.  For example, to
  express the current time in NTSC frames (~29.97 fps), you could
  use the following formatting:

    3758.5 seconds, "*.01000 frames|29.97002997" -> "112642.358 frames"

  Finally there is a further special character that can be used after a "|" 
  and that is "N".  This applies special rule for NTSC drop-frame timecode.

  Summary of format string rules:

  - The characters '0-9', '*', and '#' are numeric.  Any sequence of
    these characters is treated as defining a new field by specifying
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
    decimal point) is treated as a scaling factor.  The time is
    multiplied by this factor before multiplying.
  - The special character 'N' after '|' is only used for NTSC drop-frame.

*******************************************************************//**

\class TimeTextCtrlAx
\brief TimeTextCtrlAx gives the TimeTextCtrl Accessibility.

*******************************************************************//**

\class TimeConverter
\brief TimeConverter has all the time conversion and snapping 
functionality that used to live in TimeTextCtrl.  The idea is to have 
a GUI-less class which can do the conversions, so that we can use it 
in sanpping without having a window created each time.

*//****************************************************************//**

\class BuiltinFormatString
\brief BuiltinFormatString is a structure used in the TimeTextCtrl 
and holds both a descriptive name for the string format and a 
printf inspired style format string, optimised for displaying time in 
different formats.

*//****************************************************************//**

\class TimeField
\brief TimeField is a class used in the TimeTextCtrl

*//****************************************************************//**

\class DigitInfo
\brief DigitInfo is a class used in the TimeTextCtrl

**********************************************************************/


#include "../Audacity.h"
#include "../AudacityApp.h"
#include "TimeTextCtrl.h"
#include "../Sequence.h"   // for sampleCount
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../AColor.h"

#include <math.h>

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
// TimeField Class
// ----------------------------------------------------------------------------
//
class TimeField
{
public:
   TimeField(bool _frac, int _base, int _range, bool _zeropad)
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
WX_DEFINE_OBJARRAY(TimeFieldArray);
WX_DEFINE_OBJARRAY(DigitInfoArray);

//
// ----------------------------------------------------------------------------
// TimeConverter Class
// ----------------------------------------------------------------------------
//
TimeConverter::TimeConverter(const wxString & formatName,
                             double timeValue,
                             double sampleRate)
{
   /* i18n-hint: Name of time display format that shows time in seconds */
   BuiltinFormatStrings[0].name = _("seconds");
   /* i18n-hint: Format string for displaying time in seconds. Change the comma
    * in the middle to the 1000s separator for your locale, and the 'seconds'
    * on the end to the word for seconds. Don't change the numbers. */
   BuiltinFormatStrings[0].formatStr = _("01000,01000 seconds");
   /* i18n-hint: Name of time display format that shows time in hours, minutes
    * and seconds */
   BuiltinFormatStrings[1].name = _("hh:mm:ss");
   /* i18n-hint: Format string for displaying time in hours, minutes and
    * seconds. Change the 'h' to the abbreviation for hours, 'm' to the
    * abbreviation for minutes and 's' to the abbreviation for seconds. Don't
    * change the numbers unless there aren't 60 seconds in a minute in your
    * locale */
   BuiltinFormatStrings[1].formatStr = _("0100 h 060 m 060 s");
   /* i18n-hint: Name of time display format that shows time in days, hours,
    * minutes and seconds */
   BuiltinFormatStrings[2].name = _("dd:hh:mm:ss");
   /* i18n-hint: Format string for displaying time in days, hours, minutes and
    * seconds. Change the 'days' to the word for days, 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes and 's' to the
    * abbreviation for seconds. Don't change the numbers unless there aren't
    * 24 hours in a day in your locale */
   BuiltinFormatStrings[2].formatStr = _("0100 days 024 h 060 m 060 s");
   /* i18n-hint: Name of time display format that shows time in hours,
    * minutes, seconds and hundredths of a second (1/100 second) */
   BuiltinFormatStrings[3].name = _("hh:mm:ss + hundredths");
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and hundredths of a second. Change the 'h' to the abbreviation for hours,
    * 'm' to the abbreviation for minutes and 's' to the abbreviation for seconds (the
    * hundredths are shown as decimal seconds) . Don't change the numbers
    * unless there aren't 60 minutes in an hour in your locale */
   BuiltinFormatStrings[3].formatStr =_("0100 h 060 m 060.0100 s");
   /* i18n-hint: Name of time display format that shows time in hours,
    * minutes, seconds and milliseconds (1/1000 second) */
   BuiltinFormatStrings[4].name = _("hh:mm:ss + milliseconds");
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and milliseconds. Change the 'h' to the abbreviation for hours, 'm' to the
    * abbreviation for minutes and 's' to the abbreviation for seconds (the
    * milliseconds are shown as decimal seconds) . Don't change the numbers
    * unless there aren't 60 minutes in an hour in your locale */
   BuiltinFormatStrings[4].formatStr =_("0100 h 060 m 060.01000 s");
   /* i18n-hint: Name of time display format that shows time in hours,
    * minutes, seconds and samples (at the current project sample rate) */
   BuiltinFormatStrings[5].name = _("hh:mm:ss + samples");
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and samples. Change the 'h' to the abbreviation for hours, 'm' to the
    * abbreviation for minutes, 's' to the abbreviation for seconds and
    * translate samples . Don't change the numbers
    * unless there aren't 60 seconds in a minute in your locale */
   BuiltinFormatStrings[5].formatStr = _("0100 h 060 m 060 s+.# samples");
   /* i18n-hint: Name of time display format that shows time in samples (at the
    * current project sample rate).  For example the number of a sample at 1 
    * second into a recording at 44.1KHz would be 44,100.
    */
   BuiltinFormatStrings[6].name = _("samples");
   /* i18n-hint: Format string for displaying time in samples (lots of samples).
    * Change the ',' to the 1000s separator for your locale, and translate
    * samples. If 1000s aren't a base multiple for your number system, then you
    * can change the numbers to an appropriate one, and put a 0 on the front */
   BuiltinFormatStrings[6].formatStr = _("01000,01000,01000 samples|#");
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at 24 frames per second (commonly used for films) */
   BuiltinFormatStrings[7].name = _("hh:mm:ss + film frames (24 fps)");
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames at 24 frames per second. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames' . Don't change the numbers
    * unless there aren't 60 seconds in a minute in your locale */
   BuiltinFormatStrings[7].formatStr = _("0100 h 060 m 060 s+.24 frames");
   /* i18n-hint: Name of time display format that shows time in frames (lots of
    * frames) at 24 frames per second (commonly used for films) */
   BuiltinFormatStrings[8].name = _("film frames (24 fps)");
   /* i18n-hint: Format string for displaying time in frames at 24 frames per
    * second. Translate 'frames' and leave the rest alone */
   BuiltinFormatStrings[8].formatStr = _("01000,01000 frames|24");
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at NTSC TV drop-frame rate (used for American /
    * Japanese TV, and very odd) */
   BuiltinFormatStrings[9].name = _("hh:mm:ss + NTSC drop frames");
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with NTSC drop frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. Leave the |N alone, it's important! */
   BuiltinFormatStrings[9].formatStr = _("0100 h 060 m 060 s+.30 frames|N");
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at NTSC TV non-drop-frame rate (used for American /
    * Japanese TV, and doesn't quite match wall time */
   BuiltinFormatStrings[10].name = _("hh:mm:ss + NTSC non-drop frames");
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with NTSC drop frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. Leave the | .999000999 alone, 
    * the whole things really is slightly off-speed! */
   BuiltinFormatStrings[10].formatStr = _("0100 h 060 m 060 s+.030 frames| .999000999");
   /* i18n-hint: Name of time display format that shows time in frames at NTSC
    * TV frame rate (used for American / Japanese TV */
   BuiltinFormatStrings[11].name = _("NTSC frames");
   /* i18n-hint: Format string for displaying time in frames with NTSC frames.
    * Translate 'frames' and leave the rest alone. That really is the frame
    * rate! */
   BuiltinFormatStrings[11].formatStr = _("01000,01000 frames|29.97002997");
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at PAL TV frame rate (used for European TV) */
   BuiltinFormatStrings[12].name = _("hh:mm:ss + PAL frames (25 fps)");
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with PAL TV frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. Nice simple time code! */
   BuiltinFormatStrings[12].formatStr = _("0100 h 060 m 060 s+.25 frames");
   /* i18n-hint: Name of time display format that shows time in frames at PAL
    * TV frame rate (used for European TV) */
   BuiltinFormatStrings[13].name = _("PAL frames (25 fps)");
   /* i18n-hint: Format string for displaying time in frames with NTSC frames.
    * Translate 'frames' and leave the rest alone. */
   BuiltinFormatStrings[13].formatStr = _("01000,01000 frames|25");
   /* i18n-hint: Name of time display format that shows time in hours, minutes,
    * seconds and frames at CD Audio frame rate (75 frames per second) */
   BuiltinFormatStrings[14].name = _("hh:mm:ss + CDDA frames (75 fps)");
   /* i18n-hint: Format string for displaying time in hours, minutes, seconds
    * and frames with CD Audio frames. Change the 'h' to the abbreviation
    * for hours, 'm' to the abbreviation for minutes, 's' to the abbreviation
    * for seconds and translate 'frames'. */
   BuiltinFormatStrings[14].formatStr = _("0100 h 060 m 060 s+.75 frames");
   /* i18n-hint: Name of time display format that shows time in frames at CD
    * Audio frame rate (75 frames per second) */
   BuiltinFormatStrings[15].name = _("CDDA frames (75 fps)");
   /* i18n-hint: Format string for displaying time in frames with CD Audio
    * frames. Translate 'frames' and leave the rest alone */
   BuiltinFormatStrings[15].formatStr = _("01000,01000 frames|75");

   mPrefix = wxT("");
   mValueTemplate = wxT("");
   mValueMask = wxT("");
   mValueString = wxT("");

   mScalingFactor = 1.0f;
   mSampleRate = 1.0f;
   mNtscDrop = false;

   mFocusedDigit = 0;

   SetFormatName(formatName);
   SetTimeValue(timeValue);
   SetSampleRate(sampleRate);
}

void TimeConverter::ParseFormatString( const wxString & format)
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
            mFields.Add(TimeField(inFrac, base, range, zeropad));
            fracMult *= range;
            numFracFields++;
         }
         else {
            unsigned int j;
            for(j=0; j<mFields.GetCount(); j++)
               mFields[j].base *= range;
            mFields.Add(TimeField(inFrac, 1, range, zeropad));
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

      pos += mFields[i].digits;
      for(j=0; j<mFields[i].digits; j++) {
         mDigits.Add(DigitInfo(i, j, pos, wxRect()));
         mValueTemplate += wxT("0");
         mValueMask += wxT("0");
      }

      pos += mFields[i].label.Length();
      mValueTemplate += mFields[i].label;
      for(j=0; j<(int)mFields[i].label.Length(); j++)
         mValueMask += wxT(".");
   }
}

void TimeConverter::PrintDebugInfo()
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

void TimeConverter::ValueToControls()
{
   ValueToControls(mTimeValue);
}

void TimeConverter::ValueToControls(double RawTime, bool nearest /* = true */)
{
   //RawTime = 4.9995f; Only for testing!
   RawTime = (double)((sampleCount)floor(RawTime * mSampleRate + (nearest ? 0.5f : 0.0f))) / mSampleRate; // put on a sample
   double theValue = RawTime * mScalingFactor + .000001; // what's this .000001 for?
   sampleCount t_int;
   bool round = true;
   // We round on the last field.  If we have a fractional field we round using it.
   // Otherwise we round to nearest integer.
   for(unsigned int i=0; i<mFields.GetCount(); i++) {
      if (mFields[i].frac)
         round = false;
   }
   if(round)
      t_int = sampleCount(theValue + (nearest ? 0.5f : 0.0f));
   else
   {  
      wxASSERT( mFields[mFields.GetCount()-1].frac );
      theValue += (nearest ? 0.5f : 0.0f) / mFields[mFields.GetCount()-1].base;
      t_int = sampleCount(theValue);
   }
   double t_frac = (theValue - t_int);
   unsigned int i;
   int tenMins;
   int mins;
   int addMins;
   int secs;
   int frames;

   mValueString = mPrefix;

   if(mNtscDrop) {
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
      int value;

      if (mFields[i].frac) {
         // JKC: This old code looks bogus to me.
         // The rounding is not propogating to earlier fields in the frac case.
         //value = (int)(t_frac * mFields[i].base + 0.5);  // +0.5 as rounding required
         // I did the rounding earlier.
         value = (int)(t_frac * mFields[i].base); 
         // JKC: TODO: Find out what the range is supposed to do.
         // It looks bogus too.
         //if (mFields[i].range > 0)
         //   value = value % mFields[i].range;
      }
      else {
         value = (t_int / mFields[i].base);
         if (mFields[i].range > 0)
            value = value % mFields[i].range;
      }

      wxString field = wxString::Format(mFields[i].formatStr, value);
      mValueString += field;
      mValueString += mFields[i].label;
   }
}

void TimeConverter::ControlsToValue()
{
   unsigned int i;
   double t = 0.0;

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
      int t_int = int(t + .000000001);
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

   mTimeValue = t;
}

void TimeConverter::SetFormatName(const wxString & formatName)
{
   SetFormatString(GetBuiltinFormat(formatName));
}

void TimeConverter::SetFormatString(const wxString & formatString)
{
   mFormatString = formatString;
   ParseFormatString(mFormatString);
   ValueToControls();
   ControlsToValue();
}

void TimeConverter::SetSampleRate(double sampleRate)
{
   mSampleRate = sampleRate;
   ParseFormatString(mFormatString);
   ValueToControls();
   ControlsToValue();
}

void TimeConverter::SetTimeValue(double newTime)
{
   mTimeValue = newTime;
   ValueToControls();
   ControlsToValue();
}

double TimeConverter::GetTimeValue()
{
   ControlsToValue();
   return mTimeValue;
}

wxString TimeConverter::GetFormatString()
{
   return mFormatString;
}

int TimeConverter::GetFormatIndex()
{
   int ndx = 1;
   int i;

   for (i = 0; i < GetNumBuiltins(); i++) {
      if (mFormatString == GetBuiltinFormat(i)) {
         ndx = i;
         break;
      }
   }

   return ndx;
}
   
int TimeConverter::GetNumBuiltins()
{
   return (sizeof(BuiltinFormatStrings) / sizeof(BuiltinFormatStrings[0]));
}

wxString TimeConverter::GetBuiltinName(const int index)
{
   if (index >= 0 && index < GetNumBuiltins())
      return BuiltinFormatStrings[index].name;

   return wxEmptyString;
}

wxString TimeConverter::GetBuiltinFormat(const int index)
{
   if (index >= 0 && index < GetNumBuiltins())
      return BuiltinFormatStrings[index].formatStr;

   return wxEmptyString;
}

wxString TimeConverter::GetBuiltinFormat(const wxString &name)
{
   int ndx = 4; // Default to "hh:mm:ss + milliseconds".
   int i;

   for (i = 0; i < GetNumBuiltins(); i++) {
      if (name == GetBuiltinName(i)) {
         ndx = i;
         break;
      }
   }

   return GetBuiltinFormat(ndx);
}

wxString TimeConverter::GetTimeString()
{
   ValueToControls();

   return mValueString;
}

void TimeConverter::Increment()
{
   mFocusedDigit = mDigits.GetCount() - 1;
   Adjust(1, 1);
}

void TimeConverter::Decrement()
{
   mFocusedDigit = mDigits.GetCount() - 1;
   Adjust(1, -1);
}

void TimeConverter::Adjust(int steps, int dir)
{
   wxASSERT(dir == -1 || dir == 1);
   wxASSERT(steps > 0);
   if (steps < 0)
      steps = -steps;

   while (steps != 0)
   {
      for (size_t i = 0; i < mFields.GetCount(); i++)
      {
         if ((mDigits[mFocusedDigit].pos >= mFields[i].pos) && (mDigits[mFocusedDigit].pos < mFields[i].pos + mFields[i].digits))
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
            mTimeValue *= mScalingFactor;

            double mult = pow(10., mFields[i].digits - (mDigits[mFocusedDigit].pos - mFields[i].pos) - 1);
            if (mFields[i].frac)
            {
               mTimeValue += ((mult / (double)mFields[i].base) * dir);
            }
            else
            {
               mTimeValue += ((mult * (double)mFields[i].base) * dir);
            }

            if (mNtscDrop)
            {
               if ((mTimeValue - (int)mTimeValue) * 30 < 2)
               {
                  if ((((int)mTimeValue) % 60 == 0) && (((int)mTimeValue) % 600 != 0))
                  {
                     mTimeValue = (int)mTimeValue + (dir > 0 ? 2. : -1.) / 30.;
                  }
               }
            }

            if (mTimeValue < 0.)
            {
               mTimeValue = 0.;
            }

            mTimeValue /= mScalingFactor;

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

BEGIN_EVENT_TABLE(TimeTextCtrl, wxControl)
   EVT_ERASE_BACKGROUND(TimeTextCtrl::OnErase)
   EVT_PAINT(TimeTextCtrl::OnPaint)
   EVT_CONTEXT_MENU(TimeTextCtrl::OnContext)
   EVT_MENU_RANGE(ID_MENU, ID_MENU+100, TimeTextCtrl::OnMenu)
   EVT_MOUSE_EVENTS(TimeTextCtrl::OnMouse)
   EVT_KEY_DOWN(TimeTextCtrl::OnKeyDown)
   EVT_SET_FOCUS(TimeTextCtrl::OnFocus)
   EVT_KILL_FOCUS(TimeTextCtrl::OnFocus)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, TimeTextCtrl::OnCaptureKey)
END_EVENT_TABLE()

IMPLEMENT_CLASS(TimeTextCtrl, wxControl)

TimeTextCtrl::TimeTextCtrl(wxWindow *parent,
                           wxWindowID id,
                           wxString formatName,
                           double timeValue,
                           double sampleRate,
                           const wxPoint &pos,
                           const wxSize &size,
                           bool autoPos):
   wxControl(parent, id, pos, size, wxSUNKEN_BORDER | wxWANTS_CHARS),
   TimeConverter(formatName, timeValue, sampleRate),
   mBackgroundBitmap(NULL),
   mDigitFont(NULL),
   mLabelFont(NULL),
   mLastField(1),
   mAutoPos(autoPos)
{

   mDigitBoxW = 10;
   mDigitBoxH = 16;

   mMenuEnabled = true;
   mButtonWidth = 9;

   Layout();
   Fit();
   ValueToControls();
   //mchinen - aug 15 09 - this seems to put the mTimeValue back to zero, and do nothing else.
   //ControlsToValue(); 

   mScrollRemainder = 0.0;

#if wxUSE_ACCESSIBILITY
   SetLabel(wxT(""));
   SetName(wxT(""));
   SetAccessible(new TimeTextCtrlAx(this));
#endif
}

TimeTextCtrl::~TimeTextCtrl()
{
   wxCommandEvent e(EVT_RELEASE_KEYBOARD);
   e.SetEventObject(this);
   GetParent()->GetEventHandler()->ProcessEvent(e);

   if (mBackgroundBitmap)
      delete mBackgroundBitmap;
   if (mDigitFont)
      delete mDigitFont;
   if (mLabelFont)
      delete mLabelFont;
}

// Set the focus to the first (left-most) non-zero digit
// If all digits are zero, the right-most position is focused
void TimeTextCtrl::UpdateAutoFocus()
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

void TimeTextCtrl::SetFormatName(const wxString & formatName)
{
   SetFormatString(GetBuiltinFormat(formatName));
}

void TimeTextCtrl::SetFormatString(const wxString & formatString)
{
   TimeConverter::SetFormatString(formatString);
   Layout();
   Fit();
   ValueToControls();
   ControlsToValue();
   UpdateAutoFocus();
}

void TimeTextCtrl::SetSampleRate(double sampleRate)
{
   TimeConverter::SetSampleRate(sampleRate);
   Layout();
   Fit();
   ValueToControls();
   ControlsToValue();
}

void TimeTextCtrl::SetTimeValue(double newTime)
{
   TimeConverter::SetTimeValue(newTime);
   ValueToControls();
   ControlsToValue();
}

void TimeTextCtrl::EnableMenu(bool enable)
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

bool TimeTextCtrl::Layout()
{
   unsigned int i, j;
   int x, pos;

   wxMemoryDC memDC;
   if (mBackgroundBitmap) {
      delete mBackgroundBitmap;
      mBackgroundBitmap = NULL;
   }
   // Placeholder bitmap so the memDC has something to reference
   mBackgroundBitmap = new wxBitmap(1, 1);
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
   memDC.SetFont(wxFont(fontSize, wxFIXED, wxNORMAL, wxNORMAL));
   memDC.GetTextExtent(exampleText, &strW, &strH);
   while(strW <= mDigitBoxW && strH <= mDigitBoxH) {
      fontSize++;
      memDC.SetFont(wxFont(fontSize, wxFIXED, wxNORMAL, wxNORMAL));
      memDC.GetTextExtent(exampleText, &strW, &strH);
   }
   fontSize--;

   if (mDigitFont)
      delete mDigitFont;
   mDigitFont = new wxFont(fontSize, wxFIXED, wxNORMAL, wxNORMAL);
   memDC.SetFont(*mDigitFont);
   memDC.GetTextExtent(exampleText, &strW, &strH);
   mDigitW = strW;
   mDigitH = strH;

   // The label font should be a little smaller
   fontSize--;
   if (mLabelFont)
      delete mLabelFont;
   mLabelFont = new wxFont(fontSize, wxFIXED, wxNORMAL, wxNORMAL);

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

   delete mBackgroundBitmap; // Delete placeholder
   mBackgroundBitmap = new wxBitmap(mWidth + mButtonWidth, mHeight);
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

void TimeTextCtrl::Fit()
{
   wxSize sz = GetSize();
   wxSize csz = GetClientSize();

   sz.x = mButtonWidth + mWidth + (sz.x - csz.x);
   sz.y = mHeight + (sz.y - csz.y);

   SetInitialSize(sz);
}

void TimeTextCtrl::OnErase(wxEraseEvent & WXUNUSED(event))
{
   // Ignore it to prevent flashing
}

void TimeTextCtrl::OnPaint(wxPaintEvent & WXUNUSED(event))
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

void TimeTextCtrl::OnMenu(wxCommandEvent &event)
{
   int id = event.GetId() - ID_MENU;

   if (!mMenuEnabled || id < 0 || id > GetNumBuiltins()) {
      event.Skip();
      return;
   }

   SetFormatString(GetBuiltinFormat(id));

   wxCommandEvent e(EVT_TIMETEXTCTRL_UPDATED, GetId());
   e.SetInt(id);
   e.SetString(GetBuiltinName(id));
   GetParent()->GetEventHandler()->AddPendingEvent(e);
}

void TimeTextCtrl::OnContext(wxContextMenuEvent &event)
{
   wxMenu menu;
   int i;

   if (!mMenuEnabled) {
      event.Skip();
      return;
   }

   SetFocus();

   for(i=0; i<GetNumBuiltins(); i++) {
      menu.AppendCheckItem(ID_MENU+i, GetBuiltinName(i));
      if (mFormatString == GetBuiltinFormat(i))
         menu.Check(ID_MENU+i, true);
   }

   PopupMenu(&menu, wxPoint(0, 0));
}

void TimeTextCtrl::OnMouse(wxMouseEvent &event)
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
   else if( event.m_wheelRotation != 0 ) {
      double steps = event.m_wheelRotation /
         (event.m_wheelDelta > 0 ? (double)event.m_wheelDelta : 120.0) + 
         mScrollRemainder;
      mScrollRemainder = steps - floor(steps);
      steps = floor(steps);

      Adjust((int)fabs(steps), steps < 0.0 ? -1 : 1);
      Updated();
   }
}

void TimeTextCtrl::OnFocus(wxFocusEvent &event)
{
   wxCommandEvent e(EVT_CAPTURE_KEYBOARD);

   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      e.SetEventType(EVT_RELEASE_KEYBOARD);
   }
   e.SetEventObject(this);
   GetParent()->GetEventHandler()->ProcessEvent(e);

   Refresh(false);
}

void TimeTextCtrl::OnCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   int keyCode = kevent->GetKeyCode();

   // Convert numeric keypad entries.
   if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9)) keyCode -= WXK_NUMPAD0 - '0';

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
         return;

      default:
         if (keyCode >= '0' && keyCode <= '9')
            return;
   }

   event.Skip();

   return;
}

void TimeTextCtrl::OnKeyUp(wxKeyEvent &event)
{
   int keyCode = event.GetKeyCode();

   event.Skip(true);
   
   if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9))
      keyCode -= WXK_NUMPAD0 - '0';

   if ((keyCode >= '0' && keyCode <= '9') ||
       (keyCode == WXK_BACK) ||
       (keyCode == WXK_UP) ||
       (keyCode == WXK_DOWN)) {
      Updated(true);
   }
}

void TimeTextCtrl::OnKeyDown(wxKeyEvent &event)
{
   if (mDigits.GetCount() == 0)
   {
      mFocusedDigit = 0;
      return;
   }

   event.Skip();

   int keyCode = event.GetKeyCode();
   int digit = mFocusedDigit;

   if (mFocusedDigit < 0)
      mFocusedDigit = 0;
   if (mFocusedDigit >= (int)mDigits.GetCount())
      mFocusedDigit = mDigits.GetCount()-1;

   // Convert numeric keypad entries.
   if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9)) keyCode -= WXK_NUMPAD0 - '0';

   if (keyCode >= '0' && keyCode <= '9') {
      mValueString[mDigits[mFocusedDigit].pos] = wxChar(keyCode);
      ControlsToValue();
      ValueToControls();
      mFocusedDigit = (mFocusedDigit+1)%(mDigits.GetCount());
      Updated();
   }

   else if (keyCode == WXK_BACK) {
      // Moves left, replaces that char with '0', stays there...
      mFocusedDigit--;
      mFocusedDigit += mDigits.GetCount();
      mFocusedDigit %= mDigits.GetCount();
      mValueString[mDigits[mFocusedDigit].pos] = '0';
      ControlsToValue();
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

   else if (keyCode == WXK_UP) {
      Adjust(1, 1);
      Updated();
   }

   else if (keyCode == WXK_DOWN) {
      Adjust(1, -1);
      Updated();
   }

   else if (keyCode == WXK_TAB) {   
      wxWindow *parent = GetParent();
      wxNavigationKeyEvent nevent;
      nevent.SetWindowChange(event.ControlDown());
      nevent.SetDirection(!event.ShiftDown());
      nevent.SetEventObject(parent);
      nevent.SetCurrentFocus(parent);
      GetParent()->ProcessEvent(nevent);
      event.Skip(false);
   } 

   else if (keyCode == WXK_RETURN || keyCode == WXK_NUMPAD_ENTER) {
      wxTopLevelWindow *tlw = wxDynamicCast(wxGetTopLevelParent(this), wxTopLevelWindow);
      wxWindow *def = tlw->GetDefaultItem();
      if (def && def->IsEnabled()) {
         wxCommandEvent cevent(wxEVT_COMMAND_BUTTON_CLICKED,
                               def->GetId());
         GetParent()->ProcessEvent(cevent);
         event.Skip(false);
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

void TimeTextCtrl::SetFieldFocus(int digit)
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
   // somewhat the same.  See TimeTextCtrlAx below for even more odd looking
   // hackery.
   //
   // If you change SetFieldFocus(), Updated(), or TimeTextCtrlAx, make sure
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

void TimeTextCtrl::Updated(bool keyup /* = false */)
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

void TimeTextCtrl::ValueToControls()
{
   TimeConverter::ValueToControls(mTimeValue);
   Refresh(false);
}


void TimeTextCtrl::ControlsToValue()
{
   TimeConverter::ControlsToValue();
}

#if wxUSE_ACCESSIBILITY

TimeTextCtrlAx::TimeTextCtrlAx(TimeTextCtrl *ctrl)
:  wxWindowAccessible(ctrl)
{
   mCtrl = ctrl;
   mLastField = -1;
   mLastDigit = -1;
}

TimeTextCtrlAx::~TimeTextCtrlAx()
{
}

// Performs the default action. childId is 0 (the action for this object)
// or > 0 (the action for a child).
// Return wxACC_NOT_SUPPORTED if there is no default action for this
// window (e.g. an edit control).
wxAccStatus TimeTextCtrlAx::DoDefaultAction(int WXUNUSED(childId))
{
   return wxACC_NOT_SUPPORTED;
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus TimeTextCtrlAx::GetChild(int childId, wxAccessible **child)
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
wxAccStatus TimeTextCtrlAx::GetChildCount(int *childCount)
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
wxAccStatus TimeTextCtrlAx::GetDefaultAction(int WXUNUSED(childId), wxString *actionName)
{
   actionName->Clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus TimeTextCtrlAx::GetDescription(int WXUNUSED(childId), wxString *description)
{
   description->Clear();

   return wxACC_OK;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus TimeTextCtrlAx::GetFocus(int *childId, wxAccessible **child)
{
   *childId = mCtrl->GetFocusedDigit();
   *child = this;

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus TimeTextCtrlAx::GetHelpText(int WXUNUSED(childId), wxString *helpText)
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
wxAccStatus TimeTextCtrlAx::GetKeyboardShortcut(int WXUNUSED(childId), wxString *shortcut)
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus TimeTextCtrlAx::GetLocation(wxRect & rect, int elementId)
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
wxAccStatus TimeTextCtrlAx::GetName(int childId, wxString *name)
{
   // Slightly messy trick to save us some prefixing.
   TimeFieldArray & mFields = mCtrl->mFields;
      
   wxString value = mCtrl->GetTimeString();
   int field = mCtrl->GetFocusedField();

   // Return the entire time string including the control label
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
               mCtrl->GetTimeString();
   }
   // The user has moved from one field of the time to another so
   // report the value of the field and the field's label.
   else if (mLastField != field) {
      wxString label = mFields[field - 1].label;
      int cnt = mFields.GetCount();
      wxString decimal = wxLocale::GetInfo(wxLOCALE_DECIMAL_POINT, wxLOCALE_CAT_NUMBER);

      // If the new field is the last field, then check it to see if
      // it represents fractions of a second.
      if (field > 1 && field == cnt) {
         if (mFields[field - 2].label == decimal) {
            int digits = mFields[field - 1].digits;
            if (digits == 2) {
               label = _("centiseconds");
            }
            else if (digits == 3) {
               label = _("milliseconds");
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
              mCtrl->GetTimeString().at(mCtrl->mDigits[childId - 1].pos);
      mLastField = field;
      mLastDigit = childId;
   }
   // The user has moved from one digit to another within a field so
   // just report the digit under the cursor.
   else if (mLastDigit != childId) {
      *name = mCtrl->GetTimeString().at(mCtrl->mDigits[childId - 1].pos);
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
wxAccStatus TimeTextCtrlAx::GetRole(int WXUNUSED(childId), wxAccRole *role)
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
wxAccStatus TimeTextCtrlAx::GetSelections(wxVariant * WXUNUSED(selections))
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus TimeTextCtrlAx::GetState(int WXUNUSED(childId), long *state)
{
   *state = wxACC_STATE_SYSTEM_FOCUSABLE;
   *state |= (mCtrl == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0);

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus TimeTextCtrlAx::GetValue(int WXUNUSED(childId), wxString * WXUNUSED(strValue))
{
   return wxACC_NOT_IMPLEMENTED;
}

#endif

