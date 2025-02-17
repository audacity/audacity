/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file ParsedNumericConverterFormatter.cpp

 Dmitry Vedenko split from NumericConverter.cpp

 **********************************************************************/
#include "ParsedNumericConverterFormatter.h"
#include "NumericConverterRegistry.h"

#include "SampleCount.h"
#include "NumericConverterFormats.h"
#include "NumericConverterFormatterContext.h"

#include "Project.h"
#include "ProjectRate.h"

#include <cmath>

namespace {
/**
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

*******************************************************************/

struct FieldConfig final
{
    bool frac; // is it a fractional field
    int base; // divide by this (multiply, after decimal point)
    // Code in the parser converts range to `long`
    long range; // then take modulo this
};

class ParsedNumericConverterFormatter final : public NumericConverterFormatter, public PrefsListener
{
public:
    ParsedNumericConverterFormatter(
        NumericConverterType type, const TranslatableString& untranslatedFormat, const FormatterContext& context)
        : mContext{context}
        , mType{type}
        , mFormat{untranslatedFormat.Translation()}
        , mUntranslatedFormat{untranslatedFormat}
    {
        UpdateFormat();

        if (IsTimeRelatedFormat()) {
            auto project = mContext.GetProject();

            if (project != nullptr) {
                // We need a non const object to subscribe...
                mProjectRateChangedSubscription
                    =const_cast<ProjectRate&>(ProjectRate::Get(*project))
                      .Subscribe([this](const auto&) { UpdateFormat(); });
            }
        }
    }

    bool IsTimeRelatedFormat() const
    {
        return mType == NumericConverterType_TIME()
               || mType == NumericConverterType_DURATION();
    }

    void
    ParseFormatString()
    {
        mPrefix.clear();
        mFields.clear();
        mDigits.clear();
        mFieldConfigs.clear();

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
        for (i = 0; i < mFormat.length(); i++) {
            bool handleDelim = false;
            bool handleNum = false;

            if (mFormat[i] == '|') {
                wxString remainder = mFormat.Right(mFormat.length() - i - 1);
                // For languages which use , as a separator.
                remainder.Replace(wxT(","), wxT("."));

                mScalingFactorIsSamples = remainder == wxT("#");

                if (mScalingFactorIsSamples) {
                    mScalingFactor = mSampleRate;
                } else if (remainder == wxT("N")) {
                    mNtscDrop = true;
                } else {
                    // Use the C locale here for string to number.
                    // Translations are often incomplete.
                    // We can't rely on the correct ',' or '.' in the
                    // translation, so we work based on '.' for decimal point.
                    remainder.ToCDouble(&mScalingFactor);
                }
                i = mFormat.length() - 1; // force break out of loop
                if (!delimStr.empty()) {
                    handleDelim = true;
                }
                if (!numStr.empty()) {
                    handleNum = true;
                }
            } else if (
                (mFormat[i] >= '0' && mFormat[i] <= '9')
                || mFormat[i] == wxT('*') || mFormat[i] == wxT('#')) {
                numStr += mFormat[i];
                if (!delimStr.empty()) {
                    handleDelim = true;
                }
            } else {
                delimStr += mFormat[i];
                if (!numStr.empty()) {
                    handleNum = true;
                }
            }

            if (i == mFormat.length() - 1) {
                if (!numStr.empty()) {
                    handleNum = true;
                }
                if (!delimStr.empty()) {
                    handleDelim = true;
                }
            }

            if (handleNum) {
                bool zeropad = false;
                long range = 0;

                if (numStr.Right(1) == wxT("#")) {
                    range = static_cast<long int>(mSampleRate);
                } else if (numStr.Right(1) != wxT("*")) {
                    numStr.ToLong(&range);
                }
                if (numStr.GetChar(0) == '0' && numStr.length() > 1) {
                    zeropad = true;
                }

                // Hack: always zeropad
                zeropad = true;

                if (inFrac) {
                    int base = fracMult * range;
                    mFieldConfigs.push_back({ inFrac, base, range });
                    mFields.push_back(NumericField::ForRange(range, zeropad));
                    fracMult *= range;
                    numFracFields++;
                } else {
                    unsigned int j;
                    for (j = 0; j < mFields.size(); j++) {
                        mFieldConfigs[j].base *= range;
                    }
                    mFieldConfigs.push_back({ inFrac, 1, range });
                    mFields.push_back(NumericField::ForRange(range, zeropad));
                    numWholeFields++;
                }
                numStr = wxT("");
            }

            if (handleDelim) {
                bool goToFrac = false;

                if (!inFrac) {
                    wxChar delim = delimStr[delimStr.length() - 1];
                    if (delim == '<' || delim == '>') {
                        goToFrac = true;
                        if (delimStr.length() > 1) {
                            delimStr = delimStr.BeforeLast(delim);
                        }
                    }
                }

                if (inFrac) {
                    if (numFracFields == 0) {
                        // Should never happen
                        return;
                    }
                    if (handleNum && numFracFields > 1) {
                        mFields[mFields.size() - 2].label = delimStr;
                    } else {
                        mFields[mFields.size() - 1].label = delimStr;
                    }
                } else {
                    if (numWholeFields == 0) {
                        mPrefix = delimStr;
                    } else {
                        delimStr.Replace(wxT("<"), wxT(","));
                        delimStr.Replace(wxT(">"), wxT("."));
                        mFields[numWholeFields - 1].label = delimStr;
                    }
                }

                if (goToFrac) {
                    inFrac = true;
                }
                delimStr = wxT("");
            }
        }

        size_t pos = 0;

        pos += mPrefix.length();

        for (i = 0; i < mFields.size(); i++) {
            mFields[i].pos = pos;

            for (size_t j = 0; j < mFields[i].digits; j++) {
                mDigits.push_back(DigitInfo { i, j, pos });
                pos++;
            }

            pos += mFields[i].label.length();
        }

        // This Publish will happen from the
        // constructor as well, despite it is not
        // possible to catch it there
        Publish({});
    }

    void UpdateFormat()
    {
        const auto newSampleRate = mContext.GetSampleRate();

        const bool sampleRateChanged = newSampleRate != mSampleRate;

        mSampleRate = newSampleRate;

        if (mFields.empty() || (sampleRateChanged && mScalingFactorIsSamples)) {
            ParseFormatString();
        }
    }

    ConversionResult ValueToString(
        double rawValue, bool nearest) const override
    {
        ConversionResult result;

        if (IsTimeRelatedFormat() && mContext.HasSampleRate()) {
            rawValue = floor(rawValue * mSampleRate + (nearest ? 0.5f : 0.0f))
                       / mSampleRate; // put on a sample
        }
        double theValue = rawValue * mScalingFactor
                          // PRL:  what WAS this .000001 for?  Nobody could explain.
                          // + .000001
        ;

        sampleCount t_int;
        bool round = true;
        // We round on the last field.  If we have a fractional field we round
        // using it. Otherwise we round to nearest integer.
        for (size_t i = 0; i < mFields.size(); i++) {
            if (mFieldConfigs[i].frac) {
                round = false;
            }
        }
        if (theValue < 0) {
            t_int = -1;
        } else if (round) {
            t_int = sampleCount(theValue + (nearest ? 0.5f : 0.0f));
        } else {
            wxASSERT(mFieldConfigs.back().frac);
            theValue += (nearest ? 0.5f : 0.0f) / mFieldConfigs.back().base;
            t_int = sampleCount(theValue);
        }
        double t_frac;
        if (theValue < 0) {
            t_frac = -1;
        } else {
            t_frac = (theValue - t_int.as_double());
        }

        int tenMins;
        int mins;
        int addMins;
        int secs;
        int frames;

        result.valueString = mPrefix;

        if (mNtscDrop && theValue >= 0) {
            frames = (int)(theValue * 30. / 1.001 + (nearest ? 0.5f : 0.0f));
            tenMins = frames / 17982;
            frames -= tenMins * 17982;
            mins = tenMins * 10;
            if (frames >= 1800) {
                frames -= 1800;
                mins++;
                addMins = frames / 1798;
                frames -= addMins * 1798;
                mins += addMins;
                secs = frames / 30;
                frames -= secs * 30;
                frames += 2;
                if (frames >= 30) {
                    secs++;
                    frames -= 30;
                }
            } else {
                secs = frames / 30;
                frames -= secs * 30;
            }
            t_int = mins * 60 + secs;
            t_frac = frames / 30.;
        }

        for (size_t i = 0; i < mFields.size(); i++) {
            long long value = -1;

            if (mFieldConfigs[i].frac) {
                // JKC: This old code looks bogus to me.
                // The rounding is not propagating to earlier fields in the frac
                // case.
                // value = (int)(t_frac * mFields[i].base + 0.5);  // +0.5 as
                // rounding required
                // I did the rounding earlier.
                if (t_frac >= 0) {
                    value = t_frac * mFieldConfigs[i].base;
                }
                // JKC: TODO: Find out what the range is supposed to do.
                // It looks bogus too.
                // if (mFields[i].range > 0)
                //   value = value % mFields[i].range;
            } else {
                if (t_int >= 0) {
                    value = t_int.as_long_long() / mFieldConfigs[i].base;
                    if (mFieldConfigs[i].range > 0) {
                        value = value % mFieldConfigs[i].range;
                    }
                }
            }

            wxString field;

            if (value < 0) {
                for (int ii = 0; ii < mFields[i].digits; ++ii) {
                    field += wxT("-");
                }
            } else {
                field = wxString::Format(mFields[i].formatStr, (int)value);
            }

            result.fieldValueStrings.push_back(field);

            result.valueString += field;
            result.valueString += mFields[i].label;
        }

        return result;
    }

    std::optional<double> StringToValue(
        const wxString& valueString) const override
    {
        unsigned int i;
        double t = 0.0;

        if (
            mFields.size() > 0
            && valueString.Mid(mFields[0].pos, 1) == wxChar('-')) {
            return std::nullopt;
        }

        for (i = 0; i < mFields.size(); i++) {
            const auto pos = mFields[i].pos;
            const auto digits = mFields[i].digits;

            if (pos >= valueString.size() || pos + digits > valueString.size()) {
                return std::nullopt;
            }

            long val;

            const auto fieldStringValue
                =valueString.Mid(mFields[i].pos, mFields[i].digits);

            if (!fieldStringValue.ToLong(&val)) {
                return std::nullopt;
            }

            if (mFieldConfigs[i].frac) {
                t += (val / (double)mFieldConfigs[i].base);
            } else {
                t += (val * (double)mFieldConfigs[i].base);
            }
        }

        t /= mScalingFactor;

        if (mNtscDrop) {
            int t_int = (int)(t + .000000001);
            double t_frac = (t - t_int);
            int tenMins = t_int / 600;
            double frames = tenMins * 17982;
            t_int -= tenMins * 600;
            int mins = t_int / 60;
            int addMins = 0;
            if (mins > 0) {
                frames += 1800;
                addMins = mins - 1;
            }
            frames += addMins * 1798;
            t_int -= mins * 60;
            if (mins == 0) { // first min of a block of 10, don't drop frames 0 and 1
                frames += t_int * 30 + t_frac * 30.;
            } else { // drop frames 0 and 1 of first seconds of these minutes
                if (t_int > 0) {
                    frames += 28 + (t_int - 1) * 30 + t_frac * 30.;
                } else {
                    frames += t_frac * 30. - 2.;
                }
            }
            t = frames * 1.001 / 30.;
        }

        return t;
    }

    double SingleStep(double value, int digitIndex, bool upwards) const override
    {
        const auto dir = upwards ? 1 : -1;
        for (size_t i = 0; i < mFields.size(); i++) {
            if (
                (mDigits[digitIndex].pos >= mFields[i].pos)
                && (mDigits[digitIndex].pos < mFields[i].pos + mFields[i].digits)) { // it's this field
                if (value < 0) {
                    value = 0;
                }

                value *= mScalingFactor;

                const double mult = pow(
                    10., mFields[i].digits
                    - (mDigits[digitIndex].pos - mFields[i].pos) - 1);

                if (mFieldConfigs[i].frac) {
                    value += ((mult / (double)mFieldConfigs[i].base) * dir);
                } else {
                    value += ((mult * (double)mFieldConfigs[i].base) * dir);
                }

                if (mNtscDrop) {
                    if ((value - (int)value) * 30 < 2) {
                        if ((((int)value) % 60 == 0) && (((int)value) % 600 != 0)) {
                            value = (int)value + (dir > 0 ? 2. : -1.) / 30.;
                        }
                    }
                }

                if (value < 0.) {
                    value = 0.;
                }

                value /= mScalingFactor;

                if (mNtscDrop) {
                    mNtscDrop = false;
                    auto result = ValueToString(value, false);
                    mNtscDrop = true;
                    return *StringToValue(result.valueString);
                }

                return value;
            }
        }

        return value;
    }

    void UpdatePrefs() override
    {
        auto newFormat = mUntranslatedFormat.Translation();

        if (mFormat == newFormat) {
            return;
        }

        mFormat = newFormat;
        ParseFormatString();
    }

private:
    const FormatterContext mContext;
    const NumericConverterType mType;
    wxString mFormat;
    const TranslatableString mUntranslatedFormat;

    std::vector<FieldConfig> mFieldConfigs;

    double mScalingFactor;
    double mSampleRate { 1.0 };

    Observer::Subscription mProjectRateChangedSubscription;

    bool mScalingFactorIsSamples { false };

    mutable bool mNtscDrop;
};

//
// ----------------------------------------------------------------------------
// BuiltinFormatString Struct
// ----------------------------------------------------------------------------
//
struct FormatStrings final
{
    TranslatableString formatStr;
    // How to name the fraction of the unit; not necessary for time formats
    // or when the format string has no decimal point
    TranslatableString fraction;

    FormatStrings(
        const TranslatableString& format = {},
        const TranslatableString& fraction = {})
        : formatStr{format}
        , fraction{fraction}
    {
    }

    friend bool operator==(const FormatStrings& x, const FormatStrings& y)
    {
        return x.formatStr == y.formatStr && x.fraction == y.fraction;
    }

    friend bool operator!=(const FormatStrings& x, const FormatStrings& y)
    {
        return !(x == y);
    }
};
/** \brief struct to hold a formatting control string and its user facing name
 * Used in an array to hold the built-in time formats that are always available
 * to the user */
struct BuiltinFormatString
{
    NumericFormatSymbol name;
    FormatStrings formatStrings;

    friend inline bool
    operator==(const BuiltinFormatString& a, const BuiltinFormatString& b)
    {
        return a.name == b.name;
    }
};
/** \brief array of formats the control knows about internally
 *  array of string pairs for name of the format and the format string
 *  needed to create that format output. This is used for the pop-up
 *  list of formats to choose from in the control.          */
static BuiltinFormatString TimeConverterFormats_[] =  {
    {
        NumericConverterFormats::SecondsFormat(),
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
        { XO("01000,01000>01000 seconds"),
          XO("milliseconds") }
    },

    {
        NumericConverterFormats::HoursMinsSecondsFormat(),
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
        NumericConverterFormats::HundredthsFormat(),
        /* i18n-hint: Format string for displaying time in hours, minutes, seconds
         * and hundredths of a second. Change the 'h' to the abbreviation for hours,
         * 'm' to the abbreviation for minutes and 's' to the abbreviation for seconds
         * (the hundredths are shown as decimal seconds). Don't change the numbers
         * unless there aren't 60 minutes in an hour in your locale.
         * The decimal separator is specified using '<' if your language uses a ',' or
         * to '>' if your language uses a '.'. */
        { XO("0100 h 060 m 060>0100 s"),
          XO("centiseconds") }
    },

    {
        NumericConverterFormats::MillisecondsFormat(),
        /* i18n-hint: Format string for displaying time in hours, minutes, seconds
         * and milliseconds. Change the 'h' to the abbreviation for hours, 'm' to the
         * abbreviation for minutes and 's' to the abbreviation for seconds (the
         * milliseconds are shown as decimal seconds) . Don't change the numbers
         * unless there aren't 60 minutes in an hour in your locale.
         * The decimal separator is specified using '<' if your language uses a ',' or
         * to '>' if your language uses a '.'. */
        { XO("0100 h 060 m 060>01000 s"),
          XO("milliseconds") }
    },

    {
        NumericConverterFormats::TimeAndSampleFormat(),
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

NumericConverterFormats::DefaultFormatRegistrator timeDefault {
    NumericConverterType_TIME(), NumericConverterFormats::MillisecondsFormat()
};

/** \brief array of formats the control knows about internally
 *  array of string pairs for name of the format and the format string
 *  needed to create that format output. This is used for the pop-up
 *  list of formats to choose from in the control. */
static const BuiltinFormatString FrequencyConverterFormats_[] = {
    {
        NumericConverterFormats::HertzFormat(),
        {
            /* i18n-hint: Format string for displaying frequency in hertz. Change
            * the decimal point for your locale. Don't change the numbers.
            * The decimal separator is specified using '<' if your language uses a ',' or
            * to '>' if your language uses a '.'. */
            XO("010,01000>0100 Hz"),
            XO("centihertz")
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
            XO("01000>01000 kHz|0.001"),
            XO("hertz")
        }
    },
};

NumericConverterFormats::DefaultFormatRegistrator frequencyDefault {
    NumericConverterType_FREQUENCY(), NumericConverterFormats::HertzFormat()
};

/** \brief array of formats the control knows about internally
 *  array of string pairs for name of the format and the format string
 *  needed to create that format output. This is used for the pop-up
 *  list of formats to choose from in the control. */
static const BuiltinFormatString BandwidthConverterFormats_[] = {
    {
        NumericConverterFormats::OctavesFormat(),
        {
            /* i18n-hint: Format string for displaying log of frequency in octaves.
             * Change the decimal points for your locale. Don't change the numbers.
             * The decimal separator is specified using '<' if your language uses a ',' or
             * to '>' if your language uses a '.'. */
            XO("100>01000 octaves|1.442695041"), // Scale factor is 1 / ln (2)
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
            XO("1000 semitones >0100 cents|17.312340491"), // Scale factor is 12 / ln (2)
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
            XO("10>01000 decades|0.434294482"), // Scale factor is 1 / ln (10)
            /* i18n-hint: a decade is a tenfold increase of frequency */
            XO("thousandths of decades")
        }
    },
};

NumericConverterFormats::DefaultFormatRegistrator bandwidthDefault {
    NumericConverterType_BANDWIDTH(), NumericConverterFormats::OctavesFormat()
};

class ParsedNumericConverterFormatterFactory final : public NumericConverterFormatterFactory
{
public:
    ParsedNumericConverterFormatterFactory(
        NumericConverterType type, TranslatableString format)
        : mType{std::move(type)}
        , mFormat{std::move(format)}
    {
        // # in the format means that Sample Rate is used
        // to calculate the values. Otherwise, Sample Rate is optional for
        // the TIME and DURATION formats and ignored by the others.
        mDependsOnSampleRate = mFormat.Debug().find(L'#') != wxString::npos;
    }

    std::unique_ptr<NumericConverterFormatter>
    Create(const FormatterContext& context) const override
    {
        if (!IsAcceptableInContext(context)) {
            return {}
        }

        return std::make_unique<ParsedNumericConverterFormatter>(
            mType, mFormat, context);
    }

    bool IsAcceptableInContext(const FormatterContext& context) const override
    {
        return !mDependsOnSampleRate || context.HasSampleRate();
    }

private:
    NumericConverterType mType;
    TranslatableString mFormat;

    bool mDependsOnSampleRate;
};

static auto MakeItem(const NumericConverterType& type)
{
    return [&type](const BuiltinFormatString& formatString) {
        const auto functionIdentifier = formatString.name.Internal();
        return NumericConverterFormatterItem(
            functionIdentifier, formatString.name,
            formatString.formatStrings.fraction,
            std::make_unique<ParsedNumericConverterFormatterFactory>(
                type, formatString.formatStrings.formatStr));
    };
}

template<size_t N>
static auto MakeGroup(const Identifier& identifier, NumericConverterType type,
                      const BuiltinFormatString (& formatStrings)[N])
{
    return NumericConverterFormatterGroup(identifier, { type },
                                          std::begin(formatStrings), std::end(formatStrings),
                                          MakeItem(type));
}

static NumericConverterItemRegistrator sRegistration {
    NumericConverterItems("parsed",
                          // The sequence of these groups is unimportant because their numeric
                          // converter types are all different
                          MakeGroup("parsedTime",
                                    NumericConverterType_TIME(), TimeConverterFormats_),
                          MakeGroup("parsedDuration", NumericConverterType_DURATION(),
                                    TimeConverterFormats_),
                          MakeGroup("parsedFrequency", NumericConverterType_FREQUENCY(),
                                    FrequencyConverterFormats_),
                          MakeGroup("parsedBandwith", NumericConverterType_BANDWIDTH(),
                                    BandwidthConverterFormats_)
                          )
};
} // namespace

std::unique_ptr<NumericConverterFormatter>
CreateParsedNumericConverterFormatter(
    const FormatterContext& context, NumericConverterType type,
    const TranslatableString& format)
{
    return std::make_unique<ParsedNumericConverterFormatter>(type, format, context);
}
