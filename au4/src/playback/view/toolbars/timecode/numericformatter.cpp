/*
* Audacity: A Digital Audio Editor
*/
#include "numericformatter.h"

#include <cstdio>

using namespace au::playback;

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
NumericFormatter::NumericFormatter(const QString& formatStr)
    : TimecodeFormatter(formatStr)
{
}

void NumericFormatter::init()
{
    m_prefix.clear();
    m_fields.clear();
    m_digits.clear();
    m_fieldConfigs.clear();

    m_scalingFactor = 1.0;

    // We will change inFrac to true when we hit our first decimal point.
    bool inFrac = false;
    int fracMult = 1;
    int numWholeFields = 0;
    int numFracFields = 0;
    QString numStr;
    QString delimStr;
    unsigned int i;

    m_ntscDrop = false;
    for (i = 0; i < m_format.length(); i++) {
        bool handleDelim = false;
        bool handleNum = false;

        if (m_format[i] == '|') {
            QString remainder = m_format.right(m_format.length() - i - 1);
            // For languages which use , as a separator.
            remainder.replace(',', '.');

            m_scalingFactorIsSamples = remainder == "#";

            if (m_scalingFactorIsSamples) {
                m_scalingFactor = m_sampleRate;
            } else if (remainder == "N") {
                m_ntscDrop = true;
            } else {
                // Use the C locale here for string to number.
                // Translations are often incomplete.
                // We can't rely on the correct ',' or '.' in the
                // translation, so we work based on '.' for decimal point.
                m_scalingFactor = remainder.toDouble();
            }
            i = m_format.length() - 1; // force break out of loop
            if (!delimStr.isEmpty()) {
                handleDelim = true;
            }
            if (!numStr.isEmpty()) {
                handleNum = true;
            }
        } else if (
            (m_format[i] >= '0' && m_format[i] <= '9')
            || m_format[i] == '*' || m_format[i] == '#') {
            numStr += m_format[i];
            if (!delimStr.isEmpty()) {
                handleDelim = true;
            }
        } else {
            delimStr += m_format[i];
            if (!numStr.isEmpty()) {
                handleNum = true;
            }
        }

        if (i == m_format.length() - 1) {
            if (!numStr.isEmpty()) {
                handleNum = true;
            }
            if (!delimStr.isEmpty()) {
                handleDelim = true;
            }
        }

        if (handleNum) {
            bool zeropad = false;
            long range = 0;

            if (numStr.right(1) == "#") {
                range = static_cast<long int>(m_sampleRate);
            } else if (numStr.right(1) != "*") {
                range = numStr.toLong();
            }
            if (numStr[0] == '0' && numStr.length() > 1) {
                zeropad = true;
            }

            // Hack: always zeropad
            zeropad = true;

            if (inFrac) {
                int base = fracMult * range;
                m_fieldConfigs.push_back({ inFrac, base, range });
                m_fields.push_back(NumericField::range(range, zeropad));
                fracMult *= range;
                numFracFields++;
            } else {
                unsigned int j;
                for (j = 0; j < m_fields.size(); j++) {
                    m_fieldConfigs[j].base *= range;
                }
                m_fieldConfigs.push_back({ inFrac, 1, range });
                m_fields.push_back(NumericField::range(range, zeropad));
                numWholeFields++;
            }
            numStr = "";
        }

        if (handleDelim) {
            bool goToFrac = false;

            if (!inFrac) {
                QChar delim = delimStr[delimStr.length() - 1];
                if (delim == '<' || delim == '>') {
                    goToFrac = true;
                    if (delimStr.length() > 1) {
                        delimStr = delimStr.left(delimStr.lastIndexOf(delim));
                    }
                }
            }

            if (inFrac) {
                if (numFracFields == 0) {
                    // Should never happen
                    return;
                }
                if (handleNum && numFracFields > 1) {
                    m_fields[m_fields.size() - 2].label = delimStr;
                } else {
                    m_fields[m_fields.size() - 1].label = delimStr;
                }
            } else {
                if (numWholeFields == 0) {
                    m_prefix = delimStr;
                } else {
                    delimStr.replace('<', ',');
                    delimStr.replace('>', '.');
                    m_fields[numWholeFields - 1].label = delimStr;
                }
            }

            if (goToFrac) {
                inFrac = true;
            }
            delimStr = "";
        }
    }

    size_t pos = 0;

    pos += m_prefix.length();

    for (i = 0; i < m_fields.size(); i++) {
        m_fields[i].pos = pos;

        for (size_t j = 0; j < m_fields[i].digits; j++) {
            m_digits.push_back(DigitInfo { i, j, pos });
            pos++;
        }

        pos += m_fields[i].label.length();
    }
}

NumericFormatter::ConversionResult NumericFormatter::valueToString(double value, bool nearest) const
{
    ConversionResult result;
    double rawValue = floor(value * m_sampleRate + (nearest ? 0.5f : 0.0f))
                      / m_sampleRate; // put on a sample
    double theValue = rawValue * m_scalingFactor
                      // PRL:  what WAS this .000001 for?  Nobody could explain.
                      // + .000001
    ;

    int t_int; // todo
    bool round = true;
    // We round on the last field.  If we have a fractional field we round
    // using it. Otherwise we round to nearest integer.
    for (size_t i = 0; i < m_fields.size(); i++) {
        if (m_fieldConfigs[i].frac) {
            round = false;
        }
    }
    if (theValue < 0) {
        t_int = -1;
    } else if (round) {
        t_int = int(theValue + (nearest ? 0.5f : 0.0f));
    } else {
        theValue += (nearest ? 0.5f : 0.0f) / m_fieldConfigs.back().base;
        t_int = int(theValue);
    }
    double t_frac;
    if (theValue < 0) {
        t_frac = -1;
    } else {
        t_frac = (theValue - double(t_int));
    }

    int tenMins;
    int mins;
    int addMins;
    int secs;
    int frames;

    result.valueString = m_prefix;

    if (m_ntscDrop && theValue >= 0) {
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

    for (size_t i = 0; i < m_fields.size(); i++) {
        long long value = -1;

        if (m_fieldConfigs[i].frac) {
            // JKC: This old code looks bogus to me.
            // The rounding is not propagating to earlier fields in the frac
            // case.
            // value = (int)(t_frac * mFields[i].base + 0.5);  // +0.5 as
            // rounding required
            // I did the rounding earlier.
            if (t_frac >= 0) {
                value = t_frac * m_fieldConfigs[i].base;
            }
            // JKC: TODO: Find out what the range is supposed to do.
            // It looks bogus too.
            // if (mFields[i].range > 0)
            //   value = value % mFields[i].range;
        } else {
            if (t_int >= 0) {
                value = long(t_int) / m_fieldConfigs[i].base;
                if (m_fieldConfigs[i].range > 0) {
                    value = value % m_fieldConfigs[i].range;
                }
            }
        }

        char field[10];

        if (value < 0) {
            for (int ii = 0; ii < m_fields[i].digits; ++ii) {
                field[0] = '-';
            }
        } else {
            snprintf(field, sizeof(field), m_fields[i].formatStr.toStdString().c_str(), (int)value);
        }

        result.fieldValueStrings.push_back(field);

        result.valueString += field;
        result.valueString += m_fields[i].label;
    }

    return result;
}

std::optional<double> NumericFormatter::stringToValue(const QString& value) const
{
    unsigned int i;
    double t = 0.0;

    if (m_fields.size() > 0 && value.mid(m_fields[0].pos, 1) == '-') {
        return std::nullopt;
    }

    for (i = 0; i < m_fields.size(); i++) {
        const auto pos = m_fields[i].pos;
        const auto digits = m_fields[i].digits;

        if (pos >= value.size() || pos + digits > value.size()) {
            return std::nullopt;
        }

        long val;

        const auto fieldStringValue
            =value.mid(m_fields[i].pos, m_fields[i].digits);

        bool ok = false;
        val = fieldStringValue.toLong(&ok);
        if (!ok) {
            return std::nullopt;
        }

        if (m_fieldConfigs[i].frac) {
            t += (val / (double)m_fieldConfigs[i].base);
        } else {
            t += (val * (double)m_fieldConfigs[i].base);
        }
    }

    t /= m_scalingFactor;

    if (m_ntscDrop) {
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

double NumericFormatter::singleStep(double value, int digitIndex, bool upwards)
{
    const auto dir = upwards ? 1 : -1;
    for (size_t i = 0; i < m_fields.size(); i++) {
        if (
            (m_digits[digitIndex].pos >= m_fields[i].pos)
            && (m_digits[digitIndex].pos < m_fields[i].pos + m_fields[i].digits)) { // it's this field
            if (value < 0) {
                value = 0;
            }

            value *= m_scalingFactor;

            const double mult = pow(
                10., m_fields[i].digits
                - (m_digits[digitIndex].pos - m_fields[i].pos) - 1);

            if (m_fieldConfigs[i].frac) {
                value += ((mult / (double)m_fieldConfigs[i].base) * dir);
            } else {
                value += ((mult * (double)m_fieldConfigs[i].base) * dir);
            }

            if (m_ntscDrop) {
                if ((value - (int)value) * 30 < 2) {
                    if ((((int)value) % 60 == 0) && (((int)value) % 600 != 0)) {
                        value = (int)value + (dir > 0 ? 2. : -1.) / 30.;
                    }
                }
            }

            if (value < 0.) {
                value = 0.;
            }

            value /= m_scalingFactor;

            if (m_ntscDrop) {
                m_ntscDrop = false;
                auto result = valueToString(value, false);
                m_ntscDrop = true;
                return *stringToValue(result.valueString);
            }

            return value;
        }
    }

    return value;
}
