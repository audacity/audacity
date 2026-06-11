/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterFormats.cpp

 Dmitry Vedenko

 **********************************************************************/

#include "NumericConverterFormats.h"
#include "NumericConverterRegistry.h"
#include "NumericConverterFormatterContext.h"

#include <cassert>
#include <unordered_map>

namespace {
std::unordered_map<NumericConverterType, NumericFormatSymbol>& GetDefaultSymbols()
{
    static std::unordered_map<NumericConverterType, NumericFormatSymbol> symbols;
    return symbols;
}
}

namespace NumericConverterFormats {
DefaultFormatRegistrator::DefaultFormatRegistrator(
    const NumericConverterType& type, const NumericFormatSymbol& symbol)
{
    auto& defaultSymbols = GetDefaultSymbols();

    if (defaultSymbols.find(type) != defaultSymbols.end()) {
        // We do not allow to register multiple defaults
        // for a single type
        assert(false);
        return;
    }

    defaultSymbols.emplace(type, symbol);
}

NumericFormatSymbol Default(const NumericConverterType& type)
{
    auto& defaultSymbols = GetDefaultSymbols();

    auto it = defaultSymbols.find(type);

    if (it != defaultSymbols.end()) {
        return it->second;
    }

    // Fail the debug build early
    assert(false);

    return {};
}

NUMERIC_FORMATS_API NumericFormatSymbol Lookup(
    const FormatterContext& context,
    const NumericConverterType& type,
    const NumericFormatID& formatIdentifier)
{
    if (formatIdentifier.empty()) {
        return Default(type);
    }

    auto result = NumericConverterRegistry::Find(context, type, { formatIdentifier });

    if (result == nullptr) {
        return Default(type);
    }

    return result->symbol;
}

NUMERIC_FORMATS_API NumericFormatSymbol DefaultSelectionFormat()
{
    return MillisecondsFormat();
}

NUMERIC_FORMATS_API NumericFormatSymbol TimeAndSampleFormat()
{
    /*: Name of time display format that shows time in hours,
     * minutes, seconds and samples (at the current project sample rate) */
    return { TranslatableString("numeric-formats", "hh:mm:ss + samples") };
}

NUMERIC_FORMATS_API NumericFormatSymbol SecondsFormat()
{
    /*: Name of time display format that shows time in seconds */
    return { TranslatableString("numeric-formats", "seconds") };
}

NUMERIC_FORMATS_API NumericFormatSymbol HoursMinsSecondsFormat()
{
    /*: Name of time display format that shows time in hours, minutes
     * and seconds */
    return { TranslatableString("numeric-formats", "hh:mm:ss") };
}

NUMERIC_FORMATS_API NumericFormatSymbol MillisecondsFormat()
{
    /*: Name of time display format that shows time in hours,
     * minutes, seconds and milliseconds (1/1000 second) */
    return { TranslatableString("numeric-formats", "hh:mm:ss + milliseconds") };
}

NUMERIC_FORMATS_API NumericFormatSymbol HundredthsFormat()
{
    /*: Name of time display format that shows time in hours,
     * minutes, seconds and hundredths of a second (1/100 second) */
    return { TranslatableString("numeric-formats", "hh:mm:ss + hundredths") };
}

NUMERIC_FORMATS_API NumericFormatSymbol HertzFormat()
{
    /*: Name of display format that shows frequency in hertz */
    return { TranslatableString("numeric-formats", "Hz") };
}

NUMERIC_FORMATS_API NumericFormatSymbol OctavesFormat()
{
    /*: Name of display format that shows log of frequency
     * in octaves */
    return { TranslatableString("numeric-formats", "octaves") };
}

NUMERIC_FORMATS_API NumericFormatID
GetBestDurationFormat(const NumericFormatID& timeFormat)
{
    return timeFormat;
}
} // namespace NumericConverterFormats
