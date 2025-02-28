/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterFormats.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include "NumericConverterType.h"
#include "ComponentInterfaceSymbol.h"

class FormatterContext;

namespace NumericConverterFormats {
struct DefaultFormatRegistrator final
{
    explicit DefaultFormatRegistrator(
        const NumericConverterType& type, const NumericFormatSymbol& symbol);
};

//! Returns the default format for the type or empty symbol, if no default symbol is registered
NUMERIC_FORMATS_API NumericFormatSymbol Default(const NumericConverterType& type);

//! Looks up the format, returns Default for the type if the format is not registered
NUMERIC_FORMATS_API NumericFormatSymbol Lookup(
    const FormatterContext& context, const NumericConverterType& type, const NumericFormatID& formatIdentifier);

NUMERIC_FORMATS_API NumericFormatSymbol DefaultSelectionFormat();
NUMERIC_FORMATS_API NumericFormatSymbol TimeAndSampleFormat();
NUMERIC_FORMATS_API NumericFormatSymbol SecondsFormat();
NUMERIC_FORMATS_API NumericFormatSymbol HoursMinsSecondsFormat();
NUMERIC_FORMATS_API NumericFormatSymbol HundredthsFormat();
NUMERIC_FORMATS_API NumericFormatSymbol MillisecondsFormat();
NUMERIC_FORMATS_API NumericFormatSymbol HertzFormat();
NUMERIC_FORMATS_API NumericFormatSymbol OctavesFormat();

//! Return the best duration format for the given time format. Currently is an identity function
NUMERIC_FORMATS_API NumericFormatID
GetBestDurationFormat(const NumericFormatID& timeFormat);
}
