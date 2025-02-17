/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterType.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include "Identifier.h"

using NumericConverterType = Identifier;

NUMERIC_FORMATS_API const NumericConverterType& NumericConverterType_TIME();
NUMERIC_FORMATS_API const NumericConverterType& NumericConverterType_DURATION();
NUMERIC_FORMATS_API const NumericConverterType& NumericConverterType_FREQUENCY();
NUMERIC_FORMATS_API const NumericConverterType& NumericConverterType_BANDWIDTH();
