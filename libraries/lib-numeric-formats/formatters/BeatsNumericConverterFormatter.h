/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file ParsedNumericConverterFormatter.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include <memory>
#include "NumericConverterFormatter.h"

NUMERIC_FORMATS_API std::unique_ptr<NumericConverterFormatter>
CreateBeatsNumericConverterFormatter(
   double tempo, int upperTimeSignature, int lowerTimeSignature,
   int fracPart = 0);


