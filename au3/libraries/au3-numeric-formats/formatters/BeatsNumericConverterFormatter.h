/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file BeatsNumericConverterFormatter.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include <memory>
#include "NumericConverterFormatter.h"

class FormatterContext;

NUMERIC_FORMATS_API std::unique_ptr<NumericConverterFormatter>
CreateBeatsNumericConverterFormatter(
    const FormatterContext& context, int fracPart = 0, bool timeFormat = true);
