/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file ParsedNumericConverterFormatter.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include <memory>

#include "NumericConverterType.h"
#include "NumericConverterFormatter.h"
#include "TranslatableString.h"

class FormatterContext;

NUMERIC_FORMATS_API std::unique_ptr<NumericConverterFormatter>
CreateParsedNumericConverterFormatter(
    const FormatterContext& context, NumericConverterType type, const TranslatableString& format);
