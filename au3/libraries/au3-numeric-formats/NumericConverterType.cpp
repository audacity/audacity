/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterType.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "NumericConverterType.h"

const NumericConverterType& NumericConverterType_TIME()
{
    static NumericConverterType value { L"time" };
    return value;
}

const NumericConverterType& NumericConverterType_DURATION()
{
    static NumericConverterType value { L"duration" };
    return value;
}

const NumericConverterType& NumericConverterType_FREQUENCY()
{
    static NumericConverterType value { L"frequency" };
    return value;
}

const NumericConverterType& NumericConverterType_BANDWIDTH()
{
    static NumericConverterType value { L"bandwidth" };
    return value;
}
