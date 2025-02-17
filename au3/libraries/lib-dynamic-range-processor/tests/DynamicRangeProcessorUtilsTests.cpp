/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorUtilsTests.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DynamicRangeProcessorUtils.h"
#include <catch2/catch.hpp>

namespace DynamicRangeProcessorUtils {
TEST_CASE("DynamicRangeProcessorUtils")
{
    SECTION("GetCompressorPresets")
    {
        REQUIRE(
            GetCompressorPresets().size()
            == Detail::serializedCompressorPresets.size());
    }
    SECTION("GetLimiterPresets")
    {
        REQUIRE(
            GetLimiterPresets().size() == Detail::serializedLimiterPresets.size());
    }
}
} // namespace DynamicRangeProcessorUtils
