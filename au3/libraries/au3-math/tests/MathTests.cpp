/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MathTests.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "LinearFit.h"

#include <catch2/catch.hpp>

TEST_CASE("LinearFit")
{
    SECTION("exact fit, no weights")
    {
        const std::vector<double> x { 1.0, 2.0, 3.0, 4.0, 5.0 };
        const std::vector<double> y { 2.0, 3.0, 4.0, 5.0, 6.0 };
        const auto result = LinearFit(x, y);
        REQUIRE(result.first == 1.0);
        REQUIRE(result.second == 1.0);
    }

    SECTION("inexact fit, corrected by zero-weight")
    {
        const std::vector<double> x { 1.0, 2.0, 3.0, 4.0, 5.0 };
        const std::vector<double> y { 2.0, 3.0, 4.0, 5.0, 7.0 };
        const std::vector<double> weights { 1.0, 1.0, 1.0, 1.0, 0.0 };
        const auto result = LinearFit(x, y, weights);
        REQUIRE(result.first == 1.0);
        REQUIRE(result.second == 1.0);
    }

    SECTION("exact fit, random weights")
    {
        const std::vector<double> x { 1.0, 2.0, 3.0, 4.0, 5.0 };
        const std::vector<double> y { 2.0, 3.0, 4.0, 5.0, 6.0 };
        const std::vector<double> weights { 1.0, 2.0, 1.0, 3.0, 1.0 };
        const auto result = LinearFit(x, y, weights);
        REQUIRE(result.first == 1.0);
        REQUIRE(result.second == 1.0);
    }

    SECTION("exact fit with int x and y input vectors")
    {
        const std::vector<int> x { 1, 2, 3, 4, 5 };
        const std::vector<int> y { 2, 3, 4, 5, 6 };
        const std::vector<double> weights { 1.0, 1.0, 1.0, 1.0, 1.0 };
        const auto result = LinearFit(x, y, weights);
        REQUIRE(result.first == 1.0);
        REQUIRE(result.second == 1.0);
    }

    SECTION("exact fit, with non-unit gradient")
    {
        const std::vector<double> x { 1.0, 2.0, 3.0, 4.0, 5.0 };
        const std::vector<double> y { 2.0, 4.0, 6.0, 8.0, 10.0 };
        const std::vector<double> weights { 1.0, 1.0, 1.0, 1.0, 0.0 };
        const auto result = LinearFit(x, y, weights);
        REQUIRE(result.first == 2.0);
        REQUIRE(result.second == 0.0);
    }
}
