/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file FromCharsTests.cpp
 @brief Tests for the FromChars functions

 Dmitry Vedenko
 **********************************************************************/

#include <catch2/catch.hpp>

#include <string>
#include <string_view>
#include <type_traits>

#include "FromChars.h"

template<typename T>
void TestFromChars(std::string_view input, T expectedValue, std::errc errc = {})
{
    T value;

    const auto result
        =FromChars(input.data(), input.data() + input.length(), value);

    REQUIRE(errc == result.ec);

    if (errc == std::errc {}) {
        if constexpr (std::is_floating_point_v<std::decay_t<T> >) {
            REQUIRE(Approx(expectedValue) == value);
        } else {
            REQUIRE(expectedValue == value);
        }

        REQUIRE(result.ptr == input.data() + input.length());
    }
}

TEMPLATE_TEST_CASE(
    "FromChars/signed integers", "", short, int, long, long long)
{
    TestFromChars<TestType>("0", 0, {});
    TestFromChars<TestType>("1", 1, {});
    TestFromChars<TestType>("-1", -1, {});
    TestFromChars<TestType>("127", 127, {});
    TestFromChars<TestType>("-12", -12, {});

    TestFromChars<TestType>("", {}, std::errc::invalid_argument);
    TestFromChars<TestType>("a", {}, std::errc::invalid_argument);

    TestFromChars<TestType>(
        std::to_string(std::numeric_limits<TestType>::min()),
        std::numeric_limits<TestType>::min(), {});

    TestFromChars<TestType>(
        std::to_string(std::numeric_limits<TestType>::max()),
        std::numeric_limits<TestType>::max(), {});

    TestFromChars<TestType>(
        std::to_string(std::numeric_limits<TestType>::min()) + "0",
        std::numeric_limits<TestType>::min(), std::errc::result_out_of_range);

    TestFromChars<TestType>(
        std::to_string(std::numeric_limits<TestType>::max()) + "0",
        std::numeric_limits<TestType>::max(), std::errc::result_out_of_range);
}

TEMPLATE_TEST_CASE(
    "FromChars/unsigned integers", "", unsigned short, unsigned int,
    unsigned long, unsigned long long)
{
    TestFromChars<TestType>("0", 0, {});
    TestFromChars<TestType>("1", 1, {});
    TestFromChars<TestType>("-1", -1, std::errc::invalid_argument);
    TestFromChars<TestType>("127", 127, {});
    TestFromChars<TestType>("-12", -12, std::errc::invalid_argument);

    TestFromChars<TestType>("", {}, std::errc::invalid_argument);
    TestFromChars<TestType>("a", {}, std::errc::invalid_argument);

    TestFromChars<TestType>(
        std::to_string(std::numeric_limits<TestType>::min()),
        std::numeric_limits<TestType>::min(), {});

    TestFromChars<TestType>(
        std::to_string(std::numeric_limits<TestType>::max()),
        std::numeric_limits<TestType>::max(), {});

    TestFromChars<TestType>(
        std::to_string(std::numeric_limits<TestType>::max()) + "0",
        std::numeric_limits<TestType>::max(), std::errc::result_out_of_range);
}

TEMPLATE_TEST_CASE("FromChars/floats", "", float, double)
{
    TestFromChars<TestType>("0", 0, {});
    TestFromChars<TestType>("0.0", 0, {});
    TestFromChars<TestType>(".0", 0, {});
    TestFromChars<TestType>("1", 1, {});
    TestFromChars<TestType>("-1", -1, {});
    TestFromChars<TestType>("127", 127, {});
    TestFromChars<TestType>("-12", -12, {});
    TestFromChars<TestType>("-12.5", -12.5, {});
    TestFromChars<TestType>("3.1415", 3.1415, {});
    TestFromChars<TestType>("3.14159265359", 3.14159265359, {});

    for (auto magnitude = std::numeric_limits<TestType>::min_exponent10;
         magnitude <= std::numeric_limits<TestType>::max_exponent10; ++magnitude) {
        TestFromChars(
            "1e" + std::to_string(magnitude), std::pow(10, magnitude), {});

        TestFromChars(
            "-1e" + std::to_string(magnitude), -std::pow(10, magnitude), {});
    }

    TestFromChars<TestType>("", {}, std::errc::invalid_argument);
    TestFromChars<TestType>("a", {}, std::errc::invalid_argument);

    TestFromChars<TestType>(
        "1e1000", std::numeric_limits<TestType>::infinity(), {});
}
