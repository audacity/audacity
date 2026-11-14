/*
 * Audacity: A Digital Audio Editor
 */
#include "shared/axis/axisticks.h"

#include "framework/global/types/number.h"

#include <gtest/gtest.h>

using namespace au::shared;

TEST(axisTicks, real_world_cases)
{
    // These test cases are based on real-world scenarios and helped tuning the algorithm.

    {
        constexpr auto min = 20.0;
        constexpr auto max = 24000.0;
        constexpr auto scale = AxisScale::Linear;
        const AxisTicks ticks = axisTicks(min, max, scale);

        EXPECT_GE(ticks.major.size(), kMinMajorTicks);

        // End ticks are always major and exactly on the min and max values.
        EXPECT_EQ(ticks.major.front().val, min);
        EXPECT_EQ(ticks.major.back().val, max);

        for (const auto freq : { 5000.0, 10000.0, 15000.0, 20000.0 }) {
            EXPECT_TRUE(std::any_of(ticks.major.begin(), ticks.major.end(), [freq](const AxisTick& tick) {
                return muse::is_equal(tick.val, freq);
            })) << "Expected major tick at " << freq;
        }
    }

    {
        constexpr auto min = 0.0;
        constexpr auto max = 23999.99609375;
        constexpr auto scale = AxisScale::Mel;
        const AxisTicks ticks = axisTicks(min, max, scale);

        EXPECT_GE(ticks.major.size(), kMinMajorTicks);

        // End ticks are always major and exactly on the min and max values.
        EXPECT_EQ(ticks.major.front().val, min);
        EXPECT_EQ(ticks.major.back().val, max);

        // Some expected major ticks
        EXPECT_TRUE(std::any_of(ticks.major.begin(), ticks.major.end(), [](const AxisTick& tick) {
            return muse::is_equal(tick.val, 10000.0);
        }));
        EXPECT_TRUE(std::any_of(ticks.major.begin(), ticks.major.end(), [](const AxisTick& tick) {
            return muse::is_equal(tick.val, 5000.0);
        }));
    }
}

TEST(axisTicks, normalized_scales_are_supported) {
    constexpr auto min = 0.0;
    constexpr auto max = 1.0;
    constexpr auto scale = AxisScale::Logarithmic;
    const AxisTicks ticks = axisTicks(min, max, scale);
    EXPECT_GE(ticks.major.size(), kMinMajorTicks);
}
