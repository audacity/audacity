/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <utility>

#include "au3-clip-processing/ClipFades.h"

namespace {
constexpr auto TEST_NUM_STEPS = 100;
constexpr auto TEST_TOLERANCE = 1e-9;

double positionAt(int step)
{
    return static_cast<double>(step) / TEST_NUM_STEPS;
}

class FadeGainTests : public ::testing::TestWithParam<FadeType>
{
};

class InverseFadeGainTests : public ::testing::TestWithParam<std::pair<FadeType, FadeType> >
{
};
}

TEST_P(FadeGainTests, GoesFromSilenceToUnity)
{
    EXPECT_NEAR(ClipFades::fadeGain(GetParam(), 0.0), 0.0, TEST_TOLERANCE);
    EXPECT_NEAR(ClipFades::fadeGain(GetParam(), 1.0), 1.0, TEST_TOLERANCE);
}

TEST_P(FadeGainTests, IsMonotonicallyIncreasing)
{
    for (auto step = 1; step <= TEST_NUM_STEPS; ++step) {
        EXPECT_GT(ClipFades::fadeGain(GetParam(), positionAt(step)), ClipFades::fadeGain(GetParam(), positionAt(step - 1)));
    }
}

INSTANTIATE_TEST_SUITE_P(AllFadeTypes, FadeGainTests,
                         ::testing::Values(FadeType::Linear, FadeType::Exponential, FadeType::Logarithmic));

TEST_P(InverseFadeGainTests, OuterFadeUndoesInnerFade)
{
    const auto [inner, outer] = GetParam();
    for (auto step = 0; step <= TEST_NUM_STEPS; ++step) {
        const auto t = positionAt(step);
        EXPECT_NEAR(ClipFades::fadeGain(outer, ClipFades::fadeGain(inner, t)), t, TEST_TOLERANCE);
    }
}

INSTANTIATE_TEST_SUITE_P(LogarithmicAndExponential, InverseFadeGainTests,
                         ::testing::Values(std::pair { FadeType::Exponential, FadeType::Logarithmic },
                                           std::pair { FadeType::Logarithmic, FadeType::Exponential }));
