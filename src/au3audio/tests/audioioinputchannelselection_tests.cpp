/*
 * Audacity: A Digital Audio Editor
 */
#include <iterator>

#include <gtest/gtest.h>

#include "au3-audio-io/internal/AudioIOInputChannelSelection.h"

namespace au::au3audio {
namespace details = audacity::audio_io::details;

TEST(AudioIOInputChannelSelectionTests, AcceptsGenericExecutableRoutes)
{
    EXPECT_TRUE(details::IsStructurallyValidInputChannelSelection({ { 1, 2 } }));
    EXPECT_TRUE(details::IsStructurallyValidInputChannelSelection({ { 3 }, { 1, 2 } }));
}

TEST(AudioIOInputChannelSelectionTests, RejectsMalformedAndOverlappingRoutes)
{
    EXPECT_FALSE(details::IsStructurallyValidInputChannelSelection({ {} }));
    EXPECT_FALSE(details::IsStructurallyValidInputChannelSelection({ { 0, 1, 2 } }));
    EXPECT_FALSE(details::IsStructurallyValidInputChannelSelection({ { 0 }, { 0, 1 } }));
    EXPECT_FALSE(details::IsStructurallyValidInputChannelSelection({ { 2, 2 } }));
}

TEST(AudioIOInputChannelSelectionTests, DerivesLegacyRouteCountOrderAndStreamWidth)
{
    const details::InputChannelSelection selection { { 0 }, { 2, 3 }, { 7 } };

    EXPECT_EQ(details::LegacyInputChannelSelection(0), details::InputChannelSelection {});
    EXPECT_EQ(details::LegacyInputChannelSelection(1), details::InputChannelSelection({ { 0 } }));
    EXPECT_EQ(details::LegacyInputChannelSelection(2), details::InputChannelSelection({ { 0, 1 } }));
    EXPECT_EQ(details::LegacyInputChannelSelection(3),
              details::InputChannelSelection({ { 0 }, { 1 }, { 2 } }));
    EXPECT_EQ(details::InputChannelSelectionCount(selection), 4u);
    EXPECT_EQ(details::FlattenInputChannelSelection(selection),
              std::vector<unsigned int>({ 0, 2, 3, 7 }));
    EXPECT_EQ(details::InputChannelSelectionStreamWidth(selection), 8u);
}

TEST(AudioIOInputChannelSelectionTests, CopiesOnlyTheRequestedPhysicalInputChannel)
{
    const float input[] {
        10.0f, 11.0f, 12.0f, 13.0f,
        20.0f, 21.0f, 22.0f, 23.0f,
        30.0f, 31.0f, 32.0f, 33.0f,
    };
    float output[] { -1.0f, -1.0f, -1.0f };

    EXPECT_TRUE(details::CopyInputChannel(input, 4, 2, output, 3));
    EXPECT_EQ(std::vector<float>(std::begin(output), std::end(output)),
              std::vector<float>({ 12.0f, 22.0f, 32.0f }));
}

TEST(AudioIOInputChannelSelectionTests, CopiesIntegerInputWithoutChangingSampleValues)
{
    const short input[] { -32768, 3, 32767, 7 };
    short output[] { 0, 0 };

    EXPECT_TRUE(details::CopyInputChannel(input, 2, 0, output, 2));
    EXPECT_EQ(std::vector<short>(std::begin(output), std::end(output)),
              std::vector<short>({ -32768, 32767 }));
}

TEST(AudioIOInputChannelSelectionTests, RejectsCopyOutsideTheInputStream)
{
    const float input[] { 1.0f, 2.0f };
    float output = 99.0f;

    EXPECT_FALSE(details::CopyInputChannel(input, 2, 2, &output, 1));
    EXPECT_FLOAT_EQ(output, 99.0f);
}

TEST(AudioIOInputChannelSelectionTests, MixesMonoAndStereoGroupsForMonitoring)
{
    const float input[] {
        0.9f, 0.2f, -0.4f, 0.8f,
        -0.6f, 0.1f, 0.4f, -0.2f,
    };
    float output[] { 0.0f, 0.0f, 0.0f, 0.0f };

    details::MixInputChannelSelectionToStereo(
        input, 4, { { 0 }, { 2, 3 } }, output, 2);

    EXPECT_FLOAT_EQ(output[0], 0.25f);
    EXPECT_FLOAT_EQ(output[1], 0.85f);
    EXPECT_FLOAT_EQ(output[2], -0.1f);
    EXPECT_FLOAT_EQ(output[3], -0.4f);
}

TEST(AudioIOInputChannelSelectionTests, MonitoringMixClampsEachOutputChannel)
{
    const float input[] { 2.0f, -2.0f };
    float output[] { 0.0f, 0.0f };

    details::MixInputChannelSelectionToStereo(
        input, 2, { { 0, 1 } }, output, 1);

    EXPECT_FLOAT_EQ(output[0], 1.0f);
    EXPECT_FLOAT_EQ(output[1], -1.0f);
}

TEST(AudioIOInputChannelSelectionTests, PeakIgnoresUnselectedPrefixChannels)
{
    const float input[] {
        0.1f, 0.99f, 0.3f, -0.7f,
        0.2f, -1.0f, -0.6f, 0.4f,
    };

    EXPECT_FLOAT_EQ(details::InputChannelSelectionPeak(
                        input, 4, { 2, 3 }, 2), 0.7f);
}
}
