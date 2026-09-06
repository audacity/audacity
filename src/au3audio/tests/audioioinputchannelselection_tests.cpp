/*
 * Audacity: A Digital Audio Editor
 */
#include <iterator>

#include <gtest/gtest.h>

#include "au3-audio-io/internal/AudioIOInputChannelSelection.h"

namespace au::au3audio {
namespace details = audacity::audio_io::details;

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
}
