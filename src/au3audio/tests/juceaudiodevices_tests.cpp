/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "framework/global/containers.h"

#include "../internal/juce/juceaudiodevices.h"

namespace au::au4juce {
TEST(JuceAudioDevicesTests, ListsConsistentDevices)
{
    JuceAudioDevices devices;
    const auto apis = devices.apis();
    if (apis.empty()) {
        GTEST_SKIP() << "no audio device available";
    }

    EXPECT_TRUE(muse::contains(apis, devices.preferredApi(apis)));

    for (const auto& api : apis) {
        const auto outputs = devices.outputDevices(api);
        const auto inputs = devices.inputDevices(api);
        EXPECT_FALSE(outputs.empty() && inputs.empty()) << api;

        const auto defaultOutput = devices.systemDefaultOutputDevice(api);
        if (!defaultOutput.empty()) {
            EXPECT_TRUE(muse::contains(outputs, defaultOutput)) << api << ": " << defaultOutput;
        }
        const auto defaultInput = devices.systemDefaultInputDevice(api);
        if (!defaultInput.empty()) {
            EXPECT_TRUE(muse::contains(inputs, defaultInput)) << api << ": " << defaultInput;
            EXPECT_GT(devices.inputChannels(api, defaultInput), 0) << api << ": " << defaultInput;
        }
    }
}

TEST(JuceAudioDevicesTests, UnknownDevicesHaveNoChannels)
{
    JuceAudioDevices devices;
    EXPECT_EQ(devices.inputChannels("no such api", "no such device"), 0);
    for (const auto& api : devices.apis()) {
        EXPECT_EQ(devices.inputChannels(api, "no such device"), 0);
    }
}
}
