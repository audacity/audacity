/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "au3-recording/CaptureRouting.h"

using Sources = std::vector<std::vector<size_t> >;

namespace au::au3audio {
namespace {
Sources inputChannelsOf(const CaptureRouting& routing)
{
    Sources result;
    for (const CaptureChannelSources& channel : routing.trackChannels) {
        const auto inputs = channel.inputChannels();
        result.emplace_back(inputs.begin(), inputs.end());
    }
    return result;
}
}

TEST(CaptureRoutingTests, MonoInputIntoAMonoChannelIsRecordedAsIs)
{
    //! [WHEN] One input channel is recorded into one track channel
    const CaptureRouting routing = MakeCaptureRouting(1, 1);

    //! [THEN] The input goes into it unchanged
    EXPECT_EQ(inputChannelsOf(routing), (Sources { { 0 } }));
    EXPECT_FALSE(routing.needsMixdown);
}

TEST(CaptureRoutingTests, StereoInputIntoAMonoChannelIsMixedDown)
{
    //! [WHEN] Two input channels are recorded into one track channel
    const CaptureRouting routing = MakeCaptureRouting(2, 1);

    //! [THEN] Both inputs are mixed into it
    EXPECT_EQ(inputChannelsOf(routing), (Sources { { 0, 1 } }));
    EXPECT_TRUE(routing.needsMixdown);
}

TEST(CaptureRoutingTests, MonoInputIntoTwoChannelsIsDuplicated)
{
    //! [WHEN] One input channel is recorded into two track channels (a stereo track)
    const CaptureRouting routing = MakeCaptureRouting(1, 2);

    //! [THEN] The input goes into both
    EXPECT_EQ(inputChannelsOf(routing), (Sources { { 0 }, { 0 } }));
    EXPECT_FALSE(routing.needsMixdown);
}

TEST(CaptureRoutingTests, StereoInputIntoTwoChannelsIsSplitLeftAndRight)
{
    //! [WHEN] Two input channels are recorded into two track channels (a stereo track or two mono tracks)
    const CaptureRouting routing = MakeCaptureRouting(2, 2);

    //! [THEN] Left goes into the first channel, right into the second
    EXPECT_EQ(inputChannelsOf(routing), (Sources { { 0 }, { 1 } }));
    EXPECT_FALSE(routing.needsMixdown);
}

TEST(CaptureRoutingTests, MoreInputsThanChannelsTakesTheFirstInputs)
{
    //! [WHEN] Four input channels are recorded into three track channels
    const CaptureRouting routing = MakeCaptureRouting(4, 3);

    //! [THEN] The first three inputs go into them in order, the fourth is dropped
    EXPECT_EQ(inputChannelsOf(routing), (Sources { { 0 }, { 1 }, { 2 } }));
    EXPECT_FALSE(routing.needsMixdown);
}

TEST(CaptureRoutingTests, MoreThanTwoInputsIntoAMonoChannelTakesTheFirstInput)
{
    //! [WHEN] Four input channels are recorded into one track channel
    const CaptureRouting routing = MakeCaptureRouting(4, 1);

    //! [THEN] Only the first input is recorded, without mixing
    EXPECT_EQ(inputChannelsOf(routing), (Sources { { 0 } }));
    EXPECT_FALSE(routing.needsMixdown);
}

TEST(CaptureRoutingTests, MoreChannelsThanInputsLeavesTheExtraChannelsEmpty)
{
    //! [WHEN] Three input channels are recorded into four track channels
    const CaptureRouting routing = MakeCaptureRouting(3, 4);

    //! [THEN] The fourth channel gets no input
    EXPECT_EQ(inputChannelsOf(routing), (Sources { { 0 }, { 1 }, { 2 }, {} }));
    EXPECT_FALSE(routing.needsMixdown);
}

TEST(CaptureRoutingTests, NoInputsOrNoChannelsRecordsNothing)
{
    EXPECT_TRUE(MakeCaptureRouting(0, 2).trackChannels.empty());
    EXPECT_TRUE(MakeCaptureRouting(2, 0).trackChannels.empty());
}
}
