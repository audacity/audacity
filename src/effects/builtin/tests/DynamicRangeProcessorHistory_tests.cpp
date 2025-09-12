/*
 * Audacity: A Digital Audio Editor
 */
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorHistory.h"

#include <gtest/gtest.h>
#include <gmock/gmock.h>

namespace au::effects {
namespace {
constexpr auto sampleRate = 42.0;
}

TEST(DynamicRangeProcessorHistoryTests, CalledBeforeAnyPush)
{
    DynamicRangeProcessorHistory history(sampleRate);
    const auto view = history.GetViewOnNewPackets();
    EXPECT_EQ(view.numPackets(), 0);
}

TEST(DynamicRangeProcessorHistoryTests, stuff) {
    DynamicRangeProcessorHistory history(sampleRate);

    constexpr auto sampsPerPacket = 1;
    history.Push({ { 1, sampsPerPacket }, { 2, sampsPerPacket } });
    const auto view = history.GetViewOnNewPackets();
    EXPECT_EQ(view.numPackets(), 2);

    history.Push({ { 3, sampsPerPacket } });
    const auto view2 = history.GetViewOnNewPackets();
    EXPECT_EQ(view2.numPackets(), 1);

    history.Push({ { 5, sampsPerPacket }, { 7, sampsPerPacket } });
    history.Push({ { 15, sampsPerPacket }, { 17, sampsPerPacket } });
    history.BeginNewSegment();
    history.Push({ { 100, sampsPerPacket, 10.f } });
    const auto view3 = history.GetViewOnNewPackets();
    ASSERT_EQ(view3.numPackets(), 5);
    EXPECT_EQ(view3.at(4).target, 10.f);
}
}
