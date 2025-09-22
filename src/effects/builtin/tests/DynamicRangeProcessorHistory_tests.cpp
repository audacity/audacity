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

TEST(DynamicRangeProcessorHistoryTests, initial_state)
{
    //! [GIVEN] A DynamicRangeProcessorHistory object is freshly created
    DynamicRangeProcessorHistory history(sampleRate);

    //! [WHEN] `GetViewOnNewPackets()` is called
    const auto view = history.GetViewOnNewPackets();

    //! [THEN] the view reports no packets.
    EXPECT_EQ(view.numPackets(), 0);
}

TEST(DynamicRangeProcessorHistoryTests, simple_push) {
    //! [GIVEN] `Push` is called once with two packets
    DynamicRangeProcessorHistory history(sampleRate);
    constexpr auto sampsPerPacket = 1;
    history.Push({ { 1, sampsPerPacket }, { 2, sampsPerPacket } });

    //! [WHEN] GetViewOnNewPackets() is called
    const auto view = history.GetViewOnNewPackets();

    //! [THEN] the view reports two new packets
    EXPECT_EQ(view.numPackets(), 2);
}

TEST(DynamicRangeProcessorHistoryTests, GetViewOnNewPackets_modifies_state_correctly) {
    //! [GIVEN] `GetViewOnNewPackets()` was called after packets were pushed
    DynamicRangeProcessorHistory history(sampleRate);
    constexpr auto sampsPerPacket = 1;
    history.Push({ { 1, sampsPerPacket }, { 2, sampsPerPacket } });
    (void)history.GetViewOnNewPackets();

    //! [WHEN] `GetViewOnNewPackets()` is called again without packet pushes in between
    const auto view = history.GetViewOnNewPackets();

    //! [THEN] the view reports no new packet.
    EXPECT_EQ(view.numPackets(), 0);
}

TEST(DynamicRangeProcessorHistoryTests, GetViewOnNewPackets_returns_expected_info_in_complex_scenario) {
    //! [GIVEN] `Push` was called repeatedly, with disjunct packet sequences, `BeginNewSegment()` was called, and `Push` was called once again with some packets
    DynamicRangeProcessorHistory history(sampleRate);
    constexpr auto sampsPerPacket = 1;
    history.Push({ { 5, sampsPerPacket }, { 7, sampsPerPacket } });
    history.Push({ { 15, sampsPerPacket }, { 17, sampsPerPacket } });
    history.BeginNewSegment();
    constexpr auto arbitraryTargetCompression = 10.f;
    history.Push({ { 100, sampsPerPacket, arbitraryTargetCompression } });

    //! [WHEN] `GetViewOnNewPackets()` is called
    const auto view = history.GetViewOnNewPackets();

    //! [THEN] the view reports the total number of packets with the correct packet info.
    ASSERT_EQ(view.numPackets(), 5);
    EXPECT_EQ(view.at(4).target, arbitraryTargetCompression);
}
}
