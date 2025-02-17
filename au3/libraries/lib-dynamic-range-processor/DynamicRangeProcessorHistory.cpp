/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorHistory.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DynamicRangeProcessorHistory.h"
#include "DynamicRangeProcessorTypes.h"
#include <algorithm>
#include <cassert>
#include <iterator>

DynamicRangeProcessorHistory::DynamicRangeProcessorHistory(double sampleRate)
    : mSampleRate{sampleRate}
{
}

void DynamicRangeProcessorHistory::Push(
    const std::vector<DynamicRangeProcessorOutputPacket>& packets)
{
    if (packets.empty()) {
        return;
    }

    if (!mFirstPacketFirstSampleIndex.has_value()) {
        mFirstPacketFirstSampleIndex = packets.front().indexOfFirstSample;
    }

    const int numNewPackets = packets.size();
    const auto lastPacketTime = !mSegments.empty() && !mSegments[0].empty()
                                ? std::make_optional(mSegments[0].back().time)
                                : std::nullopt;

    const auto firstPacketToInsertIt
        =std::find_if(packets.begin(), packets.end(), [&](const auto& packet) {
        return !lastPacketTime.has_value()
               || GetPacketTime(packet) > *lastPacketTime;
    });

    if (firstPacketToInsertIt == packets.end()) {
        return;
    }

    // Some packets can go lost in the transmission from audio to main thread.
    const auto isContinuous = mExpectedNextPacketFirstSampleIndex.has_value()
                              && firstPacketToInsertIt->indexOfFirstSample
                              == *mExpectedNextPacketFirstSampleIndex;
    if (mSegments.empty() || mBeginNewSegment || !isContinuous) {
        mSegments.emplace_back();
        mBeginNewSegment = false;
    }
    mExpectedNextPacketFirstSampleIndex
        =packets.back().indexOfFirstSample + packets.back().numSamples;

    auto& lastSegment = mSegments.back();

    std::transform(
        firstPacketToInsertIt, packets.end(), std::back_inserter(lastSegment),
        [&](const auto& packet) -> Packet {
        const auto t = GetPacketTime(packet);
        return { t, packet.targetCompressionDb, packet.actualCompressionDb,
                 packet.inputDb, packet.outputDb };
    });

    // Clean up older packets.
    // Algorithmically it's not completely correct to only do this for the oldest
    // segment, but in practice, `Push` is called much more often than
    // `BeginNewSegment`, so a simpler implementation is sufficient.
    const auto lastTime = lastSegment.back().time;
    auto& firstSegment = mSegments.front();
    const auto it = std::find_if(
        firstSegment.begin(), firstSegment.end(),
        [lastTime](const Packet& packet) {
        // Extend a little bit the time window, to avoid the extremities of a
        // display to tremble.
        return lastTime - packet.time < maxTimeSeconds + 1.f;
    });
    firstSegment.erase(firstSegment.begin(), it);

    if (firstSegment.empty()) {
        mSegments.erase(mSegments.begin());
    }
}

void DynamicRangeProcessorHistory::BeginNewSegment()
{
    mBeginNewSegment = true;
}

const std::vector<DynamicRangeProcessorHistory::Segment>&
DynamicRangeProcessorHistory::GetSegments() const
{
    return mSegments;
}

bool DynamicRangeProcessorHistory::IsEmpty() const
{
    return std::all_of(
        mSegments.begin(), mSegments.end(),
        [](const auto& segment) { return segment.empty(); });
}

float DynamicRangeProcessorHistory::GetPacketTime(
    const DynamicRangeProcessorOutputPacket& packet) const
{
    assert(mFirstPacketFirstSampleIndex.has_value());
    return (packet.indexOfFirstSample
            - mFirstPacketFirstSampleIndex.value_or(0))
           / mSampleRate;
}
