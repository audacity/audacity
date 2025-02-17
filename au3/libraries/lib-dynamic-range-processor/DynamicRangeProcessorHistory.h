/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorHistory.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorTypes.h"
#include <memory>
#include <optional>
#include <vector>

class DYNAMIC_RANGE_PROCESSOR_API DynamicRangeProcessorHistory final
{
public:
    DynamicRangeProcessorHistory(double sampleRate);

    struct Packet
    {
        float time = 0.f;
        float target = 0.f;
        float follower = 0.f;
        float input = 0.f;
        float output = 0.f;
    };

    static constexpr auto maxTimeSeconds = 2.5f;

    using Segment = std::vector<Packet>;

    void Push(const std::vector<DynamicRangeProcessorOutputPacket>& packets);
    void BeginNewSegment();
    const std::vector<Segment>& GetSegments() const;
    bool IsEmpty() const;

private:
    float GetPacketTime(const DynamicRangeProcessorOutputPacket& packet) const;

    const double mSampleRate;
    bool mBeginNewSegment = true;
    std::vector<Segment> mSegments;
    std::optional<long long> mFirstPacketFirstSampleIndex;
    std::optional<long long> mExpectedNextPacketFirstSampleIndex;
};

class DynamicRangeProcessorHistory;
