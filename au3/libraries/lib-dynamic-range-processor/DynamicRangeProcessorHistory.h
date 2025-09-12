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
    using Segments = std::vector<Segment>;

    class PacketView
    {
    public:
        int numPackets() const;
        const Packet& at(int i) const;

    private:
        friend class DynamicRangeProcessorHistory;
        PacketView(const DynamicRangeProcessorHistory& history, int numViewedPackets);

        const int m_numViewedPackets;
        const DynamicRangeProcessorHistory& m_history;
    };

    void Push(const std::vector<DynamicRangeProcessorOutputPacket>& packets);
    void BeginNewSegment();
    PacketView GetViewOnNewPackets();
    const Segments& GetSegments() const;
    bool IsEmpty() const;

private:
    float GetPacketTime(const DynamicRangeProcessorOutputPacket& packet) const;
    int TotalNumPackets() const;

    const double mSampleRate;
    bool mBeginNewSegment = true;
    Segments mSegments;
    int m_numViewedPackets = 0;
    std::optional<long long> mFirstPacketFirstSampleIndex;
    std::optional<long long> mExpectedNextPacketFirstSampleIndex;
};

class DynamicRangeProcessorHistory;
