/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <vector>

using PRCrossfadeData = std::vector< std::vector < float > >;

//! Timing of a recording: pre-roll, latency correction, punch-and-roll crossfades, and progress
struct RecordingSchedule {
    double mLeadInTime{};
    double mLatencyCompensation{}; // negative value usually
    double mDuration{};
    PRCrossfadeData mCrossfadeData;

    // These are initialized by the main thread, then updated
    // only by the thread calling SequenceBufferExchange:
    double mPosition{};
    bool mLatencyCorrected{};

    double TotalCorrection() const { return mLatencyCompensation - mLeadInTime; }
    double ToConsume() const;
    double Consumed() const;
    double ToDiscard() const;
};
