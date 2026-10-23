/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file LoopbackLatency.h
 @brief Round-trip latency from a burst train played and recorded back

 A pseudo-random burst is played once per period. Output and input share one
 frame counter, so the offset of each burst in the captured signal is the
 round trip in frames.

 **********************************************************************/

#ifndef __AUDACITY_LOOPBACK_LATENCY__
#define __AUDACITY_LOOPBACK_LATENCY__

#include <cstddef>
#include <vector>

namespace LoopbackLatency {
constexpr size_t BurstLength = 512;
//! Minimum ratio of the correlation peak to the RMS of all correlation values
constexpr double MinConfidence = 8.0;

AUDIO_IO_API std::vector<float> MakeBurst(float gain);

struct Hit {
    long lag = -1;
    double confidence = 0.0;
    bool inverted = false;
};

//! Best match of `burst` in `capture[begin, begin + span + burst.size())`
AUDIO_IO_API Hit FindBurst(const std::vector<float>& capture, const std::vector<float>& burst, size_t begin, size_t span);

struct Analysis {
    //! Median over the found bursts; -1 when none was found
    long roundTripFrames = -1;
    size_t burstsFound = 0;
    size_t burstsTotal = 0;
    //! Largest minus smallest offset; clock drift between devices shows here
    long spreadFrames = 0;
    double minConfidence = 0.0;
    bool inverted = false;
    float peak = 0.0f;
};

//! `capture` holds one channel that starts at the first played frame
AUDIO_IO_API Analysis Analyse(const std::vector<float>& capture, const std::vector<float>& burst, size_t periodFrames);
}

#endif
