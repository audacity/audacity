/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <LoopTempoEstimator/LteTypes.h>

#include <array>
#include <memory>
#include <optional>

#include "au3-stretching-sequence/AudioSegmentSampleView.h"

class ClipInterface;

namespace au::importexport {
class LteAudioReaderBridge : public LTE::LteAudioReader
{
public:
    explicit LteAudioReaderBridge(std::shared_ptr<const ClipInterface> clip);

    double GetSampleRate() const override;
    long long GetNumSamples() const override;
    void ReadFloats(float* buffer, long long where, size_t numFrames) const override;

private:
    void addChannel(size_t iChannel, float* buffer, long long start, size_t len) const;

    std::shared_ptr<const ClipInterface> m_clip;

    // Cache for sample views (two channels, two alternating caches each
    // to handle back-and-forth access patterns used by the FFT algorithm)
    using ChannelCache = std::array<std::optional<AudioSegmentSampleView>, 2>;
    mutable std::array<ChannelCache, 2> m_cache;
    mutable std::array<bool, 2> m_useFirst { true, true };
};
}
