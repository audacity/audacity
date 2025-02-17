/**********************************************************************

  Audacity: A Digital Audio Editor

  DownmixStage.h

*******************************************************************/

#pragma once

#include <vector>
#include <memory>

#include "AudioGraphBuffers.h"
#include "AudioGraphSource.h"

class DownmixSource;

//! Combines multiple audio graph sources into a single source
class DownmixStage final : public AudioGraph::Source
{
public:
    enum class ApplyVolume
    {
        Discard,//< No source volume is applied
        MapChannels, //< Apply volume per source's channel
        Mixdown, //< Average volume from all channels in the source, numOutChannels should be 1
    };

private:
    std::vector<std::unique_ptr<DownmixSource> > mDownmixSources;
    // Resample into these buffers, or produce directly when not resampling
    AudioGraph::Buffers mFloatBuffers;
    size_t mNumChannels;
    ApplyVolume mApplyVolume;

public:

    DownmixStage(std::vector<std::unique_ptr<DownmixSource> > downmixSources, size_t numChannels, size_t bufferSize, ApplyVolume applyGain);

    ~DownmixStage() override;

    bool AcceptsBuffers(const Buffers& buffers) const override;

    bool AcceptsBlockSize(size_t blockSize) const override;

    std::optional<size_t> Acquire(Buffers& data, size_t maxToProcess) override;
    sampleCount Remaining() const override;

    bool Release() override;
};
