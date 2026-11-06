/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-mixer/ISequenceProcessingNode.h"

#include "IPlayheadMapper.h"

#include <memory>
#include <vector>

/**
 * @brief Its input as the playhead reads it: read by stream index, where an IPlayheadMapper says
 *
 * @details Where the playhead jumps, e.g. where the stream loops, a read is split into segments, each read from where the
 * mapper says.
 */
class PlayheadMappingNode final : public ISequenceProcessingNode
{
public:
    PlayheadMappingNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const IPlayheadMapper> mapper,
                        size_t numChannels);

    //! The samples of the stream from `start`
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::shared_ptr<ISequenceProcessingNode> mInput;
    const std::shared_ptr<const IPlayheadMapper> mMapper;
    std::vector<float*> mBuffers;
};
