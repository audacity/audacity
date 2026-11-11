/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ISequenceProcessingNode.h"

#include <memory>
#include <optional>
#include <vector>

/**
 * @brief Cross-fades when changes in its input between two `GetSamples` calls are detected.
 *
 * @details It reads `lookahead` samples ahead, and keeps them, as the input gave them and as they are to be output. At each
 * read, it reads the `lookahead` samples from where the read starts: if they differ from those it kept, it cross-fades
 * from what was to be output into them.
 *
 * Changes take effect from the next sample output, over `lookahead` samples. A read that doesn't continue the previous
 * one, e.g. on a seek or a loop, isn't cross-faded.
 *
 * The input must give the same samples when read again with the same processing, and be able to read again the
 * samples read ahead.
 */
class CrossFadingSmoother final : public ISequenceProcessingNode
{
public:
    CrossFadingSmoother(std::unique_ptr<ISequenceProcessingNode> input, size_t numChannels, size_t lookahead);

    //! 5ms. Tested with a 440Hz sine tone. About the least duration before the crossfade becomes unpleasant.
    static constexpr double smootherCrossfadeDuration = 0.005;

    // ISequenceProcessingNode
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    using ChannelBuffers = std::vector<std::vector<float> >;

    //! Pointers to `channelBuffers`, from `offset` on
    float* const* Pointers(ChannelBuffers& channelBuffers, size_t offset);

    const std::unique_ptr<ISequenceProcessingNode> mInput;
    const size_t mLookahead;
    const std::vector<float> mFadeIn;

    //! From the first sample read ahead: the samples as the input gave them ...
    ChannelBuffers mInputSamples;
    //! ... and as they are to be output
    ChannelBuffers mOutput;
    //! The samples read ahead, read again
    ChannelBuffers mReread;
    std::vector<float*> mPointers;

    //! Where the next read starts if it continues the previous one
    std::optional<sampleCount> mExpectedStart;
};
