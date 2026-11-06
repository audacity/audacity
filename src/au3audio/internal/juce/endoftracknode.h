/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstdint>
#include <memory>
#include <optional>

#include "au3-mixer/ISequenceProcessingNode.h"

class IPlayheadMapper;

namespace au::au4juce {
class IEndOfTrackContext;

//! Its input, silent where its track ends: past the end of the play range, unless looping, or past where the track
//! stops, if it does
/*!
 * Before the track's effects, so that their tails ring out, as AudioIO's mixers do. As AudioIO: while recording, the
 * stream runs on past the play range's end, but the tracks stop playing there; a track recorded into with a lead-in stops
 * where the recording starts. In place, in stereo.
 */
class EndOfTrackNode final : public ISequenceProcessingNode
{
public:
    //! @param mapper where the playhead reads, by stream index
    //! @param stop where the track stops, in the timeline, in samples, if it does
    EndOfTrackNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const IPlayheadMapper> mapper,
                   const IEndOfTrackContext& context, std::optional<int64_t> stop = std::nullopt);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const std::shared_ptr<const IPlayheadMapper> m_mapper;
    const IEndOfTrackContext& m_context;
    const std::optional<int64_t> m_stop;
};
}
