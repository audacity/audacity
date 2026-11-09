/*
* Audacity: A Digital Audio Editor
*/
#include "endoftracknode.h"

#include <algorithm>

#include "au3-clip-processing/sequence-graph/IPlayheadMapper.h"

#include "iendoftrackcontext.h"
#include "stereo.h"

using namespace au::au4audio;

EndOfTrackNode::EndOfTrackNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const IPlayheadMapper> mapper,
                               const IEndOfTrackContext& context, std::optional<int64_t> stop)
    : m_input{std::move(input)}, m_mapper{std::move(mapper)}, m_context{context}, m_stop{stop}
{
}

void EndOfTrackNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    m_input->GetSamples(buffers, start, len);

    auto end = m_context.playEnd();
    if (m_stop) {
        end = std::min(end.value_or(*m_stop), *m_stop);
    }
    if (!end) {
        return;
    }
    // Segment by segment of what the playhead reads: it may loop
    for (size_t done = 0; done < len;) {
        const auto segment = m_mapper->Map(start + done, len - done);
        const auto position = segment.position.as_long_long();
        const auto audible = static_cast<size_t>(std::clamp<int64_t>(*end - position, 0, static_cast<int64_t>(segment.len)));
        for (int ch = 0; ch < numChannels; ++ch) {
            std::fill(buffers[ch] + done + audible, buffers[ch] + done + segment.len, 0.f);
        }
        done += std::max<size_t>(segment.len, 1);
    }
}
