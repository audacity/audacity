/*
* Audacity: A Digital Audio Editor
*/
#include "fastiocallback.h"

#include <algorithm>

#include "au3-clip-processing/sequence-graph/LockFreeQueueNode.h"

#ifdef AU_USE_TRACKTION
#include "../tracktion/tracktiontoaudacitynode.h"
#endif

using namespace au::au4juce;

FastIoCallback::~FastIoCallback()
{
    stop();
}

std::optional<TrackReadAhead> FastIoCallback::trackReadAhead() const
{
    const auto chunkSize = readAheadChunkSize();
    return TrackReadAhead { chunkSize, readAheadNumChunks, maxResyncLeadNumChunks * chunkSize };
}

#ifdef AU_USE_TRACKTION
std::shared_ptr<ISequenceProcessingNode> FastIoCallback::makeMix(std::vector<std::shared_ptr<ISequenceProcessingNode> > tracks,
                                                                 size_t maxFrames)
{
    return std::make_shared<au4tracktion::TracktionToAudacityNode>(std::move(tracks), m_sampleRate, maxFrames);
}
#endif

void FastIoCallback::startReadingAhead()
{
    for (const auto& track : m_tracks) {
        track.queue->Start();
    }
}

void FastIoCallback::invalidateReadAhead()
{
    for (const auto& track : m_tracks) {
        track.queue->Invalidate();
    }
}

int64_t FastIoCallback::readAheadEnd() const
{
    int64_t end = 0;
    for (const auto& track : m_tracks) {
        end = std::max<int64_t>(end, track.queue->ReadEnd().as_long_long());
    }
    return end;
}

size_t FastIoCallback::render(float* const* stereo, int64_t index, size_t frames)
{
    // In step with the device: what the tracks' workers haven't read in time is skipped, not played late
    m_top->GetSamples(stereo, index, frames);
    return frames;
}
