/*
* Audacity: A Digital Audio Editor
*/
#include "safeiocallback.h"

#include "framework/global/types/string.h"

#include "au3-clip-processing/sequence-graph/LockFreeQueueNode.h"

using namespace au::au4juce;

SafeIoCallback::~SafeIoCallback()
{
    stop();
}

void SafeIoCallback::onBuilt(std::shared_ptr<ISequenceProcessingNode> top)
{
    // The device waits for the worker: after a seek, it resumes where the device is
    m_queue = std::make_unique<LockFreeQueueNode>(std::move(top), numChannels, readAheadChunkSize(), readAheadNumChunks, 0);
}

void SafeIoCallback::startReadingAhead()
{
    m_queue->Start();
}

void SafeIoCallback::invalidateReadAhead()
{
    m_queue->Invalidate();
}

int64_t SafeIoCallback::readAheadEnd() const
{
    // The sequences are read further ahead by their effects' latency
    return m_queue->ReadEnd().as_long_long() + m_maxLatency;
}

size_t SafeIoCallback::render(float* const* stereo, int64_t index, size_t frames)
{
    m_queue->Prepare(index);
    // As AudioIO: what the worker hasn't rendered yet is played late, after silence
    const auto available = m_queue->Available(index, frames);
    m_unplayedFrames += static_cast<int64_t>(frames - available);
    m_queue->GetSamples(stereo, index, available);
    return available;
}

void SafeIoCallback::releaseRendering()
{
    // Stops the worker, before what it renders goes
    m_queue.reset();
}

std::string SafeIoCallback::dropoutDetails() const
{
    return ", " + muse::String::number(1000.0 * m_unplayedFrames / m_sampleRate, 0).toStdString()
           + " ms of silence waiting for rendering";
}
