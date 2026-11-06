/*
* Audacity: A Digital Audio Editor
*/
#include "safeiocallback.h"

#include <algorithm>
#include <cmath>

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "au3-clip-processing/sequence-graph/LockFreeQueueNode.h"
#include "au3-project/Project.h"

#include "blockcontext.h"
#include "effectsnode.h"
#include "metertapnode.h"
#include "sequentialmixnode.h"

using namespace au::au4juce;

namespace {
//! A margin on how far the worker may have rendered ahead when the loop changes: after a seek, it resumes from where
//! the device is
constexpr size_t maxResyncLeadNumChunks = 2;

using Segments = std::vector<std::pair<int64_t, int64_t> >;

//! The segments that start at `from`, cut at `end`
Segments cut(const Segments& segments, int64_t from, int64_t end)
{
    Segments result;
    for (const auto& [position, length] : segments) {
        if (from >= end) {
            break;
        }
        result.push_back({ position, std::min(length, end - from) });
        from += length;
    }
    return result;
}
}

SafeIoCallback::SafeIoCallback(AudacityProject& project, int token, std::unique_ptr<juce::AudioIODevice> device,
                               bool followsPlayRegion, LockFreeQueue<audio::AudioCallbackInfo>& callbackInfos,
                               LockFreeQueue<PlayedRange>& playedRanges, const std::atomic<float>& playbackVolume)
    : project{project}, token{token}, followsPlayRegion{followsPlayRegion}, m_wProject{project.shared_from_this()},
    m_sampleRate{device->getCurrentSampleRate()}, m_blockSize{std::max(1, device->getCurrentBufferSizeSamples())},
    m_callbackInfos{callbackInfos}, m_playedRanges{playedRanges}, m_playbackVolume{playbackVolume}, m_device{std::move(device)}
{
}

SafeIoCallback::~SafeIoCallback()
{
    stop();
}

void SafeIoCallback::build(const TransportSequences& sequences)
{
    const auto& playbackSequences = sequences.playbackSequences;
    const auto maxFrames = static_cast<size_t>(m_blockSize);
    // At the rate of the stream, not of the sequences
    m_effectsInitialization.emplace(m_wProject, m_sampleRate, numChannels, m_blockSize);
    for (const auto& sequence : playbackSequences) {
        if (const auto group = sequence->FindChannelGroup()) {
            m_effectsInitialization->AddGroup(*group, numChannels, m_sampleRate, m_blockSize);
        }
    }
    m_blockContext = std::make_unique<BlockContext>(m_wProject, *m_effectsInitialization, playbackSequences, *m_mapper,
                                                    m_endPosition, m_sampleRate);

    // Not compensated again if it changes during the stream
    const auto masterLatency = m_blockContext->latency(RealtimeEffectManager::MasterGroup);
    TrackSettings settings;
    settings.mapper = m_mapper;
    settings.effects = m_blockContext.get();
    settings.muteSolo = m_blockContext.get();
    settings.endOfTrack = m_blockContext.get();
    settings.sampleRate = m_sampleRate;
    settings.maxFrames = maxFrames;
    // The master reads the tracks ahead by its effects' latency, and as long again where reading jumps
    settings.meterCapacity = readAheadChunkSize() + 2 * static_cast<size_t>(std::max(0, masterLatency));
    int maxTrackLatency = 0;
    std::vector<std::shared_ptr<ISequenceProcessingNode> > outputs;
    for (size_t i = 0; i < playbackSequences.size(); ++i) {
        const auto& sequence = playbackSequences[i];
        if (const auto group = sequence->FindChannelGroup()) {
            maxTrackLatency = std::max(maxTrackLatency, m_blockContext->latency(group));
        }
        m_tracks.push_back(buildTrack(sequence, sequences.playbackGraphs[i], settings));
        outputs.push_back(m_tracks.back().output);
    }
    m_maxLatency = std::max(0, masterLatency) + maxTrackLatency;

    std::shared_ptr<ISequenceProcessingNode> master = std::make_shared<SequentialMixNode>(std::move(outputs), maxFrames);
    master = std::make_shared<EffectsNode>(std::move(master), numChannels, RealtimeEffectManager::MasterGroup, *m_blockContext,
                                           maxFrames);
    m_masterTap = std::make_shared<MeterTapNode>(std::move(master), numChannels, readAheadChunkSize());
    // The device waits for the worker: after a seek, it resumes where the device is
    m_queue = std::make_unique<LockFreeQueueNode>(std::make_shared<BlockContextNode>(m_masterTap, *m_blockContext), numChannels,
                                                  readAheadChunkSize(), readAheadNumChunks, 0);

    for (int ch = 0; ch < numChannels; ++ch) {
        m_buffer[ch].assign(maxFrames, 0.f);
        m_bufferPointers[ch] = m_buffer[ch].data();
    }
}

void SafeIoCallback::setPlayRange(double startTime, double endTime, std::optional<std::pair<double, double> > loopRange)
{
    m_endPosition = toSamples(endTime);
    std::optional<PlayheadMapper::Range> loop;
    if (loopRange) {
        // Unlike AudioIO, it doesn't loop ranges of 50 samples or fewer
        loop = toLoopRange(*loopRange);
    }
    m_mapper->start(toSamples(startTime), loop);
    m_queue->Start();
}

void SafeIoCallback::seek(double time)
{
    m_mapper->setPosition(toSamples(time));
    m_queue->Invalidate();
}

void SafeIoCallback::updateLoop(std::optional<std::pair<double, double> > loopRange, double endTime)
{
    // From where the device is, further than may have been rendered ahead
    const auto from = m_mapper->deviceIndex();
    const auto horizon = from + static_cast<int64_t>((readAheadNumChunks + maxResyncLeadNumChunks + 1) * readAheadChunkSize())
                         + m_maxLatency;
    const auto before = segments(from, horizon);
    if (loopRange) {
        m_mapper->setLoop(toLoopRange(*loopRange));
    } else {
        m_endPosition = toSamples(endTime);
        m_mapper->setLoop(std::nullopt);
    }
    // Only if what was rendered ahead changes. Loaded after the change, what was rendered before it is included. The
    // sequences are read further ahead by their effects' latency.
    const auto end = std::min<int64_t>(m_queue->ReadEnd().as_long_long() + m_maxLatency, horizon);
    if (cut(segments(from, horizon), from, end) != cut(before, from, end)) {
        m_queue->Invalidate();
    }
}

Segments SafeIoCallback::segments(int64_t from, int64_t end) const
{
    Segments segments;
    for (auto index = from; index < end;) {
        const auto segment = m_mapper->Map(index, static_cast<size_t>(end - index));
        segments.push_back({ segment.position.as_long_long(), static_cast<int64_t>(segment.len) });
        index += static_cast<int64_t>(std::max<size_t>(segment.len, 1));
    }
    return segments;
}

PlayheadMapper::Range SafeIoCallback::toLoopRange(const std::pair<double, double>& loopRange) const
{
    const auto start = toSamples(loopRange.first);
    return { start, std::max(start + 1, toSamples(loopRange.second)) };
}

void SafeIoCallback::start()
{
    m_device->start(this);
}

void SafeIoCallback::stop()
{
    if (m_device) {
        m_device->stop();
        m_device->close();
        m_device.reset();
    }
    // Stops the worker, before what it renders goes
    m_queue.reset();
    m_masterTap.reset();
    m_tracks.clear();
    m_blockContext.reset();
    // After the device and the worker stopped, so that nothing renders with them
    m_effectsInitialization.reset();
}

RealtimeEffects::InitializationScope* SafeIoCallback::effectsInitialization()
{
    return m_effectsInitialization ? &*m_effectsInitialization : nullptr;
}

void SafeIoCallback::audioDeviceIOCallbackWithContext(const float* const*, int, float* const* outputChannelData,
                                                      int numOutputChannels, int numSamples, const juce::AudioIODeviceCallbackContext&)
{
    for (int ch = 0; ch < numOutputChannels; ++ch) {
        if (outputChannelData[ch]) {
            std::fill_n(outputChannelData[ch], numSamples, 0.f);
        }
    }
    if (m_finished || !m_queue || m_paused) {
        return;
    }

    const auto dacTime = std::chrono::steady_clock::now()
                         + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
        std::chrono::duration<double>(m_device->getOutputLatencyInSamples() / m_sampleRate));
    int done = 0;
    int unplayed = 0;
    while (done < numSamples && !m_finished) {
        const int frames = std::min(numSamples - done, m_blockSize);
        unplayed += renderChunk(outputChannelData, numOutputChannels, done, frames);
        done += frames;
    }
    // As AudioIO, only what played advances the transport
    if (numSamples > unplayed) {
        m_callbackInfos.Put({ dacTime, numSamples - unplayed });
    }
}

int SafeIoCallback::renderChunk(float* const* output, int numOutputChannels, int offset, int requested)
{
    const auto index = m_mapper->deviceIndex();
    m_queue->Prepare(index);
    // As AudioIO: what the worker hasn't rendered yet is played late, after silence
    const auto frames = static_cast<int>(m_queue->Available(index, static_cast<size_t>(requested)));
    m_queue->GetSamples(m_bufferPointers, index, static_cast<size_t>(frames));

    // What plays now: unless looping, the end of the play range ends it
    bool finished = false;
    if (m_mapper->loop()) {
        for (int64_t done = 0; done < frames;) {
            const auto segment = m_mapper->Map(index + done, static_cast<size_t>(frames - done));
            reportPlayed(segment.position.as_long_long(), static_cast<int64_t>(segment.len));
            done += static_cast<int64_t>(segment.len);
        }
    } else {
        const auto start = m_mapper->linearPosition(index);
        const auto inRange = std::clamp<int64_t>(m_endPosition.load() - start, 0, frames);
        if (inRange > 0) {
            reportPlayed(start, inRange);
        }
        if (inRange < frames) {
            for (auto& buffer : m_buffer) {
                std::fill(buffer.begin() + inRange, buffer.begin() + frames, 0.f);
            }
            finished = true;
        }
    }

    const float* stereo[numChannels] { m_bufferPointers[0], m_bufferPointers[1] };
    writeOutput(stereo, output, numOutputChannels, offset, frames);
    m_mapper->advance(frames);
    if (finished) {
        m_finished = true;
    }
    return requested - frames;
}

void SafeIoCallback::writeOutput(const float* const* stereo, float* const* output, int numOutputChannels, int offset, int frames) const
{
    const float volume = m_playbackVolume.load(std::memory_order_relaxed);
    for (int ch = 0; ch < numOutputChannels; ++ch) {
        if (!output[ch]) {
            continue;
        }
        for (int i = 0; i < frames; ++i) {
            const float sample = numOutputChannels == 1
                                 ? 0.5f * (stereo[0][i] + stereo[1][i])
                                 : ch < numChannels ? stereo[ch][i] : 0.f;
            output[ch][offset + i] = sample * volume;
        }
    }
}

void SafeIoCallback::reportPlayed(int64_t start, int64_t frames)
{
    m_playedRanges.Put({ toTime(start), static_cast<unsigned long>(frames) });
}

void SafeIoCallback::audioDeviceError(const juce::String& errorMessage)
{
    LOGE() << "audio device error: " << errorMessage.toStdString();
}
