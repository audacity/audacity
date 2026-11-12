/*
* Audacity: A Digital Audio Editor
*/
#include "iocallback.h"

#include <algorithm>
#include <cmath>

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "au3-clip-processing/sequence-graph/LockFreeQueueNode.h"
#include "au3-project/Project.h"

#include "blockcontext.h"
#include "effectsnode.h"
#include "sequentialmixnode.h"

using namespace au::au4audio;

namespace {
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

IoCallback::IoCallback(AudacityProject& project, int token, std::unique_ptr<audio::IDevice> device,
                       bool followsPlayRegion, LockFreeQueue<audio::AudioCallbackInfo>& callbackInfos,
                       LockFreeQueue<PlayedRange>& playedRanges, const std::atomic<float>& playbackVolume,
                       double readAheadDuration)
    : project{project}, token{token}, followsPlayRegion{followsPlayRegion}, m_sampleRate{device->sampleRate()},
    m_wProject{project.shared_from_this()}, m_blockSize{device->blockSize()},
    m_readAheadChunkSize{static_cast<size_t>(std::max(1L, std::lround(readAheadChunkDuration * m_sampleRate)))},
    // However short the setting, a device block ahead, and the chunk being rendered: the device waits for whole blocks
    m_readAheadNumChunks{std::max(static_cast<size_t>(std::max(1L, std::lround(readAheadDuration / readAheadChunkDuration))),
                                  (static_cast<size_t>(m_blockSize) + m_readAheadChunkSize - 1) / m_readAheadChunkSize + 1)},
    m_callbackInfos{callbackInfos}, m_playedRanges{playedRanges}, m_playbackVolume{playbackVolume}, m_device{std::move(device)}
{
}

IoCallback::~IoCallback()
{
    stop();
}

void IoCallback::build(const TransportSequences& sequences)
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
    // The device callback processes the master's effects; the workers those of the tracks, each in a scope of its own
    std::vector<const ChannelGroup*> callbackGroups { RealtimeEffectManager::MasterGroup };
    const auto makeBlockContext = [&](std::vector<const ChannelGroup*> groups) {
        return std::make_unique<BlockContext>(m_wProject, *m_effectsInitialization, playbackSequences, *m_mapper, m_endPosition,
                                              m_sampleRate, std::move(groups));
    };
    m_blockContext = makeBlockContext(std::move(callbackGroups));

    // Not compensated again if it changes during the stream
    const auto masterLatency = m_blockContext->latency(RealtimeEffectManager::MasterGroup);
    TrackSettings settings;
    settings.mapper = m_mapper;
    settings.effects = m_blockContext.get();
    settings.muteSolo = m_blockContext.get();
    settings.endOfTrack = m_blockContext.get();
    settings.sampleRate = m_sampleRate;
    settings.maxFrames = maxFrames;
    settings.masterLatency = std::max(0, masterLatency);
    int maxTrackLatency = 0;
    std::vector<std::shared_ptr<ISequenceProcessingNode> > outputs;
    for (size_t i = 0; i < playbackSequences.size(); ++i) {
        const auto& sequence = playbackSequences[i];
        if (const auto group = sequence->FindChannelGroup()) {
            maxTrackLatency = std::max(maxTrackLatency, m_blockContext->latency(group));
        }
        auto trackSettings = settings;
        trackSettings.readAhead = TrackReadAhead { m_readAheadChunkSize, m_readAheadNumChunks };
        // Its worker processes its effects, in a scope of their own
        const auto group = sequence->FindChannelGroup();
        m_trackContexts.push_back(makeBlockContext(group ? std::vector<const ChannelGroup*> { group }
                                                   : std::vector<const ChannelGroup*> {}));
        trackSettings.chainContext = m_trackContexts.back().get();
        trackSettings.effects = trackSettings.chainContext;
        trackSettings.muteSolo = trackSettings.chainContext;
        trackSettings.endOfTrack = trackSettings.chainContext;
        m_tracks.push_back(buildTrack(sequence, trackSettings));
        outputs.push_back(m_tracks.back().output);
    }
    m_maxLatency = std::max(0, masterLatency) + maxTrackLatency;

    std::shared_ptr<ISequenceProcessingNode> master = std::make_shared<SequentialMixNode>(std::move(outputs), maxFrames);
    master = std::make_shared<EffectsNode>(std::move(master), numChannels, RealtimeEffectManager::MasterGroup, *m_blockContext,
                                           maxFrames);
    m_top = std::make_shared<BlockContextNode>(std::move(master), *m_blockContext);

    for (int ch = 0; ch < numChannels; ++ch) {
        m_buffer[ch].assign(maxFrames, 0.f);
        m_bufferPointers[ch] = m_buffer[ch].data();
    }
}

void IoCallback::setPlayRange(double startTime, double endTime, std::optional<std::pair<double, double> > loopRange)
{
    m_endPosition = toSamples(endTime);
    std::optional<PlayheadMapper::Range> loop;
    if (loopRange) {
        // Unlike AudioIO, it doesn't loop ranges of 50 samples or fewer
        loop = toLoopRange(*loopRange);
    }
    m_mapper->start(toSamples(startTime), loop);
    startReadingAhead();
}

void IoCallback::seek(double time)
{
    m_mapper->setPosition(toSamples(time));
    invalidateReadAhead();
}

void IoCallback::updateLoop(std::optional<std::pair<double, double> > loopRange, double endTime)
{
    // From where the device is, further than may have been rendered ahead
    const auto from = m_mapper->deviceIndex();
    const auto horizon = from + static_cast<int64_t>((m_readAheadNumChunks + 1) * m_readAheadChunkSize) + m_maxLatency;
    const auto before = segments(from, horizon);
    if (loopRange) {
        m_mapper->setLoop(toLoopRange(*loopRange));
    } else {
        m_endPosition = toSamples(endTime);
        m_mapper->setLoop(std::nullopt);
    }
    // Only if what was read ahead changes. Loaded after the change, what was read before it is included.
    const auto end = std::min<int64_t>(readAheadEnd(), horizon);
    if (cut(segments(from, horizon), from, end) != cut(before, from, end)) {
        invalidateReadAhead();
    }
}

Segments IoCallback::segments(int64_t from, int64_t end) const
{
    Segments segments;
    for (auto index = from; index < end;) {
        const auto segment = m_mapper->Map(index, static_cast<size_t>(end - index));
        segments.push_back({ segment.position.as_long_long(), static_cast<int64_t>(segment.len) });
        index += static_cast<int64_t>(std::max<size_t>(segment.len, 1));
    }
    return segments;
}

PlayheadMapper::Range IoCallback::toLoopRange(const std::pair<double, double>& loopRange) const
{
    const auto start = toSamples(loopRange.first);
    return { start, std::max(start + 1, toSamples(loopRange.second)) };
}

void IoCallback::start()
{
    m_device->start(*this);
}

void IoCallback::stop()
{
    if (m_device) {
        m_device->stop();
        m_device.reset();
    }
    // Stops the workers, before what they render goes
    m_top.reset();
    m_tracks.clear();
    m_trackContexts.clear();
    m_blockContext.reset();
    // After the device and the workers stopped, so that nothing renders with them
    m_effectsInitialization.reset();
}

void IoCallback::startReadingAhead()
{
    for (const auto& track : m_tracks) {
        track.queue->Start();
    }
}

void IoCallback::invalidateReadAhead()
{
    for (const auto& track : m_tracks) {
        track.queue->Invalidate();
    }
}

int64_t IoCallback::readAheadEnd() const
{
    // The sequences are read further ahead by their effects' latency
    int64_t end = 0;
    for (const auto& track : m_tracks) {
        end = std::max<int64_t>(end, track.queue->ReadEnd().as_long_long() + m_maxLatency);
    }
    return end;
}

RealtimeEffects::InitializationScope* IoCallback::effectsInitialization()
{
    return m_effectsInitialization ? &*m_effectsInitialization : nullptr;
}

void IoCallback::process(const float* const*, int, float* const* outputChannelData, int numOutputChannels, int numSamples)
{
    for (int ch = 0; ch < numOutputChannels; ++ch) {
        if (outputChannelData[ch]) {
            std::fill_n(outputChannelData[ch], numSamples, 0.f);
        }
    }
    if (m_finished || !m_top || m_paused) {
        return;
    }

    const auto dacTime = std::chrono::steady_clock::now()
                         + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
        std::chrono::duration<double>(m_device->outputLatency()));
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

size_t IoCallback::render(float* const* stereo, int64_t index, size_t frames)
{
    // As AudioIO: what the workers haven't all rendered yet is played late, after silence
    size_t available = frames;
    for (const auto& track : m_tracks) {
        track.queue->Prepare(index);
        available = std::min(available, track.queue->Available(index + track.queueLead, frames));
    }
    if (available > 0) {
        m_top->GetSamples(stereo, index, available);
        // What was played is released at once, so that the workers have until the next block to render it
        for (const auto& track : m_tracks) {
            track.queue->Prepare(index + static_cast<int64_t>(available));
        }
    }
    return available;
}

int IoCallback::renderChunk(float* const* output, int numOutputChannels, int offset, int requested)
{
    const auto index = m_mapper->deviceIndex();
    const auto frames = static_cast<int>(render(m_bufferPointers, index, static_cast<size_t>(requested)));

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

void IoCallback::writeOutput(const float* const* stereo, float* const* output, int numOutputChannels, int offset, int frames) const
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

void IoCallback::reportPlayed(int64_t start, int64_t frames)
{
    m_playedRanges.Put({ toTime(start), static_cast<unsigned long>(frames) });
}
