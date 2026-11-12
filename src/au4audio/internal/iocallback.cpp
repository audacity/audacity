/*
* Audacity: A Digital Audio Editor
*/
#include "iocallback.h"

#include <algorithm>
#include <cmath>

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "au3-project/Project.h"

#include "blockcontext.h"
#include "effectsnode.h"
#include "sequentialmixnode.h"

using namespace au::au4audio;

IoCallback::IoCallback(AudacityProject& project, int token, std::unique_ptr<audio::IDevice> device,
                       bool followsPlayRegion, LockFreeQueue<audio::AudioCallbackInfo>& callbackInfos,
                       LockFreeQueue<PlayedRange>& playedRanges, const std::atomic<float>& playbackVolume)
    : project{project}, token{token}, followsPlayRegion{followsPlayRegion}, m_sampleRate{device->sampleRate()},
    m_wProject{project.shared_from_this()}, m_blockSize{device->blockSize()},
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
    m_blockContext = std::make_unique<BlockContext>(m_wProject, *m_effectsInitialization, playbackSequences, *m_mapper,
                                                    m_endPosition, m_sampleRate);
    TrackSettings settings;
    settings.mapper = m_mapper;
    settings.effects = m_blockContext.get();
    settings.muteSolo = m_blockContext.get();
    settings.endOfTrack = m_blockContext.get();
    settings.sampleRate = m_sampleRate;
    settings.maxFrames = maxFrames;
    std::vector<std::shared_ptr<ISequenceProcessingNode> > outputs;
    for (const auto& sequence : playbackSequences) {
        m_tracks.push_back(buildTrack(sequence, settings));
        outputs.push_back(m_tracks.back().output);
    }

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
}

void IoCallback::seek(double time)
{
    m_mapper->setPosition(toSamples(time));
}

void IoCallback::updateLoop(std::optional<std::pair<double, double> > loopRange, double endTime)
{
    if (loopRange) {
        m_mapper->setLoop(toLoopRange(*loopRange));
    } else {
        m_endPosition = toSamples(endTime);
        m_mapper->setLoop(std::nullopt);
    }
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
    m_top.reset();
    m_tracks.clear();
    m_blockContext.reset();
    // After the device stopped, so that nothing renders with them
    m_effectsInitialization.reset();
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
    // For now, in the device callback: all of it
    m_top->GetSamples(stereo, index, frames);
    return frames;
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
