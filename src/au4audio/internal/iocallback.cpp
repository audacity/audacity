/*
* Audacity: A Digital Audio Editor
*/
#include "iocallback.h"

#include <algorithm>
#include <cmath>

#include "framework/global/containers.h"
#include "framework/global/log.h"
#include "framework/global/types/string.h"

#include "au3-bus-track/BusTrack.h"
#include "au3-clip-processing/sequence-graph/LockFreeQueueNode.h"
#include "au3-project/Project.h"

#include "blockcontext.h"
#include "effectsnode.h"
#include "gainpannode.h"
#include "metersender.h"
#include "metertapnode.h"
#include "monitoredinput.h"
#include "mutesolonode.h"
#include "recordingcapture.h"
#include "sequentialmixnode.h"

using namespace au::au4audio;

namespace {
//! However short the setting, as much as the device may ask for at once, and the chunk being rendered: the device waits for
//! whole blocks. Its blocks may come in a burst, as much as it buffers, e.g. when it also captures: its latency.
size_t readAheadNumChunks(const au::audio::IDevice& device, size_t settingNumChunks, size_t chunkSize)
{
    const auto buffered = std::ceil(std::max(device.inputLatency(), device.outputLatency()) * device.sampleRate());
    const auto atOnce = std::max(static_cast<size_t>(device.blockSize()), static_cast<size_t>(buffered));
    return std::max(settingNumChunks, (atOnce + chunkSize - 1) / chunkSize + 1);
}

using Busses = std::vector<std::shared_ptr<const BusTrack> >;
//! By the indices of busses: none for the master
using BusOutputs = std::vector<std::optional<size_t> >;

std::optional<size_t> busIndex(const Busses& busses, const BusTrack* bus)
{
    const auto it = std::find_if(busses.begin(), busses.end(), [bus](const auto& other) { return other.get() == bus; });
    return it == busses.end() ? std::nullopt : std::make_optional(static_cast<size_t>(it - busses.begin()));
}

//! Where the busses go. A loop, which the UI doesn't allow, goes to the master.
BusOutputs busOutputs(const Busses& busses)
{
    BusOutputs outputs;
    for (const auto& bus : busses) {
        outputs.push_back(busIndex(busses, TrackOutput::Get(*bus).FindBus()));
    }
    for (size_t b = 0; b < busses.size(); ++b) {
        std::vector<bool> visited(busses.size(), false);
        visited[b] = true;
        for (auto next = outputs[b]; next; next = outputs[*next]) {
            if (visited[*next]) {
                outputs[b].reset();
                break;
            }
            visited[*next] = true;
        }
    }
    return outputs;
}

//! `bus` and the busses after it, until the master
std::vector<size_t> pathFrom(std::optional<size_t> bus, const BusOutputs& outputs)
{
    std::vector<size_t> path;
    for (auto next = bus; next; next = outputs[*next]) {
        path.push_back(*next);
    }
    return path;
}

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
    m_readAheadNumChunks{readAheadNumChunks(*device,
                                            static_cast<size_t>(std::max(1L, std::lround(readAheadDuration / readAheadChunkDuration))),
                                            m_readAheadChunkSize)},
    m_callbackInfos{callbackInfos}, m_playedRanges{playedRanges}, m_playbackVolume{playbackVolume}, m_device{std::move(device)}
{
}

IoCallback::~IoCallback()
{
    stop();
}

void IoCallback::build(const TransportSequences& sequences, const audio::IAudioMeterPtr& meter,
                       std::optional<RecordingRange> recording)
{
    m_meter = meter;
    const auto& playbackSequences = sequences.playbackSequences;
    const auto maxFrames = static_cast<size_t>(m_blockSize);
    // At the rate of the stream, not of the sequences
    m_effectsInitialization.emplace(m_wProject, m_sampleRate, numChannels, m_blockSize);
    for (const auto& sequence : playbackSequences) {
        if (const auto group = sequence->FindChannelGroup()) {
            m_effectsInitialization->AddGroup(*group, numChannels, m_sampleRate, m_blockSize);
        }
    }
    // The busses, which mix the tracks and the busses that go to them, instead of the master: in the device callback
    Busses busses;
    for (const auto bus : ::TrackList::Get(project).Any<const BusTrack>()) {
        busses.push_back(bus->SharedPointer<const BusTrack>());
        m_effectsInitialization->AddGroup(*bus, numChannels, m_sampleRate, m_blockSize);
    }
    const auto outputsOfBusses = busOutputs(busses);
    // The bus a sequence goes to
    const auto busOf = [&](const PlayableSequence* sequence) -> std::optional<size_t> {
        const auto track = sequence ? dynamic_cast<const ::Track*>(sequence->FindChannelGroup()) : nullptr;
        return track ? busIndex(busses, TrackOutput::Get(*track).FindBus()) : std::nullopt;
    };
    // The monitored track, if any, whether it plays or not
    const auto& monitoredTrack = m_monitorSettings ? m_monitorSettings->track : nullptr;
    const auto monitoredGroup = monitoredTrack ? monitoredTrack->FindChannelGroup() : nullptr;
    const bool monitoredTrackPlays = monitoredGroup
                                     && std::any_of(playbackSequences.begin(), playbackSequences.end(), [&](const auto& sequence) {
        return sequence->FindChannelGroup() == monitoredGroup;
    });
    if (monitoredGroup && !monitoredTrackPlays) {
        m_effectsInitialization->AddGroup(*monitoredGroup, numChannels, m_sampleRate, m_blockSize);
    }
    if (m_monitorSettings) {
        m_monitoredInput = std::make_shared<MonitoredInput>(m_monitorSettings->numInputChannels, maxFrames,
                                                            *m_monitorSettings->inputVolume);
    }
    // The device callback processes the effects of the master, and of the track the input is monitored through; the
    // workers those of the other tracks, each in a scope of its own
    std::vector<const ChannelGroup*> callbackGroups { RealtimeEffectManager::MasterGroup };
    if (monitoredGroup) {
        callbackGroups.push_back(monitoredGroup);
    }
    for (const auto& bus : busses) {
        callbackGroups.push_back(bus.get());
    }
    const auto makeBlockContext = [&](std::vector<const ChannelGroup*> groups) {
        return std::make_unique<BlockContext>(m_wProject, *m_effectsInitialization, playbackSequences, *m_mapper, m_endPosition,
                                              m_sampleRate, std::move(groups), busses);
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
    // The master reads the tracks ahead by its effects' latency, and as long again where reading jumps
    settings.meterCapacity = maxFrames + 2 * static_cast<size_t>(std::max(0, masterLatency));
    settings.masterLatency = std::max(0, masterLatency);
    std::vector<int> busLatencies;
    for (const auto& bus : busses) {
        busLatencies.push_back(std::max(0, m_blockContext->latency(bus.get())));
    }
    // How far ahead what goes to `bus` is read: by the latency of the effects of the busses from it on, and of the master's
    const auto leadTo = [&](std::optional<size_t> bus) {
        int lead = settings.masterLatency;
        for (const auto b : pathFrom(bus, outputsOfBusses)) {
            lead += busLatencies[b];
        }
        return lead;
    };
    // The solos of the busses from `bus` on: soloing them plays what goes to `bus` too
    const auto solosFrom = [&](std::optional<size_t> bus) {
        std::vector<std::function<bool()> > solos;
        for (const auto b : pathFrom(bus, outputsOfBusses)) {
            solos.push_back([bus = busses[b]] { return bus->GetSolo(); });
        }
        return solos;
    };
    // The settings of a chain that goes to `bus`
    const auto routedSettings = [&](std::optional<size_t> bus) {
        auto routed = settings;
        routed.masterLatency = leadTo(bus);
        routed.meterCapacity = maxFrames + 2 * static_cast<size_t>(routed.masterLatency);
        routed.soloedWith = solosFrom(bus);
        return routed;
    };
    // What goes to the master, and to each bus, of the tracks; and the solos of the tracks before each bus
    std::vector<std::shared_ptr<ISequenceProcessingNode> > outputs;
    std::vector<std::vector<std::shared_ptr<ISequenceProcessingNode> > > busInputs(busses.size());
    std::vector<std::vector<std::function<bool()> > > solosBefore(busses.size());
    const auto route = [&](std::shared_ptr<ISequenceProcessingNode> output, std::optional<size_t> bus,
                           const std::shared_ptr<const PlayableSequence>& sequence) {
        if (!bus) {
            outputs.push_back(std::move(output));
            return;
        }
        busInputs[*bus].push_back(std::move(output));
        for (const auto b : pathFrom(bus, outputsOfBusses)) {
            if (sequence) {
                solosBefore[b].push_back([sequence] { return sequence->GetSolo(); });
            }
        }
    };
    int64_t maxLatency = settings.masterLatency;
    std::vector<int64_t> meterLeads;
    for (size_t i = 0; i < playbackSequences.size(); ++i) {
        const auto& sequence = playbackSequences[i];
        const auto bus = busOf(sequence.get());
        const auto routed = routedSettings(bus);
        meterLeads.push_back(routed.masterLatency);
        maxLatency = std::max<int64_t>(maxLatency, routed.masterLatency);
        if (const auto group = sequence->FindChannelGroup()) {
            maxLatency = std::max<int64_t>(maxLatency, routed.masterLatency + std::max(0, m_blockContext->latency(group)));
        }
        std::optional<int64_t> stop;
        if (recording) {
            stop = toSamples(recording->end);
            if (muse::contains(sequences.leadInTimeSequences, sequence)) {
                stop = std::min(*stop, toSamples(recording->start));
            }
        }
        auto trackSettings = routed;
        const auto group = sequence->FindChannelGroup();
        const bool monitored = monitoredTrackPlays && group == monitoredGroup;
        // That the input is monitored through reads only its sequence ahead: its effects render in the device callback
        trackSettings.readAhead = TrackReadAhead { m_readAheadChunkSize, m_readAheadNumChunks, !monitored };
        if (!monitored) {
            // Its worker processes its effects, in a scope of their own
            m_trackContexts.push_back(makeBlockContext(group ? std::vector<const ChannelGroup*> { group }
                                                       : std::vector<const ChannelGroup*> {}));
            trackSettings.chainContext = m_trackContexts.back().get();
            trackSettings.effects = trackSettings.chainContext;
            trackSettings.muteSolo = trackSettings.chainContext;
            trackSettings.endOfTrack = trackSettings.chainContext;
        }
        if (monitored) {
            trackSettings.monitoredInput = m_monitoredInput;
        }
        m_tracks.push_back(buildTrack(sequence, trackSettings, stop));
        route(m_tracks.back().output, bus, sequence);
    }
    if (m_monitoredInput && !monitoredTrackPlays) {
        // Not metered: the input meters show it
        const auto bus = busOf(monitoredTrack.get());
        auto monitorSettings = routedSettings(bus);
        monitorSettings.monitoredInput = m_monitoredInput;
        route(buildMonitoredInput(monitoredTrack, monitorSettings), bus, monitoredTrack);
    }
    // The solos of the busses before each bus
    for (size_t b = 0; b < busses.size(); ++b) {
        for (const auto after : pathFrom(outputsOfBusses[b], outputsOfBusses)) {
            solosBefore[after].push_back([bus = busses[b]] { return bus->GetSolo(); });
        }
    }
    m_maxLatency = maxLatency;

    // The busses, as tracks: their mixes, effects, mute/solo, volume/pan and meters; those before a bus first
    std::vector<MeterSender::Tap> busTaps;
    const std::function<std::shared_ptr<ISequenceProcessingNode>(size_t)> buildBus = [&](size_t b) {
        const auto& bus = busses[b];
        auto inputs = std::move(busInputs[b]);
        for (size_t before = 0; before < busses.size(); ++before) {
            if (outputsOfBusses[before] == b) {
                inputs.push_back(buildBus(before));
            }
        }
        std::shared_ptr<ISequenceProcessingNode> node = std::make_shared<SequentialMixNode>(std::move(inputs), maxFrames);
        node = std::make_shared<EffectsNode>(std::move(node), numChannels, bus.get(), *m_blockContext, maxFrames);
        // Soloing what goes to it, or what it goes to, plays it too
        auto soloedWith = std::move(solosBefore[b]);
        for (auto& solo : solosFrom(outputsOfBusses[b])) {
            soloedWith.push_back(std::move(solo));
        }
        node = std::make_shared<MuteSoloNode>(std::move(node),
                                              MuteSoloSwitches { [bus] { return bus->GetMute(); }, [bus] { return bus->GetSolo(); } },
                                              *m_blockContext, std::move(soloedWith));
        node = std::make_shared<GainPanNode>(std::move(node), numChannels, [bus](int channel) { return bus->GetChannelVolume(channel); });
        const auto lead = leadTo(outputsOfBusses[b]);
        auto tap = std::make_shared<MeterTapNode>(std::move(node), numChannels, maxFrames + 2 * static_cast<size_t>(lead));
        busTaps.push_back({ tap, audio::IAudioMeter::TrackId { static_cast<int64_t>(bus->GetId()) }, lead });
        return tap;
    };
    for (size_t b = 0; b < busses.size(); ++b) {
        if (!outputsOfBusses[b]) {
            outputs.push_back(buildBus(b));
        }
    }

    std::shared_ptr<ISequenceProcessingNode> master = std::make_shared<SequentialMixNode>(std::move(outputs), maxFrames);
    master = std::make_shared<EffectsNode>(std::move(master), numChannels, RealtimeEffectManager::MasterGroup, *m_blockContext,
                                           maxFrames);
    m_masterTap = std::make_shared<MeterTapNode>(std::move(master), numChannels, maxFrames);
    m_top = std::make_shared<BlockContextNode>(m_masterTap, *m_blockContext);
    if (m_meter) {
        // The main meter, then the tracks'
        std::vector<MeterSender::Tap> taps { { m_masterTap, std::nullopt, 0 } };
        for (size_t i = 0; i < m_tracks.size(); ++i) {
            const auto& track = m_tracks[i];
            std::optional<audio::IAudioMeter::TrackId> trackId;
            if (track.meterId) {
                trackId = audio::IAudioMeter::TrackId { *track.meterId };
            }
            // Heard after the effects of the master, and of its busses
            taps.push_back({ track.tap, trackId, meterLeads[i] });
        }
        taps.insert(taps.end(), busTaps.begin(), busTaps.end());
        m_meterSender = std::make_unique<MeterSender>(std::move(taps));
    }

    for (int ch = 0; ch < numChannels; ++ch) {
        m_buffer[ch].assign(maxFrames, 0.f);
        m_bufferPointers[ch] = m_buffer[ch].data();
    }
}

void IoCallback::monitorThrough(std::shared_ptr<const PlayableSequence> track, int numInputChannels,
                                const std::atomic<float>& inputVolume)
{
    m_monitorSettings = MonitorSettings { std::move(track), numInputChannels, &inputVolume };
}

void IoCallback::setCapture(std::unique_ptr<RecordingCapture> capture)
{
    m_capture = std::move(capture);
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
    if (m_meter) {
        m_meter->start(m_sampleRate);
    }
    m_device->start(*this);
}

void IoCallback::stop()
{
    if (m_device) {
        m_device->stop();
        // Underruns of the output, and overflows of the input if one is open
        const int xruns = m_device->xrunCount();
        LOGI() << "Playback dropouts: " << muse::String::number(m_dropouts.frames / m_sampleRate, 1) << " s played, "
               << xruns << " device underruns" << (m_capture ? " or input overflows" : "") << ", " << m_dropouts.lateCallbacks
               << " late callbacks (longest " << muse::String::number(1000.0 * m_dropouts.longestCallback, 1) << " ms for "
               << muse::String::number(1000.0 * m_dropouts.longestCallbackBlock, 1) << " ms of audio), "
               << muse::String::number(1000.0 * m_dropouts.unplayedFrames / m_sampleRate, 0) << " ms of silence waiting for rendering";
        m_device.reset();
    }
    // Once the device stopped, so that what it captured last is recorded
    if (m_capture) {
        m_capture->finish();
    }
    if (m_meter) {
        m_meter->stop();
    }
    // Stops the workers, before what they render goes
    m_top.reset();
    m_meterSender.reset();
    m_monitoredInput.reset();
    m_masterTap.reset();
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

bool IoCallback::isFinished() const
{
    return m_finished && std::chrono::steady_clock::now() >= m_endHeard.load();
}

void IoCallback::process(const float* const* inputChannelData, int numInputChannels, float* const* outputChannelData,
                         int numOutputChannels, int numSamples)
{
    for (int ch = 0; ch < numOutputChannels; ++ch) {
        if (outputChannelData[ch]) {
            std::fill_n(outputChannelData[ch], numSamples, 0.f);
        }
    }
    if (m_finished || !m_top) {
        return;
    }
    // As AudioIO, a paused recording still meters the input
    if (m_paused && !m_capture) {
        return;
    }
    // The input of the channels the capture records, if the device gives them
    const auto input = m_capture && numInputChannels >= m_capture->numInputChannels() ? inputChannelData : nullptr;
    // As AudioIO, a sound activated recording is paused while the input is quiet
    const bool paused = m_paused || (input && m_capture->waitsForSound(input, numSamples, position()));
    // As the capture, the monitored input needs the device to give its channels
    const auto monitored = m_monitoredInput && numInputChannels >= m_monitoredInput->numInputChannels() ? inputChannelData : nullptr;

    const auto callbackStart = std::chrono::steady_clock::now();
    const auto dacTime = callbackStart
                         + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
        std::chrono::duration<double>(m_device->outputLatency()));
    int done = 0;
    int unplayed = 0;
    while (done < numSamples && !m_finished) {
        const int frames = std::min(numSamples - done, m_blockSize);
        const auto chunkDacTime = dacTime + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
            std::chrono::duration<double>(done / m_sampleRate));
        int chunkUnplayed = 0;
        if (!paused) {
            if (m_monitoredInput) {
                m_monitoredInput->read(monitored, done, frames, m_mapper->deviceIndex());
            }
            chunkUnplayed = renderChunk(outputChannelData, numOutputChannels, done, frames, chunkDacTime);
        }
        unplayed += chunkUnplayed;
        if (input) {
            m_capture->process(input, done, frames, chunkUnplayed, chunkDacTime, paused);
        }
        done += frames;
    }
    if (m_finished) {
        // After what the device buffered
        m_endHeard = dacTime + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
            std::chrono::duration<double>(numSamples / m_sampleRate));
    }
    if (paused) {
        return;
    }
    // As AudioIO, only what played advances the transport
    if (numSamples > unplayed) {
        m_callbackInfos.Put({ dacTime, numSamples - unplayed });
    }
    const double duration = std::chrono::duration<double>(std::chrono::steady_clock::now() - callbackStart).count();
    const double block = numSamples / m_sampleRate;
    m_dropouts.frames += numSamples;
    if (duration > block) {
        ++m_dropouts.lateCallbacks;
    }
    if (duration > m_dropouts.longestCallback) {
        m_dropouts.longestCallback = duration;
        m_dropouts.longestCallbackBlock = block;
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
    m_dropouts.unplayedFrames += static_cast<int64_t>(frames - available);
    if (available > 0) {
        m_top->GetSamples(stereo, index, available);
        // What was played is released at once, so that the workers have until the next block to render it
        for (const auto& track : m_tracks) {
            track.queue->Prepare(index + static_cast<int64_t>(available));
        }
    }
    return available;
}

int IoCallback::renderChunk(float* const* output, int numOutputChannels, int offset, int requested, audio::TimePoint dacTime)
{
    const auto index = m_mapper->deviceIndex();
    const auto frames = static_cast<int>(render(m_bufferPointers, index, static_cast<size_t>(requested)));
    if (m_meterSender && frames > 0) {
        m_meterSender->send(*m_meter, index, static_cast<size_t>(frames), dacTime);
    }

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
