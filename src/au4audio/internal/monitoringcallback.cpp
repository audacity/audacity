/*
* Audacity: A Digital Audio Editor
*/
#include "monitoringcallback.h"

#include <algorithm>
#include <chrono>

#include "framework/global/log.h"

#include "au3-mixer/AudioIOSequences.h"
#include "au3-project/Project.h"

#include "blockcontext.h"
#include "effectsnode.h"
#include "monitoredinput.h"
#include "track.h"

using namespace au::au4audio;

MonitoringCallback::MonitoringCallback(AudacityProject& project, std::unique_ptr<audio::IDevice> device, bool playThrough,
                                       audio::IAudioMeterPtr meter, const std::atomic<float>& inputVolume,
                                       std::optional<Effects> effects)
    : project{project}, playThrough{playThrough}, effects{playThrough ? std::move(effects) : std::nullopt}, m_device{std::move(device)},
    m_blockSize{m_device->blockSize()}, m_sampleRate{m_device->sampleRate()},
    m_meter{std::move(meter)}, m_inputVolume{inputVolume}, m_input{m_device->numInputChannels(), m_blockSize},
    m_inputPointers(static_cast<size_t>(m_input.numChannels()))
{
    if (!this->effects) {
        return;
    }
    // As a stream initializes them, for the track and the master
    const auto wProject = project.shared_from_this();
    const auto maxFrames = static_cast<size_t>(m_blockSize);
    m_effectsInitialization.emplace(wProject, m_sampleRate, numChannels, m_blockSize);
    const auto& track = this->effects->track;
    if (const auto group = track ? track->FindChannelGroup() : nullptr) {
        m_effectsInitialization->AddGroup(*group, numChannels, m_sampleRate, m_blockSize);
    }
    m_mapper.start(0, std::nullopt);
    m_blockContext = std::make_unique<BlockContext>(wProject, *m_effectsInitialization, ConstPlayableSequences {}, m_mapper, m_playEnd,
                                                    m_sampleRate);
    m_monitoredInput = std::make_shared<MonitoredInput>(m_input.numChannels(), maxFrames, m_inputVolume);

    TrackSettings settings;
    settings.effects = m_blockContext.get();
    settings.muteSolo = m_blockContext.get();
    settings.endOfTrack = m_blockContext.get();
    settings.sampleRate = m_sampleRate;
    settings.maxFrames = maxFrames;
    settings.monitoredInput = m_monitoredInput;
    settings.masterLatency = std::max(0, m_blockContext->latency(RealtimeEffectManager::MasterGroup));
    auto chain = buildMonitoredInput(track, settings);
    chain = std::make_shared<EffectsNode>(std::move(chain), numChannels, RealtimeEffectManager::MasterGroup, *m_blockContext, maxFrames);
    m_chain = std::make_shared<BlockContextNode>(std::move(chain), *m_blockContext);
    for (auto& buffer : m_stereo) {
        buffer.assign(maxFrames, 0.f);
    }
}

RealtimeEffects::InitializationScope* MonitoringCallback::effectsInitialization()
{
    return m_effectsInitialization ? &*m_effectsInitialization : nullptr;
}

MonitoringCallback::~MonitoringCallback()
{
    stop();
}

void MonitoringCallback::start()
{
    if (m_meter) {
        m_meter->start(m_sampleRate);
    }
    m_device->start(*this);
}

void MonitoringCallback::stop()
{
    if (m_device) {
        m_device->stop();
        m_device.reset();
    }
    if (m_meter) {
        m_meter->stop();
    }
    // Once the device stopped
    m_chain.reset();
    m_monitoredInput.reset();
    m_blockContext.reset();
    m_effectsInitialization.reset();
}

void MonitoringCallback::process(const float* const* inputChannelData, int numInputChannels, float* const* outputChannelData,
                                 int numOutputChannels, int numSamples)
{
    for (int ch = 0; ch < numOutputChannels; ++ch) {
        if (outputChannelData[ch]) {
            std::fill_n(outputChannelData[ch], numSamples, 0.f);
        }
    }
    if (numInputChannels < m_input.numChannels()) {
        return;
    }

    // The device may call back with more frames than it said it would
    const auto now = std::chrono::steady_clock::now();
    for (int done = 0; done < numSamples;) {
        const auto frames = std::min(numSamples - done, m_blockSize);
        for (int ch = 0; ch < m_input.numChannels(); ++ch) {
            m_inputPointers[ch] = inputChannelData[ch] ? inputChannelData[ch] + done : nullptr;
        }
        m_input.read(m_inputPointers.data(), frames, m_inputVolume.load(std::memory_order_relaxed));
        if (m_meter) {
            m_input.sendToMeter(*m_meter, {}, now);
        }
        if (m_chain) {
            // Through the effects, rendered as the block from the next stream index
            m_monitoredInput->read(inputChannelData, done, frames, m_index);
            float* const stereo[numChannels] { m_stereo[0].data(), m_stereo[1].data() };
            m_chain->GetSamples(stereo, m_index, static_cast<size_t>(frames));
            m_index += frames;
            for (int ch = 0; ch < std::min(numOutputChannels, numChannels); ++ch) {
                if (!outputChannelData[ch]) {
                    continue;
                }
                for (int i = 0; i < frames; ++i) {
                    outputChannelData[ch][done + i] += stereo[ch][i];
                }
            }
        }
        done += frames;
    }
}
