/*
* Audacity: A Digital Audio Editor
*/
#include "recordingcapture.h"

#include <chrono>
#include <new>

#include "au3-audio-io/AudioIOListener.h"

using namespace au::au4juce;

namespace {
//! How often the worker moves the captured input into the sequences
constexpr auto drainPeriod = std::chrono::milliseconds(10);
}

RecordingCapture::RecordingCapture(AudacityProject& project, Settings settings, std::shared_ptr<AudioIOListener> listener,
                                   audio::IAudioMeterPtr meter, const std::atomic<float>& inputVolume, bool playThrough,
                                   std::function<void()> stopTransport)
    : m_project{project}, m_startTime{settings.startTime}, m_deviceRate{settings.deviceRate}, m_listener{std::move(listener)},
    m_meter{std::move(meter)}, m_inputVolume{inputVolume}, m_playThrough{playThrough}, m_stopTransport{std::move(stopTransport)},
    m_input{settings.numInputChannels, settings.maxFrames}, m_scratch(static_cast<size_t>(settings.maxFrames)),
    m_inputPointers(static_cast<size_t>(settings.numInputChannels))
{
    // As AudioIO::StartStream
    m_sink.Reset(settings.sequences, settings.detectDropouts);
    auto& schedule = m_sink.Schedule();
    schedule = {};
    schedule.mLeadInTime = settings.leadInTime;
    schedule.mLatencyCompensation = settings.latencyCompensation;
    schedule.mDuration = settings.endTime - settings.startTime;
    if (settings.crossfadeData) {
        schedule.mCrossfadeData.swap(*settings.crossfadeData);
    }
    m_sink.ConfigureRouting(static_cast<size_t>(settings.numInputChannels));
}

RecordingCapture::~RecordingCapture()
{
    finish();
}

bool RecordingCapture::start()
{
    const auto numChannels = static_cast<size_t>(m_input.numChannels());
    const auto& sequences = m_sink.Sequences();
    if (sequences.empty() || numChannels == 0) {
        return false;
    }
    // The device gives floats; the sink converts them to the sequences' format and rate
    const auto sequenceRate = sequences.front()->GetRate();
    m_sink.InitBufferTimes(numChannels);
    // As AudioIO, with less memory if need be
    for (auto allocated = false; !allocated;) {
        try {
            if (!m_sink.AllocateBuffers(numChannels, floatSample, m_deviceRate, sequenceRate / m_deviceRate, sequenceRate)) {
                return false;
            }
            allocated = true;
        } catch (const std::bad_alloc&) {
            m_sink.HalveBufferTimes();
        }
    }

    if (m_meter) {
        m_meter->start(m_deviceRate);
    }
    m_listener->OnAudioIOStartRecording();
    m_worker = std::thread { [this] {
            while (!m_stopping.load(std::memory_order_relaxed)) {
                drain(false, true);
                std::this_thread::sleep_for(drainPeriod);
            }
        } };
    return true;
}

void RecordingCapture::finish()
{
    if (m_finished) {
        return;
    }
    m_finished = true;
    if (!m_worker.joinable()) {
        return;
    }
    m_stopping = true;
    m_worker.join();
    // As AudioIO::StopStream: what the device captured last, then the end of the recording
    drain(true, false);
    m_sink.Finish(&m_project, m_listener.get());
    m_listener->OnAudioIOStopRecording();
    if (m_meter) {
        m_meter->stop();
    }
    m_sink.Schedule().mCrossfadeData.clear();
    m_sink.Clear();
}

void RecordingCapture::drain(bool force, bool streamActive)
{
    m_sink.Drain(force, streamActive, m_listener.get(), m_stopTransport);
}

void RecordingCapture::process(const float* const* input, float* const* output, int numOutputChannels, int offset, int frames,
                               double sequenceTime, audio::TimePoint time, bool paused)
{
    for (int ch = 0; ch < m_input.numChannels(); ++ch) {
        m_inputPointers[ch] = input[ch] ? input[ch] + offset : nullptr;
    }
    const auto samples = m_input.read(m_inputPointers.data(), frames, m_inputVolume.load(std::memory_order_relaxed));
    // As AudioIO, metered and played through even when paused
    if (m_meter) {
        m_input.sendToMeter(*m_meter, m_sink.Sequences(), time);
    }
    if (m_playThrough) {
        m_input.playThrough(output, numOutputChannels, offset);
    }
    if (paused) {
        return;
    }
    constexpr auto inputError = false;
    constexpr auto simulateError = false;
    m_sink.Push(reinterpret_cast<constSamplePtr>(samples), static_cast<size_t>(m_input.numChannels()), static_cast<size_t>(frames),
                sequenceTime, inputError, simulateError, m_scratch.data());
}
