/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <functional>
#include <memory>
#include <thread>
#include <vector>

#include "audio/iaudiometer.h"

#include "au3-recording/CaptureSink.h"

#include "captureinput.h"

class AudacityProject;
class AudioIOListener;

namespace au::au4audio {
//! Records the input of a stream into its capture sequences, as AudioIO does: with a CaptureSink, filled by the device
//! and drained into the sequences by a worker thread
class RecordingCapture
{
public:
    struct Settings {
        RecordableSequences sequences;
        //! Where the recording starts, after `leadInTime` of playback, and ends
        double startTime = 0.0;
        double endTime = 0.0;
        double leadInTime = 0.0;
        //! Usually negative: the input is late on the output by that much
        double latencyCompensation = 0.0;
        //! For punch and roll recording; taken
        std::vector<std::vector<float> >* crossfadeData = nullptr;
        bool detectDropouts = true;
        int numInputChannels = 0;
        double deviceRate = 0.0;
        //! The most frames processed at once
        int maxFrames = 0;
    };

    //! @param stopTransport called on the main thread if recording fails, e.g. the disk is full
    RecordingCapture(AudacityProject& project, Settings settings, std::shared_ptr<AudioIOListener> listener, audio::IAudioMeterPtr meter,
                     const std::atomic<float>& inputVolume, bool playThrough, std::function<void()> stopTransport);
    ~RecordingCapture();

    //! Main thread, before the device starts. False if the buffers can't be allocated.
    bool start();
    //! Main thread, once the device stopped: records what remains, and finishes the recording
    void finish();

    //! Whether the stream is past the lead-in, at `time` in the timeline
    bool isCapturing(double time) const { return time >= m_startTime; }
    int numInputChannels() const { return m_input.numChannels(); }

    //! Device thread: `frames` frames of input, one buffer per channel, captured unless `paused`. The input is metered
    //! and, if monitored, played through to `output` from `offset` on.
    void process(const float* const* input, float* const* output, int numOutputChannels, int offset, int frames, double sequenceTime,
                 audio::TimePoint time, bool paused);

private:
    void drain(bool force, bool streamActive);

    AudacityProject& m_project;
    const double m_startTime;
    const double m_deviceRate;
    const std::shared_ptr<AudioIOListener> m_listener;
    const audio::IAudioMeterPtr m_meter;
    const std::atomic<float>& m_inputVolume;
    const bool m_playThrough;
    const std::function<void()> m_stopTransport;

    CaptureSink m_sink;
    CaptureInput m_input;
    std::vector<float> m_scratch;
    std::vector<const float*> m_inputPointers;

    std::thread m_worker;
    std::atomic<bool> m_stopping { false };
    bool m_finished = false;
};
}
