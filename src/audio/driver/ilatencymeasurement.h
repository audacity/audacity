/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstddef>
#include <functional>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/modularity/ioc.h"

namespace au::audio {
struct LatencyMeasurement {
    enum class Status {
        Measured,
        Busy, //!< Playing, recording, or the audio settings are changing
        DeviceError,
        NoSignal, //!< Too few test signals came back to trust the result
    };

    Status status = Status::DeviceError;
    double sampleRate = 0.0;
    long roundTripFrames = -1;
    //! What automatic latency compensation would use
    double reportedInputLatencyMs = 0.0;
    double reportedOutputLatencyMs = 0.0;
    size_t signalsFound = 0;
    size_t signalsSent = 0;
    long spreadFrames = 0;
    float inputPeak = 0.0f;
    bool inverted = false;

    double roundTripMs() const { return sampleRate > 0 ? 1000.0 * roundTripFrames / sampleRate : 0.0; }
};

class ILatencyMeasurement : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(ILatencyMeasurement)

public:
    virtual ~ILatencyMeasurement() = default;

    virtual bool isMeasuring() const = 0;

    //! Plays short test signals on the playback device and records them back
    //! from the recording device, for a few seconds. Input monitoring is paused
    //! meanwhile. `done` runs on the main thread.
    virtual void measure(const muse::modularity::ContextPtr& requester, std::function<void(const LatencyMeasurement&)> done) = 0;
};
}
