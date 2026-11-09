/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <memory>
#include <vector>

#include "audio/iaudiometer.h"

#include "captureinput.h"
#include "audio/idevice.h"

class AudacityProject;

namespace au::au4audio {
//! Monitors the input of a device: meters it, and plays it through if asked
class MonitoringCallback final : public audio::IDeviceCallback
{
public:
    //! @param device opened, with input channels, and output channels for `playThrough`
    MonitoringCallback(AudacityProject& project, std::unique_ptr<audio::IDevice> device, bool playThrough,
                       audio::IAudioMeterPtr meter, const std::atomic<float>& inputVolume);
    ~MonitoringCallback() override;

    AudacityProject& project;
    const bool playThrough;

    double sampleRate() const { return m_sampleRate; }

    void start();
    void stop();

    // IDeviceCallback
    void process(const float* const* input, int numInputChannels, float* const* output, int numOutputChannels, int frames) override;

private:
    std::unique_ptr<audio::IDevice> m_device;
    const double m_sampleRate;
    const audio::IAudioMeterPtr m_meter;
    const std::atomic<float>& m_inputVolume;
    CaptureInput m_input;
    std::vector<const float*> m_inputPointers;
};
}
