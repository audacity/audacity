/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <memory>
#include <vector>

#include "audio/iaudiometer.h"

#include "captureinput.h"
#include "juceincludes.h"

class AudacityProject;

namespace au::au4juce {
//! Monitors the input of a device: meters it, and plays it through if asked
class JuceMonitoringCallback final : public juce::AudioIODeviceCallback
{
public:
    //! @param device opened, with input channels, and output channels for `playThrough`
    JuceMonitoringCallback(AudacityProject& project, std::unique_ptr<juce::AudioIODevice> device, bool playThrough,
                           audio::IAudioMeterPtr meter, const std::atomic<float>& inputVolume);
    ~JuceMonitoringCallback() override;

    AudacityProject& project;
    const bool playThrough;

    double sampleRate() const { return m_sampleRate; }

    void start();
    void stop();

    // juce::AudioIODeviceCallback
    void audioDeviceIOCallbackWithContext(const float* const* inputChannelData, int numInputChannels, float* const* outputChannelData,
                                          int numOutputChannels, int numSamples,
                                          const juce::AudioIODeviceCallbackContext& context) override;
    void audioDeviceAboutToStart(juce::AudioIODevice*) override {}
    void audioDeviceStopped() override {}
    void audioDeviceError(const juce::String& errorMessage) override;

private:
    std::unique_ptr<juce::AudioIODevice> m_device;
    const double m_sampleRate;
    const audio::IAudioMeterPtr m_meter;
    const std::atomic<float>& m_inputVolume;
    CaptureInput m_input;
    std::vector<const float*> m_inputPointers;
};
}
