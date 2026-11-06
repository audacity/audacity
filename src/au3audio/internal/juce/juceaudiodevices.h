/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>
#include <memory>
#include <utility>

#include "../iaudiodevices.h"

namespace juce {
class AudioIODeviceType;
}

namespace au::au4juce {
//! The audio devices as seen by JUCE: an API is a juce::AudioIODeviceType.
class JuceAudioDevices final : public au3audio::IAudioDevices
{
public:
    JuceAudioDevices();
    ~JuceAudioDevices() override;

    std::vector<std::string> apis() const override;
    std::string preferredApi(const std::vector<std::string>& apis) const override;

    std::vector<std::string> outputDevices(const std::string& api) const override;
    std::vector<std::string> inputDevices(const std::string& api) const override;
    std::string systemDefaultOutputDevice(const std::string& api) const override;
    std::string systemDefaultInputDevice(const std::string& api) const override;
    int inputChannels(const std::string& api, const std::string& inputDevice) const override;

    std::vector<uint64_t> sampleRates() const override;

    void rescan() override;
    void refreshInputDevice(const std::string& api, const std::string& inputDevice) override;

    bool hasAsioControlPanel(const std::string& api, const std::string& outputDevice,
                             const std::string& inputDevice) const override;
    void showAsioControlPanel(const std::string& api, const std::string& outputDevice,
                              const std::string& inputDevice) override;

    //! nullptr if there is no such API
    juce::AudioIODeviceType* deviceType(const std::string& api) const;

private:
    std::vector<std::unique_ptr<juce::AudioIODeviceType> > m_types;
    //! Knowing a device's channels takes creating it: done on demand and remembered until the next rescan
    mutable std::map<std::pair<std::string, std::string>, int> m_inputChannels;
};
}
