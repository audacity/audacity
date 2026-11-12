/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "audio/driver/iaudiodevices.h"

namespace au::portaudiodevices {
//! The PortAudio devices, as seen by au3's DeviceManager.
class PortAudioDevices final : public audio::IAudioDevices
{
public:
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

private:
    int asioDeviceIndex(const std::string& api, const std::string& outputDevice, const std::string& inputDevice) const;
};
}
