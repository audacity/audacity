/*
* Audacity: A Digital Audio Editor
*/
#include "juceaudiodevices.h"

#include <juce_audio_devices/juce_audio_devices.h>

#include "framework/global/containers.h"

#include "au3-audio-devices/DeviceManager.h"

using namespace au::jucedevices;

namespace {
std::vector<std::string> toStdStrings(const juce::StringArray& strings)
{
    std::vector<std::string> result;
    result.reserve(strings.size());
    for (const auto& string : strings) {
        result.push_back(string.toStdString());
    }
    return result;
}

std::string deviceName(juce::AudioIODeviceType& type, bool forInput)
{
    const auto names = type.getDeviceNames(forInput);
    const int index = type.getDefaultDeviceIndex(forInput);
    return index >= 0 && index < names.size() ? names[index].toStdString() : std::string();
}
}

JuceAudioDevices::JuceAudioDevices()
{
    // Those that the platform doesn't have come back null
    for (auto* const type : {
#if defined(_WIN32)
        juce::AudioIODeviceType::createAudioIODeviceType_WASAPI(juce::WASAPIDeviceMode::shared),
        juce::AudioIODeviceType::createAudioIODeviceType_WASAPI(juce::WASAPIDeviceMode::exclusive),
        juce::AudioIODeviceType::createAudioIODeviceType_DirectSound(),
        juce::AudioIODeviceType::createAudioIODeviceType_ASIO(),
#endif
        juce::AudioIODeviceType::createAudioIODeviceType_CoreAudio(),
        juce::AudioIODeviceType::createAudioIODeviceType_ALSA(),
        juce::AudioIODeviceType::createAudioIODeviceType_JACK(),
    }) {
        if (type) {
            m_types.emplace_back(type);
        }
    }
    rescan();
}

JuceAudioDevices::~JuceAudioDevices() = default;

juce::AudioIODeviceType* JuceAudioDevices::deviceType(const std::string& api) const
{
    for (const auto& type : m_types) {
        if (type->getTypeName().toStdString() == api) {
            return type.get();
        }
    }
    return nullptr;
}

std::vector<std::string> JuceAudioDevices::apis() const
{
    std::vector<std::string> result;
    for (const auto& type : m_types) {
        if (!type->getDeviceNames(false).isEmpty() || !type->getDeviceNames(true).isEmpty()) {
            result.push_back(type->getTypeName().toStdString());
        }
    }
    return result;
}

std::string JuceAudioDevices::preferredApi(const std::vector<std::string>& apis) const
{
    // Device types are listed in order of preference
    for (const auto& type : m_types) {
        const auto name = type->getTypeName().toStdString();
        if (muse::contains(apis, name)) {
            return name;
        }
    }
    return apis.empty() ? std::string() : apis.front();
}

std::vector<std::string> JuceAudioDevices::outputDevices(const std::string& api) const
{
    const auto type = deviceType(api);
    return type ? toStdStrings(type->getDeviceNames(false)) : std::vector<std::string> {};
}

std::vector<std::string> JuceAudioDevices::inputDevices(const std::string& api) const
{
    const auto type = deviceType(api);
    return type ? toStdStrings(type->getDeviceNames(true)) : std::vector<std::string> {};
}

std::string JuceAudioDevices::systemDefaultOutputDevice(const std::string& api) const
{
    const auto type = deviceType(api);
    return type ? deviceName(*type, false) : std::string();
}

std::string JuceAudioDevices::systemDefaultInputDevice(const std::string& api) const
{
    const auto type = deviceType(api);
    return type ? deviceName(*type, true) : std::string();
}

int JuceAudioDevices::inputChannels(const std::string& api, const std::string& inputDevice) const
{
    const auto type = deviceType(api);
    if (!type || inputDevice.empty() || !type->getDeviceNames(true).contains(juce::String(inputDevice))) {
        return 0;
    }

    const auto key = std::make_pair(api, inputDevice);
    if (const auto it = m_inputChannels.find(key); it != m_inputChannels.end()) {
        return it->second;
    }

    // Creating a device doesn't open it
    const std::unique_ptr<juce::AudioIODevice> device { type->createDevice({}, juce::String(inputDevice)) };
    const int channels = device ? device->getInputChannelNames().size() : 0;
    m_inputChannels.emplace(key, channels);
    return channels;
}

std::vector<uint64_t> JuceAudioDevices::sampleRates() const
{
    std::vector<uint64_t> rates;
    for (int i = 0; i < DeviceManager::NumStandardRates; ++i) {
        rates.push_back(DeviceManager::StandardRates[i]);
    }
    return rates;
}

void JuceAudioDevices::rescan()
{
    m_inputChannels.clear();
    for (const auto& type : m_types) {
        type->scanForDevices();
    }
}

void JuceAudioDevices::refreshInputDevice(const std::string&, const std::string&)
{
}

bool JuceAudioDevices::hasAsioControlPanel(const std::string&, const std::string&, const std::string&) const
{
    // TODO: juce::AudioIODevice::showControlPanel(), once there is an ASIO device type
    return false;
}

void JuceAudioDevices::showAsioControlPanel(const std::string&, const std::string&, const std::string&)
{
}
