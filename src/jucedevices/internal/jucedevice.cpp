/*
 * Audacity: A Digital Audio Editor
 */
#include "jucedevice.h"

#include <algorithm>

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "juceaudiodevices.h"

using namespace au::jucedevices;

namespace {
std::string effectiveOutputDevice(const JuceAudioDevices& devices, const au::audio::AudioConfiguration& configuration)
{
    if (configuration.outputDevice && muse::contains(devices.outputDevices(configuration.api), *configuration.outputDevice)) {
        return *configuration.outputDevice;
    }
    return devices.systemDefaultOutputDevice(configuration.api);
}

std::string effectiveInputDevice(const JuceAudioDevices& devices, const au::audio::AudioConfiguration& configuration)
{
    if (configuration.inputDevice && muse::contains(devices.inputDevices(configuration.api), *configuration.inputDevice)) {
        return *configuration.inputDevice;
    }
    return devices.systemDefaultInputDevice(configuration.api);
}

//! The requested rate if the device supports it, else its highest
double bestSampleRate(juce::AudioIODevice& device, double requested)
{
    const auto rates = device.getAvailableSampleRates();
    if (rates.isEmpty() || rates.contains(requested)) {
        return requested;
    }
    return *std::max_element(rates.begin(), rates.end());
}

//! As AudioIO asks PortAudio for `bufferLength` ms of latency: the longest buffer of the device that isn't longer, or its
//! shortest. JUCE's ALSA devices list sizes without asking the device: with PipeWire, the shortest (32 frames) may open but
//! stay silent.
int bufferSize(juce::AudioIODevice& device, double bufferLength, double sampleRate)
{
    auto sizes = device.getAvailableBufferSizes();
    if (sizes.isEmpty()) {
        return device.getDefaultBufferSize();
    }
    std::sort(sizes.begin(), sizes.end());
    const auto wanted = bufferLength / 1000.0 * sampleRate;
    int size = sizes.getFirst();
    for (const auto candidate : sizes) {
        if (candidate <= wanted) {
            size = candidate;
        }
    }
    return size;
}
}

JuceDevice::JuceDevice(std::unique_ptr<juce::AudioIODevice> device)
    : m_device{std::move(device)}
{
}

JuceDevice::~JuceDevice()
{
    stop();
    m_device->close();
}

double JuceDevice::sampleRate() const
{
    return m_device->getCurrentSampleRate();
}

int JuceDevice::blockSize() const
{
    return std::max(1, m_device->getCurrentBufferSizeSamples());
}

int JuceDevice::numInputChannels() const
{
    return m_device->getActiveInputChannels().countNumberOfSetBits();
}

int JuceDevice::numOutputChannels() const
{
    return m_device->getActiveOutputChannels().countNumberOfSetBits();
}

double JuceDevice::inputLatency() const
{
    return m_device->getInputLatencyInSamples() / sampleRate();
}

double JuceDevice::outputLatency() const
{
    return m_device->getOutputLatencyInSamples() / sampleRate();
}

void JuceDevice::start(audio::IDeviceCallback& callback)
{
    m_callback = &callback;
    m_device->start(this);
}

void JuceDevice::stop()
{
    m_device->stop();
}

int JuceDevice::xrunCount() const
{
    return m_device->getXRunCount();
}

void JuceDevice::audioDeviceIOCallbackWithContext(const float* const* inputChannelData, int numInputChannels,
                                                  float* const* outputChannelData, int numOutputChannels, int numSamples,
                                                  const juce::AudioIODeviceCallbackContext&)
{
    m_callback->process(inputChannelData, numInputChannels, outputChannelData, numOutputChannels, numSamples);
}

void JuceDevice::audioDeviceError(const juce::String& errorMessage)
{
    LOGE() << "audio device error: " << errorMessage.toStdString();
}

JuceDeviceOpener::JuceDeviceOpener(std::shared_ptr<JuceAudioDevices> devices)
    : m_devices{std::move(devices)}
{
}

std::unique_ptr<au::audio::IDevice> JuceDeviceOpener::open(const audio::AudioConfiguration& configuration, int numInputChannels,
                                                           int numOutputChannels, double sampleRate, std::string& error)
{
    // With input only, JUCE's ALSA devices stall after their first callback: with an output too, which plays silence
    if (numInputChannels > 0 && numOutputChannels == 0) {
        numOutputChannels = 2;
    }
    const auto type = m_devices->deviceType(configuration.api);
    if (!type) {
        error = "No such audio API: " + configuration.api;
        return nullptr;
    }
    const auto outputDevice = numOutputChannels > 0 ? effectiveOutputDevice(*m_devices, configuration) : std::string {};
    const auto inputDevice = numInputChannels > 0 ? effectiveInputDevice(*m_devices, configuration) : std::string {};
    std::unique_ptr<juce::AudioIODevice> device { type->createDevice(juce::String(outputDevice), juce::String(inputDevice)) };
    if (!device) {
        error = "Cannot create audio device " + outputDevice + " / " + inputDevice;
        return nullptr;
    }

    juce::BigInteger inputChannels;
    inputChannels.setRange(0, std::min(numInputChannels, device->getInputChannelNames().size()), true);
    juce::BigInteger outputChannels;
    outputChannels.setRange(0, std::min(numOutputChannels, device->getOutputChannelNames().size()), true);
    const auto rate = bestSampleRate(*device, sampleRate);
    const auto openError = device->open(inputChannels, outputChannels, rate, bufferSize(*device, configuration.bufferLength, rate));
    if (openError.isNotEmpty()) {
        error = openError.toStdString();
        return nullptr;
    }
    return std::make_unique<JuceDevice>(std::move(device));
}
