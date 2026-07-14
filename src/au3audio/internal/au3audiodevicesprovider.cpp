/*
* Audacity: A Digital Audio Editor
*/

#include "au3audiodevicesprovider.h"

#include <algorithm>

#include "framework/global/containers.h"
#include "framework/global/settings.h"
#include "framework/global/realfn.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-audio-devices/DeviceManager.h"
#include "au3-audio-devices/AudioIOBase.h"
#include "au3-project-rate/QualitySettings.h"
#include "au3-project-rate/ProjectRate.h"

using namespace muse;
using namespace au::au3;
using namespace au::au3audio;

namespace {
const muse::Settings::Key AUDIO_HOST("au3audio", "AudioIO/Host");
const muse::Settings::Key PLAYBACK_DEVICE("au3audio", "AudioIO/PlaybackDevice");
const muse::Settings::Key RECORDING_DEVICE("au3audio", "AudioIO/RecordingDevice");
const muse::Settings::Key INPUT_CHANNELS("au3audio", "AudioIO/RecordChannels");

const muse::Settings::Key AUTOMATIC_LATENCY_COMPENSATION("au3audio", "AudioIO/AutomaticLatencyCompensation");
const muse::Settings::Key LATENCY_DURATION("au3audio", "AudioIO/LatencyDuration");
const muse::Settings::Key LATENCY_COMPENSATION("au3audio", "AudioIO/LatencyCompensation");

const muse::Settings::Key DEFAULT_PROJECT_SAMPLE_RATE("au3audio", "SamplingRate/DefaultProjectSampleRate");
const muse::Settings::Key DEFAULT_PROJECT_SAMPLE_FORMAT("au3audio", "SamplingRate/DefaultProjectSampleFormatChoice");

const muse::Settings::Key RECORDING_SOURCE("au3audio", "AudioIO/RecordingSource");
const muse::Settings::Key RECORDING_SOURCE_INDEX("au3audio", "AudioIO/RecordingSourceIndex");

const muse::Settings::Key ASIO_USE_DEVICE_SAMPLE_RATE("au3audio", "AudioIO/ASIO/UseDeviceSampleRate");

std::string getPreferredAudioHost(const std::vector<std::string>& hosts)
{
    if (hosts.empty()) {
        return "";
    }

#if defined(_WIN32)
    constexpr const char* preferred[] = { "Windows WASAPI", "ASIO", "Windows DirectSound", "MME" };
#elif defined(__APPLE__)
    constexpr const char* preferred[] = { "Core Audio" };
#elif defined(__linux__)
    constexpr const char* preferred[] = { "ALSA", "JACK", "OSS" };
#else
    constexpr const char* preferred[] = {};
#endif

    for (const auto* candidate : preferred) {
        const auto it = std::find_if(hosts.begin(), hosts.end(), [candidate](const std::string& h) {
            return h == candidate;
        });
        if (it != hosts.end()) {
            return *it;
        }
    }

    return hosts.front();
}
}

void Au3AudioDevicesProvider::init()
{
    initHosts();

    muse::settings()->setDefaultValue(AUDIO_HOST, muse::Val(getPreferredAudioHost(apis())));

    if (!m_audioApis.empty() && !muse::contains(m_audioApis, currentApi())) {
        muse::settings()->setLocalValue(AUDIO_HOST, muse::Val(getPreferredAudioHost(m_audioApis)));
    }

    muse::settings()->setDefaultValue(PLAYBACK_DEVICE, muse::Val(std::string()));
    muse::settings()->setDefaultValue(RECORDING_DEVICE, muse::Val(std::string()));

    muse::settings()->setDefaultValue(INPUT_CHANNELS, muse::Val(1));
    muse::settings()->setDefaultValue(LATENCY_DURATION, muse::Val(100.0));
    muse::settings()->setDefaultValue(AUTOMATIC_LATENCY_COMPENSATION, muse::Val(false));
    muse::settings()->setDefaultValue(LATENCY_COMPENSATION, muse::Val(-130.0));
    muse::settings()->setDefaultValue(ASIO_USE_DEVICE_SAMPLE_RATE, muse::Val(true));
    muse::settings()->setDefaultValue(DEFAULT_PROJECT_SAMPLE_RATE, muse::Val(AudioIOBase::GetOptimalSupportedSampleRate()));
    muse::settings()->setDefaultValue(DEFAULT_PROJECT_SAMPLE_FORMAT,
                                      muse::Val(QualitySettings::SampleFormatSetting.Default().Internal().ToStdString()));

    muse::settings()->valueChanged(AUDIO_HOST).onReceive(this, [this](const muse::Val& val) {
        updateInputOutputDevices();
        revalidateInputOutputDevices();
        m_audioApiChanged.notify();
    });

    muse::settings()->valueChanged(PLAYBACK_DEVICE).onReceive(this, [this](const muse::Val& val) {
        Au3AudioDevicesProvider::handleDeviceChange();
        m_audioOutputDeviceChanged.notify();
    });

    muse::settings()->valueChanged(RECORDING_DEVICE).onReceive(this, [this](const muse::Val& val) {
        setupInputDevice();

        m_audioInputDeviceChanged.notify();
        m_inputChannelsListChanged.notify();
        m_inputChannelsChanged.notify();
    });

    muse::settings()->valueChanged(INPUT_CHANNELS).onReceive(this, [this](const muse::Val& val) {
        m_inputChannelsChanged.notify();
    });

    muse::settings()->valueChanged(AUTOMATIC_LATENCY_COMPENSATION).onReceive(this, [this](const muse::Val& val) {
        m_automaticCompensationEnabledChanged.notify();
    });

    muse::settings()->valueChanged(LATENCY_DURATION).onReceive(this, [this](const muse::Val& val) {
        m_bufferLengthChanged.notify();
    });

    muse::settings()->valueChanged(LATENCY_COMPENSATION).onReceive(this, [this](const muse::Val& val) {
        m_latencyCompensationChanged.notify();
    });

    muse::settings()->valueChanged(ASIO_USE_DEVICE_SAMPLE_RATE).onReceive(this, [this](const muse::Val& val) {
        m_asioUseDeviceSampleRateChanged.notify();
    });

    muse::settings()->valueChanged(DEFAULT_PROJECT_SAMPLE_FORMAT).onReceive(this, [this](const muse::Val& val) {
        QualitySettings::SampleFormatSetting.Write(wxString(val.toString()));
        m_defaultSampleFormatChanged.notify();
    });

    muse::settings()->valueChanged(DEFAULT_PROJECT_SAMPLE_RATE).onReceive(this, [this](const muse::Val& val) {
        // At the moment changing the default sample rate also changes the current project rate
        auto currentProject = globalContext()->currentProject();
        if (currentProject) {
            Au3Project* project = reinterpret_cast<Au3Project*>(currentProject->au3ProjectPtr());
            ::ProjectRate::Get(*project).SetRate(val.toInt());
        }
        m_defaultSampleRateChanged.notify();
    });

    updateInputOutputDevices();
    initInputChannels();
}

std::vector<std::string> Au3AudioDevicesProvider::outputDevices() const
{
    return m_outputDevices;
}

std::optional<std::string> Au3AudioDevicesProvider::currentOutputDevice() const
{
    const std::string device = muse::settings()->value(PLAYBACK_DEVICE).toString();
    if (device.empty()) {
        return std::nullopt;
    }
    return device;
}

void Au3AudioDevicesProvider::setOutputDevice(const std::optional<std::string>& device)
{
    if (device.has_value() && muse::contains(m_outputDevices, device.value())) {
        muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(device.value()));
    } else {
        muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(std::string()));
    }
}

async::Notification Au3AudioDevicesProvider::outputDeviceChanged() const
{
    return m_audioOutputDeviceChanged;
}

std::vector<std::string> Au3AudioDevicesProvider::inputDevices() const
{
    return m_inputDevices;
}

std::optional<std::string> Au3AudioDevicesProvider::currentInputDevice() const
{
    const std::string device = muse::settings()->value(RECORDING_DEVICE).toString();
    if (device.empty()) {
        return std::nullopt;
    }
    return device;
}

void Au3AudioDevicesProvider::setInputDevice(const std::optional<std::string>& device)
{
    if (device.has_value() && muse::contains(m_inputDevices, device.value())) {
        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(device.value()));
    } else {
        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(std::string()));
    }
}

async::Notification Au3AudioDevicesProvider::inputDeviceChanged() const
{
    return m_audioInputDeviceChanged;
}

bool Au3AudioDevicesProvider::hasRecordingDevices() const
{
    return !m_inputDevices.empty() && m_inputChannelsAvailable > 0;
}

void Au3AudioDevicesProvider::handleDeviceChange()
{
    if (audioEngine()) {
        audioEngine()->stopMonitoring();
        audioEngine()->handleDeviceChange();
    }
}

std::vector<std::string> Au3AudioDevicesProvider::apis() const
{
    return m_audioApis;
}

std::string Au3AudioDevicesProvider::currentApi() const
{
    return muse::settings()->value(AUDIO_HOST).toString();
}

void Au3AudioDevicesProvider::setApi(const std::string& audioApi)
{
    muse::settings()->setLocalValue(AUDIO_HOST, muse::Val(audioApi));
}

int Au3AudioDevicesProvider::inputChannelsSelected() const
{
    return muse::settings()->value(INPUT_CHANNELS).toInt();
}

int Au3AudioDevicesProvider::inputChannelsAvailable() const
{
    return m_inputChannelsAvailable;
}

void Au3AudioDevicesProvider::setInputChannels(const int count)
{
    muse::settings()->setLocalValue(INPUT_CHANNELS, muse::Val(count));
}

double Au3AudioDevicesProvider::bufferLength() const
{
    return muse::settings()->value(LATENCY_DURATION).toDouble();
}

void Au3AudioDevicesProvider::setBufferLength(double newBufferLength)
{
    if (!muse::RealIsEqualOrMore(newBufferLength, 0.0)) {
        newBufferLength = muse::settings()->defaultValue(LATENCY_DURATION).toDouble();
    }
    muse::settings()->setLocalValue(LATENCY_DURATION, muse::Val(newBufferLength));
}

bool Au3AudioDevicesProvider::automaticCompensationEnabled() const
{
    return muse::settings()->value(AUTOMATIC_LATENCY_COMPENSATION).toBool();
}

void Au3AudioDevicesProvider::setAutomaticCompensationEnabled(bool enabled)
{
    muse::settings()->setLocalValue(AUTOMATIC_LATENCY_COMPENSATION, muse::Val(enabled));
}

double Au3AudioDevicesProvider::latencyCompensation() const
{
    return muse::settings()->value(LATENCY_COMPENSATION).toDouble();
}

void Au3AudioDevicesProvider::setLatencyCompensation(double newLatencyCompensation)
{
    muse::settings()->setLocalValue(LATENCY_COMPENSATION, muse::Val(newLatencyCompensation));
}

bool Au3AudioDevicesProvider::asioUseDeviceSampleRate() const
{
    return muse::settings()->value(ASIO_USE_DEVICE_SAMPLE_RATE).toBool();
}

void Au3AudioDevicesProvider::setAsioUseDeviceSampleRate(bool use)
{
    muse::settings()->setLocalValue(ASIO_USE_DEVICE_SAMPLE_RATE, muse::Val(use));
}

muse::async::Notification Au3AudioDevicesProvider::asioUseDeviceSampleRateChanged() const
{
    return m_asioUseDeviceSampleRateChanged;
}

void Au3AudioDevicesProvider::showAsioControlPanel()
{
    int paIndex = deviceManager()->outputDevicePaIndex(currentApi(), effectiveOutputDevice());
    if (paIndex < 0) {
        paIndex = deviceManager()->inputDevicePaIndex(currentApi(), effectiveInputDevice());
    }

    if (paIndex < 0) {
        return;
    }

    // the panel cannot open while the device is in use
    if (audioEngine()) {
        audioEngine()->stopMonitoring();
    }

    deviceManager()->showAsioControlPanel(paIndex);
}

std::vector<uint64_t> Au3AudioDevicesProvider::sampleRates() const
{
    std::vector<uint64_t> rates;
    for (int i = 0; i < AudioIOBase::NumStandardRates; ++i) {
        int iRate = AudioIOBase::StandardRates[i];
        rates.push_back(iRate);
    }

    return rates;
}

uint64_t Au3AudioDevicesProvider::defaultSampleRate() const
{
    return muse::settings()->value(DEFAULT_PROJECT_SAMPLE_RATE).toInt();
}

void Au3AudioDevicesProvider::setDefaultSampleRate(uint64_t newRate)
{
    settings()->setLocalValue(DEFAULT_PROJECT_SAMPLE_RATE, muse::Val(static_cast<int>(newRate)));
}

void Au3AudioDevicesProvider::setDefaultSampleFormat(const std::string& format)
{
    for (const auto& symbol : QualitySettings::SampleFormatSetting.GetSymbols()) {
        if (format == symbol.Msgid().translated().toStdString()) {
            settings()->setLocalValue(DEFAULT_PROJECT_SAMPLE_FORMAT, muse::Val(symbol.Internal().ToStdString()));
        }
    }
}

async::Notification Au3AudioDevicesProvider::defaultSampleFormatChanged() const
{
    return m_defaultSampleFormatChanged;
}

std::string Au3AudioDevicesProvider::defaultSampleFormat() const
{
    auto currentFormat = muse::settings()->value(DEFAULT_PROJECT_SAMPLE_FORMAT).toString();
    for (const auto& symbol : QualitySettings::SampleFormatSetting.GetSymbols()) {
        if (currentFormat == symbol.Internal()) {
            return symbol.Msgid().translated().toStdString();
        }
    }

    return "";
}

std::vector<std::string> Au3AudioDevicesProvider::sampleFormats() const
{
    std::vector<std::string> formats;
    for (const auto& format : QualitySettings::SampleFormatSetting.GetSymbols().GetMsgids()) {
        formats.push_back(format.translated().toStdString());
    }

    return formats;
}

async::Notification Au3AudioDevicesProvider::defaultSampleRateChanged() const
{
    return m_defaultSampleRateChanged;
}

async::Notification Au3AudioDevicesProvider::automaticCompensationEnabledChanged() const
{
    return m_automaticCompensationEnabledChanged;
}

async::Notification Au3AudioDevicesProvider::latencyCompensationChanged() const
{
    return m_latencyCompensationChanged;
}

async::Notification Au3AudioDevicesProvider::bufferLengthChanged() const
{
    return m_bufferLengthChanged;
}

async::Notification Au3AudioDevicesProvider::inputChannelsAvailableChanged() const
{
    return m_inputChannelsListChanged;
}

async::Notification Au3AudioDevicesProvider::inputChannelsChanged() const
{
    return m_inputChannelsChanged;
}

async::Notification Au3AudioDevicesProvider::apiChanged() const
{
    return m_audioApiChanged;
}

void Au3AudioDevicesProvider::initHosts()
{
    const std::vector<DeviceSourceMap>& inMaps = deviceManager()->inputDeviceMaps();
    const std::vector<DeviceSourceMap>& outMaps = deviceManager()->outputDeviceMaps();

    m_audioApis.clear();

    for (auto& device : inMaps) {
        std::string host = wxToStdString(device.hostString);
        if (!muse::contains(m_audioApis, host)) {
            m_audioApis.push_back(host);
        }
    }

    for (auto& device : outMaps) {
        std::string host = wxToStdString(device.hostString);
        if (!muse::contains(m_audioApis, host)) {
            m_audioApis.push_back(host);
        }
    }
}

void Au3AudioDevicesProvider::initInputChannels()
{
    const std::vector<DeviceSourceMap>& inMaps = deviceManager()->inputDeviceMaps();
    const std::string host = currentApi();
    const std::string inputDevice = effectiveInputDevice();

    m_inputChannelsAvailable = 0;

    for (const auto& device : inMaps) {
        if (device.hostString != host) {
            continue;
        }
        const auto deviceName = MakeDeviceSourceString(&device, inMaps);
        if (deviceName == inputDevice) {
            m_inputChannelsAvailable = device.numChannels;
            break;
        }
    }
}

void Au3AudioDevicesProvider::updateInputOutputDevices()
{
    const std::vector<DeviceSourceMap>& inputDevices = deviceManager()->inputDeviceMaps();
    const std::vector<DeviceSourceMap>& outputDevices = deviceManager()->outputDeviceMaps();

    m_inputDevices.clear();
    m_outputDevices.clear();

    for (const auto& device : inputDevices) {
        if (device.hostString != currentApi()) {
            continue;
        }
        m_inputDevices.push_back(MakeDeviceSourceString(&device, inputDevices));
    }

    for (const auto& device : outputDevices) {
        if (device.hostString != currentApi()) {
            continue;
        }
        m_outputDevices.push_back(MakeDeviceSourceString(&device, outputDevices));
    }
}

void Au3AudioDevicesProvider::revalidateInputOutputDevices()
{
    // A stored device may not exist anymore after a host change or a rescan: reset it
    // to the system default. Resetting fires the setting's valueChanged handler, which
    // refreshes everything; when the stored value is kept, no notification fires even
    // though the same name (or the system default) may now resolve to a different
    // device, so refresh explicitly.
    const std::string inputDevice = muse::settings()->value(RECORDING_DEVICE).toString();
    if (!inputDevice.empty() && !muse::contains(m_inputDevices, inputDevice)) {
        setInputDevice(std::nullopt);
    } else {
        setupInputDevice();
        m_audioInputDeviceChanged.notify();
        m_inputChannelsListChanged.notify();
        m_inputChannelsChanged.notify();
    }

    const std::string outputDevice = muse::settings()->value(PLAYBACK_DEVICE).toString();
    if (!outputDevice.empty() && !muse::contains(m_outputDevices, outputDevice)) {
        setOutputDevice(std::nullopt);
    } else {
        handleDeviceChange();
        m_audioOutputDeviceChanged.notify();
    }
}

void Au3AudioDevicesProvider::setupInputDevice()
{
    const std::vector<DeviceSourceMap>& inMaps = deviceManager()->inputDeviceMaps();
    const std::string host = currentApi();

    int prevInputChannels = muse::settings()->value(INPUT_CHANNELS).toInt();

    const std::string effectiveDevice = effectiveInputDevice();

    m_inputChannelsAvailable = 0;

    for (const auto& device : inMaps) {
        const auto deviceName = MakeDeviceSourceString(&device, inMaps);
        if (device.hostString != host || deviceName != effectiveDevice) {
            continue;
        }

        deviceManager()->updateAsioDeviceCaps(device.deviceIndex);

        muse::settings()->setLocalValue(RECORDING_SOURCE_INDEX, muse::Val(device.sourceIndex));

        if (device.totalSources >= 1) {
            muse::settings()->setLocalValue(RECORDING_SOURCE, muse::Val(device.sourceString.ToStdString()));
        } else {
            muse::settings()->setLocalValue(RECORDING_SOURCE, muse::Val());
        }

        m_inputChannelsAvailable = device.numChannels;
        muse::settings()->setLocalValue(INPUT_CHANNELS, muse::Val(std::min(prevInputChannels, m_inputChannelsAvailable)));

        handleDeviceChange();

        break;
    }
}

std::string Au3AudioDevicesProvider::effectiveOutputDevice() const
{
    const std::string device = muse::settings()->value(PLAYBACK_DEVICE).toString();
    if (device.empty() || !muse::contains(m_outputDevices, device)) {
        return systemDefaultOutputDevice();
    }
    return device;
}

std::string Au3AudioDevicesProvider::effectiveInputDevice() const
{
    const std::string device = muse::settings()->value(RECORDING_DEVICE).toString();
    if (device.empty() || !muse::contains(m_inputDevices, device)) {
        return systemDefaultInputDevice();
    }
    return device;
}

std::string Au3AudioDevicesProvider::systemDefaultOutputDevice() const
{
    const std::vector<DeviceSourceMap>& outMaps = deviceManager()->outputDeviceMaps();
    const int hostIndex = deviceManager()->hostIndex(currentApi());
    return MakeDeviceSourceString(deviceManager()->defaultOutputDevice(hostIndex), outMaps);
}

std::string Au3AudioDevicesProvider::systemDefaultInputDevice() const
{
    const std::vector<DeviceSourceMap>& inMaps = deviceManager()->inputDeviceMaps();
    const int hostIndex = deviceManager()->hostIndex(currentApi());
    return MakeDeviceSourceString(deviceManager()->defaultInputDevice(hostIndex), inMaps);
}

void Au3AudioDevicesProvider::rescan()
{
    deviceManager()->rescan();

    initHosts();
    updateInputOutputDevices();
    revalidateInputOutputDevices();

    m_audioApiChanged.notify();
}
