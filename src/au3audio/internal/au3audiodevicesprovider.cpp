/*
* Audacity: A Digital Audio Editor
*/

#include "au3audiodevicesprovider.h"

#include <algorithm>

#include "framework/global/containers.h"
#include "framework/global/settings.h"
#include "framework/global/realfn.h"
#include "log.h"
#include "types/translatablestring.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-audio-devices/DeviceManager.h"
#include "au3-audio-devices/AudioIOBase.h"
#include "au3-project-rate/QualitySettings.h"
#include "au3-project-rate/ProjectRate.h"

using namespace muse;
using namespace au::au3;
using namespace au::au3audio;

static const muse::Settings::Key AUDIO_HOST("au3audio", "AudioIO/Host");
static const muse::Settings::Key PLAYBACK_DEVICE("au3audio", "AudioIO/PlaybackDevice");
static const muse::Settings::Key RECORDING_DEVICE("au3audio", "AudioIO/RecordingDevice");
static const muse::Settings::Key INPUT_CHANNELS("au3audio", "AudioIO/RecordChannels");
static const muse::Settings::Key WRAP_AUDIO_HOST("au3wrap", "AudioIO/Host");
static const muse::Settings::Key WRAP_PLAYBACK_DEVICE("au3wrap", "AudioIO/PlaybackDevice");
static const muse::Settings::Key WRAP_RECORDING_DEVICE("au3wrap", "AudioIO/RecordingDevice");

static const muse::Settings::Key LATENCY_DURATION("au3audio", "AudioIO/LatencyDuration");
static const muse::Settings::Key LATENCY_CORRECTION("au3audio", "AudioIO/LatencyCorrection");

static const muse::Settings::Key DEFAULT_PROJECT_SAMPLE_RATE("au3audio", "SamplingRate/DefaultProjectSampleRate");
static const muse::Settings::Key DEFAULT_PROJECT_SAMPLE_FORMAT("au3audio", "SamplingRate/DefaultProjectSampleFormatChoice");

static const muse::Settings::Key RECORDING_SOURCE("au3audio", "AudioIO/RecordingSource");
static const muse::Settings::Key RECORDING_SOURCE_INDEX("au3audio", "AudioIO/RecordingSourceIndex");

namespace {
QString systemDefaultDeviceLabel()
{
    return muse::qtrc("preferences", "System default");
}

std::string systemDefaultDeviceName()
{
    return systemDefaultDeviceLabel().toStdString();
}

bool isSystemDefaultDeviceSelection(const std::string& device)
{
    return device.empty() || device == systemDefaultDeviceName();
}
}

static std::string getPreferredAudioHost(const std::vector<std::string>& hosts)
{
    if (hosts.empty()) {
        return "";
    }

#if defined(_WIN32)
    constexpr const char* preferred[] = { "ASIO", "Windows WASAPI", "Windows DirectSound", "MME" };
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

void Au3AudioDevicesProvider::init()
{
    initHosts();

    muse::settings()->setDefaultValue(AUDIO_HOST, muse::Val(getPreferredAudioHost(apis())));
    muse::settings()->setDefaultValue(WRAP_AUDIO_HOST, muse::Val(getPreferredAudioHost(apis())));

    muse::settings()->setDefaultValue(PLAYBACK_DEVICE, muse::Val(std::string()));
    muse::settings()->setDefaultValue(RECORDING_DEVICE, muse::Val(std::string()));
    muse::settings()->setDefaultValue(WRAP_PLAYBACK_DEVICE, muse::Val(std::string()));
    muse::settings()->setDefaultValue(WRAP_RECORDING_DEVICE, muse::Val(std::string()));

    muse::settings()->setDefaultValue(INPUT_CHANNELS, muse::Val(1));
    muse::settings()->setDefaultValue(LATENCY_DURATION, muse::Val(100.0));
    muse::settings()->setDefaultValue(LATENCY_CORRECTION, muse::Val(-130.0));
    muse::settings()->setDefaultValue(DEFAULT_PROJECT_SAMPLE_RATE, muse::Val(AudioIOBase::GetOptimalSupportedSampleRate()));
    muse::settings()->setDefaultValue(DEFAULT_PROJECT_SAMPLE_FORMAT,
                                      muse::Val(QualitySettings::SampleFormatSetting.Default().Internal().ToStdString()));

    muse::settings()->valueChanged(AUDIO_HOST).onReceive(nullptr, [this](const muse::Val& val) {
        updateInputOutputDevices();
        muse::settings()->setLocalValue(WRAP_AUDIO_HOST, muse::Val(val.toString()));
        LOGI() << "Audio host changed (au3audio): " << val.toString()
               << " (au3wrap synced)";
        m_audioApiChanged.notify();
    });

    muse::settings()->valueChanged(PLAYBACK_DEVICE).onReceive(nullptr, [this](const muse::Val& val) {
        const std::string device = val.toString();
        muse::settings()->setLocalValue(WRAP_PLAYBACK_DEVICE,
                                        muse::Val(isSystemDefaultDeviceSelection(device) ? std::string() : device));
        LOGI() << "Playback device changed (au3audio): " << device
               << " (au3wrap synced: "
               << muse::settings()->value(WRAP_PLAYBACK_DEVICE).toString() << ")";
        Au3AudioDevicesProvider::handleDeviceChange();
        m_audioOutputDeviceChanged.notify();
    });

    muse::settings()->valueChanged(RECORDING_DEVICE).onReceive(nullptr, [this](const muse::Val& val) {
        setupInputDevice(val.toString());
        const std::string device = val.toString();
        muse::settings()->setLocalValue(WRAP_RECORDING_DEVICE,
                                        muse::Val(isSystemDefaultDeviceSelection(device) ? std::string() : device));
        LOGI() << "Recording device changed (au3audio): " << device
               << " (au3wrap synced: "
               << muse::settings()->value(WRAP_RECORDING_DEVICE).toString() << ")";

        m_audioInputDeviceChanged.notify();
        m_inputChannelsListChanged.notify();
        m_inputChannelsChanged.notify();
    });

    muse::settings()->valueChanged(INPUT_CHANNELS).onReceive(nullptr, [this](const muse::Val& val) {
        m_inputChannelsChanged.notify();
    });

    muse::settings()->valueChanged(LATENCY_DURATION).onReceive(nullptr, [this](const muse::Val& val) {
        m_bufferLengthChanged.notify();
    });

    muse::settings()->valueChanged(LATENCY_CORRECTION).onReceive(nullptr, [this](const muse::Val& val) {
        m_latencyCompensationChanged.notify();
    });

    muse::settings()->valueChanged(DEFAULT_PROJECT_SAMPLE_FORMAT).onReceive(nullptr, [this](const muse::Val& val) {
        QualitySettings::SampleFormatSetting.Write(wxString(val.toString()));
        m_defaultSampleFormatChanged.notify();
    });

    muse::settings()->valueChanged(DEFAULT_PROJECT_SAMPLE_RATE).onReceive(nullptr, [this](const muse::Val& val) {
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

    m_lastDefaultOutputDevice = systemDefaultOutputDevice();
    m_lastDefaultInputDevice = systemDefaultInputDevice();
    LOGI() << "Audio devices init: api=" << currentApi()
           << " output selection=" << muse::settings()->value(PLAYBACK_DEVICE).toString()
           << " input selection=" << muse::settings()->value(RECORDING_DEVICE).toString()
           << " system default out=" << m_lastDefaultOutputDevice
           << " system default in=" << m_lastDefaultInputDevice;
    m_defaultDevicePollTimer.onTimeout(this, [this]() {
        checkSystemDefaultDeviceChanges();
    });
    m_defaultDevicePollTimer.start();
}

std::vector<std::string> Au3AudioDevicesProvider::outputDevices() const
{
    std::vector<std::string> devices;
    devices.reserve(m_outputDevices.size() + 1);
    devices.push_back(systemDefaultDeviceName());
    devices.insert(devices.end(), m_outputDevices.begin(), m_outputDevices.end());
    return devices;
}

std::string Au3AudioDevicesProvider::currentOutputDevice() const
{
    auto device = muse::settings()->value(PLAYBACK_DEVICE).toString();
    if (device.empty()) {
        return systemDefaultDeviceName();
    }
    return device;
}

void Au3AudioDevicesProvider::setOutputDevice(const std::string& device)
{
    if (isSystemDefaultDeviceSelection(device)) {
        if (!muse::settings()->value(PLAYBACK_DEVICE).toString().empty()) {
            muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(std::string()));
        }
        muse::settings()->setLocalValue(WRAP_PLAYBACK_DEVICE, muse::Val(std::string()));
        LOGI() << "Set playback device: system default (au3wrap synced)";
        return;
    }

    if (muse::contains(m_outputDevices, device)) {
        muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(device));
        muse::settings()->setLocalValue(WRAP_PLAYBACK_DEVICE, muse::Val(device));
        LOGI() << "Set playback device: " << device;
    } else if (!m_outputDevices.empty()) {
        const std::string fallback = m_outputDevices.front();
        muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(fallback));
        muse::settings()->setLocalValue(WRAP_PLAYBACK_DEVICE, muse::Val(fallback));
        LOGI() << "Playback device not found, falling back to: " << fallback;
    } else {
        muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val());
        muse::settings()->setLocalValue(WRAP_PLAYBACK_DEVICE, muse::Val());
        LOGI() << "Playback device list empty, falling back to system default";
    }
}

async::Notification Au3AudioDevicesProvider::outputDeviceChanged() const
{
    return m_audioOutputDeviceChanged;
}

std::vector<std::string> Au3AudioDevicesProvider::inputDevices() const
{
    std::vector<std::string> devices;
    devices.reserve(m_inputDevices.size() + 1);
    devices.push_back(systemDefaultDeviceName());
    devices.insert(devices.end(), m_inputDevices.begin(), m_inputDevices.end());
    return devices;
}

std::string Au3AudioDevicesProvider::currentInputDevice() const
{
    auto device = muse::settings()->value(RECORDING_DEVICE).toString();
    if (device.empty()) {
        return systemDefaultDeviceName();
    }
    return device;
}

void Au3AudioDevicesProvider::setInputDevice(const std::string& device)
{
    if (isSystemDefaultDeviceSelection(device)) {
        if (!muse::settings()->value(RECORDING_DEVICE).toString().empty()) {
            muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(std::string()));
        }
        muse::settings()->setLocalValue(WRAP_RECORDING_DEVICE, muse::Val(std::string()));
        LOGI() << "Set recording device: system default (au3wrap synced)";
        return;
    }

    if (muse::contains(m_inputDevices, device)) {
        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(device));
        muse::settings()->setLocalValue(WRAP_RECORDING_DEVICE, muse::Val(device));
        LOGI() << "Set recording device: " << device;
    } else if (!m_inputDevices.empty()) {
        const std::string fallback = m_inputDevices.front();
        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(fallback));
        muse::settings()->setLocalValue(WRAP_RECORDING_DEVICE, muse::Val(fallback));
        LOGI() << "Recording device not found, falling back to: " << fallback;
    } else {
        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val());
        muse::settings()->setLocalValue(WRAP_RECORDING_DEVICE, muse::Val());
        LOGI() << "Recording device list empty, falling back to system default";
    }
}

async::Notification Au3AudioDevicesProvider::inputDeviceChanged() const
{
    return m_audioInputDeviceChanged;
}

void Au3AudioDevicesProvider::handleDeviceChange()
{
    if (!audioEngine()) {
        return;
    }

    if (auto* audioIo = AudioIOBase::Get(); audioIo && audioIo->IsStreamActive()) {
        LOGI() << "Stopping active stream for device change";
        audioEngine()->stopStream();
    }

    audioEngine()->stopMonitoring();
    audioEngine()->handleDeviceChange();
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
    muse::settings()->setLocalValue(WRAP_AUDIO_HOST, muse::Val(audioApi));
    LOGI() << "Set audio host: " << audioApi << " (au3wrap synced)";
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

double Au3AudioDevicesProvider::latencyCompensation() const
{
    return muse::settings()->value(LATENCY_CORRECTION).toDouble();
}

void Au3AudioDevicesProvider::setLatencyCompensation(double newLatencyCompensation)
{
    settings()->setLocalValue(LATENCY_CORRECTION, muse::Val(newLatencyCompensation));
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
        if (format == symbol.Msgid().Translation()) {
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
            return symbol.Msgid().Translation().ToStdString();
        }
    }

    return "";
}

std::vector<std::string> Au3AudioDevicesProvider::sampleFormats() const
{
    std::vector<std::string> formats;
    for (const auto& format : QualitySettings::SampleFormatSetting.GetSymbols().GetMsgids()) {
        formats.push_back(format.Translation().ToStdString());
    }

    return formats;
}

async::Notification Au3AudioDevicesProvider::defaultSampleRateChanged() const
{
    return m_defaultSampleRateChanged;
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
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    const std::vector<DeviceSourceMap>& outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();

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
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    const std::string host = currentApi();
    std::string inputDevice = muse::settings()->value(RECORDING_DEVICE).toString();
    if (isSystemDefaultDeviceSelection(inputDevice)) {
        inputDevice.clear();
        int deviceIndex = DeviceManager::Instance()->GetSystemDefaultInputDeviceIndex(host);
        if (deviceIndex >= 0) {
            for (const auto& device : inMaps) {
                if (device.deviceIndex == deviceIndex) {
                    inputDevice = MakeDeviceSourceString(&device, inMaps);
                    break;
                }
            }
        }
        if (inputDevice.empty()) {
            int hostIndex = DeviceManager::Instance()->GetHostIndex(host);
            DeviceSourceMap* defaultMap = DeviceManager::Instance()->GetDefaultInputDevice(hostIndex);
            inputDevice = MakeDeviceSourceString(defaultMap, inMaps);
        }
    }

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
    const std::vector<DeviceSourceMap>& inputDevices = DeviceManager::Instance()->GetInputDeviceMaps();
    const std::vector<DeviceSourceMap>& outputDevices = DeviceManager::Instance()->GetOutputDeviceMaps();

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

    LOGI() << "Device list updated for host: " << currentApi()
           << " (outputs=" << m_outputDevices.size()
           << ", inputs=" << m_inputDevices.size() << ")";
    for (size_t i = 0; i < m_outputDevices.size(); ++i) {
        LOGI() << "  output[" << i << "]: " << m_outputDevices[i];
    }
    for (size_t i = 0; i < m_inputDevices.size(); ++i) {
        LOGI() << "  input[" << i << "]: " << m_inputDevices[i];
    }

    setInputDevice(defaultInputDevice());
    setOutputDevice(defaultOutputDevice());
}

void Au3AudioDevicesProvider::setupInputDevice(const std::string& newDevice)
{
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    auto host = muse::settings()->value(AUDIO_HOST).toString();

    int prevInputChannels = muse::settings()->value(INPUT_CHANNELS).toInt();
    bool useSystemDefault = isSystemDefaultDeviceSelection(newDevice);
    std::string effectiveDevice = useSystemDefault ? std::string() : newDevice;
    if (useSystemDefault) {
        int deviceIndex = DeviceManager::Instance()->GetSystemDefaultInputDeviceIndex(host);
        if (deviceIndex >= 0) {
            for (const auto& device : inMaps) {
                if (device.deviceIndex == deviceIndex) {
                    effectiveDevice = MakeDeviceSourceString(&device, inMaps);
                    break;
                }
            }
        }
        if (effectiveDevice.empty()) {
            int hostIndex = DeviceManager::Instance()->GetHostIndex(host);
            DeviceSourceMap* defaultMap = DeviceManager::Instance()->GetDefaultInputDevice(hostIndex);
            effectiveDevice = MakeDeviceSourceString(defaultMap, inMaps);
        }
    }

    for (const auto& device : inMaps) {
        const auto deviceName = MakeDeviceSourceString(&device, inMaps);
        if (device.hostString != host || deviceName != effectiveDevice) {
            continue;
        }

        if (!useSystemDefault) {
            muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(newDevice));
        } else if (!muse::settings()->value(RECORDING_DEVICE).toString().empty()) {
            muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(std::string()));
        }
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

std::string Au3AudioDevicesProvider::defaultOutputDevice()
{
    return muse::settings()->value(PLAYBACK_DEVICE).toString();
}

std::string Au3AudioDevicesProvider::defaultInputDevice()
{
    return muse::settings()->value(RECORDING_DEVICE).toString();
}

std::string Au3AudioDevicesProvider::systemDefaultOutputDevice() const
{
    const std::vector<DeviceSourceMap>& outputDevices = DeviceManager::Instance()->GetOutputDeviceMaps();
    const std::string hostName = currentApi();
    int deviceIndex = DeviceManager::Instance()->GetSystemDefaultOutputDeviceIndex(hostName);
    if (deviceIndex >= 0) {
        for (const auto& device : outputDevices) {
            if (device.deviceIndex == deviceIndex) {
                return MakeDeviceSourceString(&device, outputDevices);
            }
        }
    }

    int hostIndex = DeviceManager::Instance()->GetHostIndex(hostName);
    if (hostIndex < 0) {
        return {};
    }

    DeviceSourceMap* defaultMap = DeviceManager::Instance()->GetDefaultOutputDevice(hostIndex);
    return MakeDeviceSourceString(defaultMap, outputDevices);
}

std::string Au3AudioDevicesProvider::systemDefaultInputDevice() const
{
    const std::vector<DeviceSourceMap>& inputDevices = DeviceManager::Instance()->GetInputDeviceMaps();
    const std::string hostName = currentApi();
    int deviceIndex = DeviceManager::Instance()->GetSystemDefaultInputDeviceIndex(hostName);
    if (deviceIndex >= 0) {
        for (const auto& device : inputDevices) {
            if (device.deviceIndex == deviceIndex) {
                return MakeDeviceSourceString(&device, inputDevices);
            }
        }
    }

    int hostIndex = DeviceManager::Instance()->GetHostIndex(hostName);
    if (hostIndex < 0) {
        return {};
    }

    DeviceSourceMap* defaultMap = DeviceManager::Instance()->GetDefaultInputDevice(hostIndex);
    return MakeDeviceSourceString(defaultMap, inputDevices);
}

void Au3AudioDevicesProvider::checkSystemDefaultDeviceChanges()
{
    const std::string outputSelection = muse::settings()->value(PLAYBACK_DEVICE).toString();
    const std::string inputSelection = muse::settings()->value(RECORDING_DEVICE).toString();

    bool followOutput = outputSelection.empty();
    bool followInput = inputSelection.empty();

    std::string newDefaultOutput = systemDefaultOutputDevice();
    std::string newDefaultInput = systemDefaultInputDevice();

    bool outputChanged = !newDefaultOutput.empty() && newDefaultOutput != m_lastDefaultOutputDevice;
    bool inputChanged = !newDefaultInput.empty() && newDefaultInput != m_lastDefaultInputDevice;

    if (outputChanged) {
        LOGI() << "System default output changed: "
               << m_lastDefaultOutputDevice << " -> " << newDefaultOutput
               << " (follow=" << (followOutput ? "yes" : "no") << ")";
        m_lastDefaultOutputDevice = newDefaultOutput;
    }
    if (inputChanged) {
        LOGI() << "System default input changed: "
               << m_lastDefaultInputDevice << " -> " << newDefaultInput
               << " (follow=" << (followInput ? "yes" : "no") << ")";
        m_lastDefaultInputDevice = newDefaultInput;
    }

    if ((followOutput && outputChanged) || (followInput && inputChanged)) {
        const std::string prevOutputSelection = outputSelection;
        const std::string prevInputSelection = inputSelection;
        updateInputOutputDevices();
        if (followInput && inputChanged) {
            initInputChannels();
            m_inputChannelsListChanged.notify();
            m_inputChannelsChanged.notify();
        }

        const std::string currentOutputSelection = muse::settings()->value(PLAYBACK_DEVICE).toString();
        const std::string currentInputSelection = muse::settings()->value(RECORDING_DEVICE).toString();
        const bool outputSelectionChanged = prevOutputSelection != currentOutputSelection;
        const bool inputSelectionChanged = prevInputSelection != currentInputSelection;
        const bool shouldHandleDeviceChange = (followOutput && outputChanged && !outputSelectionChanged)
                                              || (followInput && inputChanged && !inputSelectionChanged);
        if (shouldHandleDeviceChange) {
            handleDeviceChange();
        }
        if (followOutput && outputChanged && !outputSelectionChanged) {
            m_audioOutputDeviceChanged.notify();
        }
        if (followInput && inputChanged && !inputSelectionChanged) {
            m_audioInputDeviceChanged.notify();
        }
    }
}

void Au3AudioDevicesProvider::rescan()
{
    DeviceManager::Instance()->Rescan();

    initHosts();
    updateInputOutputDevices();
    initInputChannels();

    m_audioApiChanged.notify();
    m_audioOutputDeviceChanged.notify();
    m_audioInputDeviceChanged.notify();
    m_inputChannelsListChanged.notify();
    m_inputChannelsChanged.notify();
}
