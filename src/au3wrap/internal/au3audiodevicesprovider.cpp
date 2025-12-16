/*
* Audacity: A Digital Audio Editor
*/

#include "containers.h"
#include "settings.h"

#include "framework/global/log.h"
#include "framework/global/realfn.h"

#include <algorithm>

#include "internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include "au3-audio-devices/DeviceManager.h"
#include "au3-audio-devices/AudioIOBase.h"
#include "au3-utility/IteratorX.h"
#include "au3-project-rate/QualitySettings.h"
#include "au3-project-rate/ProjectRate.h"

#include "au3audiodevicesprovider.h"

using namespace muse;
using namespace au::au3;

static const muse::Settings::Key AUDIO_HOST("au3wrap", "AudioIO/Host");
static const muse::Settings::Key PLAYBACK_DEVICE("au3wrap", "AudioIO/PlaybackDevice");
static const muse::Settings::Key RECORDING_DEVICE("au3wrap", "AudioIO/RecordingDevice");
static const muse::Settings::Key INPUT_CHANNELS("au3wrap", "AudioIO/RecordChannels");

static const muse::Settings::Key LATENCY_DURATION("au3wrap", "AudioIO/LatencyDuration");
static const muse::Settings::Key LATENCY_CORRECTION("au3wrap", "AudioIO/LatencyCorrection");

static const muse::Settings::Key DEFAULT_PROJECT_SAMPLE_RATE("au3wrap", "SamplingRate/DefaultProjectSampleRate");
static const muse::Settings::Key DEFAULT_PROJECT_SAMPLE_FORMAT("au3wrap", "SamplingRate/DefaultProjectSampleFormatChoice");

static const muse::Settings::Key RECORDING_SOURCE("au3wrap", "AudioIO/RecordingSource");
static const muse::Settings::Key RECORDING_SOURCE_INDEX("au3wrap", "AudioIO/RecordingSourceIndex");

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

    muse::settings()->setDefaultValue(AUDIO_HOST, muse::Val(getPreferredAudioHost(apiList())));
    const int hostIndex = DeviceManager::Instance()->GetHostIndex(currentApi());
    const auto inputDevice = DeviceManager::Instance()->GetDefaultInputDevice(hostIndex);
    const auto outputDevice = DeviceManager::Instance()->GetDefaultOutputDevice(hostIndex);

    muse::settings()->setDefaultValue(PLAYBACK_DEVICE, muse::Val(MakeDeviceSourceString(outputDevice)));
    muse::settings()->setDefaultValue(RECORDING_DEVICE, muse::Val(MakeDeviceSourceString(inputDevice)));

    muse::settings()->setDefaultValue(INPUT_CHANNELS, muse::Val(1));
    muse::settings()->setDefaultValue(LATENCY_DURATION, muse::Val(100.0));
    muse::settings()->setDefaultValue(LATENCY_CORRECTION, muse::Val(-130.0));
    muse::settings()->setDefaultValue(DEFAULT_PROJECT_SAMPLE_RATE, muse::Val(AudioIOBase::GetOptimalSupportedSampleRate()));
    muse::settings()->setDefaultValue(DEFAULT_PROJECT_SAMPLE_FORMAT,
                                      muse::Val(QualitySettings::SampleFormatSetting.Default().Internal().ToStdString()));

    muse::settings()->valueChanged(AUDIO_HOST).onReceive(nullptr, [this](const muse::Val& val) {
        updateInputOutputDevices();
        m_audioApiChanged.notify();
    });

    muse::settings()->valueChanged(PLAYBACK_DEVICE).onReceive(nullptr, [this](const muse::Val& val) {
        Au3AudioDevicesProvider::handleDeviceChange();
        m_audioOutputDeviceChanged.notify();
    });

    muse::settings()->valueChanged(RECORDING_DEVICE).onReceive(nullptr, [this](const muse::Val& val) {
        setupInputDevice(val.toString());

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
}

std::vector<std::string> Au3AudioDevicesProvider::outputDevices() const
{
    return m_outputDevices;
}

std::string Au3AudioDevicesProvider::currentOutputDevice() const
{
    return muse::settings()->value(PLAYBACK_DEVICE).toString();
}

void Au3AudioDevicesProvider::setOutputDevice(const std::string& device)
{
    if (muse::contains(m_outputDevices, device)) {
        muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(device));
    } else if (!m_outputDevices.empty()) {
        muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(m_outputDevices.front()));
    } else {
        muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val());
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

std::string Au3AudioDevicesProvider::currentInputDevice() const
{
    return muse::settings()->value(RECORDING_DEVICE).toString();
}

void Au3AudioDevicesProvider::setInputDevice(const std::string& device)
{
    if (muse::contains(m_inputDevices, device)) {
        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(device));
    } else if (!m_inputDevices.empty()) {
        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(m_inputDevices.front()));
    } else {
        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val());
    }
}

async::Notification Au3AudioDevicesProvider::inputDeviceChanged() const
{
    return m_audioInputDeviceChanged;
}

void Au3AudioDevicesProvider::handleDeviceChange()
{
    audioEngine()->handleDeviceChange();
}

std::vector<std::string> Au3AudioDevicesProvider::apiList() const
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

double Au3AudioDevicesProvider::latencyCompensation() const
{
    return muse::settings()->value(LATENCY_CORRECTION).toDouble();
}

void Au3AudioDevicesProvider::setLatencyCompensation(double newLatencyCompensation)
{
    settings()->setLocalValue(LATENCY_CORRECTION, muse::Val(newLatencyCompensation));
}

std::vector<uint64_t> Au3AudioDevicesProvider::availableSampleRateList() const
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

std::vector<std::string> Au3AudioDevicesProvider::defaultSampleFormatList() const
{
    std::vector<std::string> sampleFormatList;
    for (const auto& format : QualitySettings::SampleFormatSetting.GetSymbols().GetMsgids()) {
        sampleFormatList.push_back(format.Translation().ToStdString());
    }

    return sampleFormatList;
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
        std::string host = wxToStdSting(device.hostString);
        if (!muse::contains(m_audioApis, host)) {
            m_audioApis.push_back(host);
        }
    }

    for (auto& device : outMaps) {
        std::string host = wxToStdSting(device.hostString);
        if (!muse::contains(m_audioApis, host)) {
            m_audioApis.push_back(host);
        }
    }
}

void Au3AudioDevicesProvider::initInputChannels()
{
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    const std::string host = currentApi();
    const std::string inputDevice = currentInputDevice();

    for (const auto& device : inMaps) {
        if (device.hostString != host) {
            continue;
        }
        const auto deviceName = wxToStdSting(MakeDeviceSourceString(&device));
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
        m_inputDevices.push_back(wxToStdSting(MakeDeviceSourceString(&device)));
    }

    for (const auto& device : outputDevices) {
        if (device.hostString != currentApi()) {
            continue;
        }
        m_outputDevices.push_back(wxToStdSting(MakeDeviceSourceString(&device)));
    }

    setInputDevice(defaultInputDevice());
    setOutputDevice(defaultOutputDevice());
}

void Au3AudioDevicesProvider::setupInputDevice(const std::string& newDevice)
{
    // Release current recording device
    audioEngine()->stopMonitoring();

    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    auto host = muse::settings()->value(AUDIO_HOST).toString();

    int prevInputChannels = muse::settings()->value(INPUT_CHANNELS).toInt();

    for (const auto& device : inMaps) {
        const auto deviceName = wxToStdSting(MakeDeviceSourceString(&device));
        if (device.hostString != host || deviceName != newDevice) {
            continue;
        }

        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(newDevice));
        muse::settings()->setLocalValue(RECORDING_SOURCE_INDEX, muse::Val(device.sourceIndex));

        if (device.totalSources >= 1) {
            muse::settings()->setLocalValue(RECORDING_SOURCE, muse::Val(device.sourceString.ToStdString()));
        } else {
            muse::settings()->setLocalValue(RECORDING_SOURCE, muse::Val());
        }

        m_inputChannelsAvailable = device.numChannels;
        muse::settings()->setLocalValue(INPUT_CHANNELS, muse::Val(std::min(prevInputChannels, m_inputChannelsAvailable)));

        Au3AudioDevicesProvider::handleDeviceChange();

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
