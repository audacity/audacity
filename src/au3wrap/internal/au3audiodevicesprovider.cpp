/*
* Audacity: A Digital Audio Editor
*/

#include "settings.h"

#include "internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include "libraries/lib-audio-devices/DeviceManager.h"
#include "libraries/lib-audio-devices/AudioIOBase.h"
#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-utility/IteratorX.h"
#include "QualitySettings.h"
#include "ProjectRate.h"

#include "log.h"
#include "realfn.h"
#include <portaudio.h>

#include "au3audiodevicesprovider.h"

using namespace muse;
using namespace au::au3;

static const muse::Settings::Key AUDIO_HOST("au3wrap", "AudioIO/Host");
static const muse::Settings::Key PLAYBACK_DEVICE("au3wrap", "AudioIO/PlaybackDevice");
static const muse::Settings::Key RECORDING_DEVICE("au3wrap", "AudioIO/RecordingDevice");
static const muse::Settings::Key INPUT_CHANNELS("au3wrap", "AudioIO/RecordChannels");

static const muse::Settings::Key LATENCY_DURATION("au3wrap", "AudioIO/LatencyDuration");
static const muse::Settings::Key LATENCY_COMPENSATION("au3wrap", "AudioIO/LatencyCorrection");

static const muse::Settings::Key DEFAULT_SAMPLE_RATE("au3wrap", "SamplingRate/DefaultProjectSampleRate");
static const muse::Settings::Key SAMPLE_FORMAT("au3wrap", "SamplingRate/DefaultProjectSampleFormatChoice");

void Au3AudioDevicesProvider::init()
{
    initHosts();
    initHostDevices();

    std::string defaultHost = !audioApiList().empty() ? audioApiList().front() : "";
    muse::settings()->setDefaultValue(AUDIO_HOST, muse::Val(defaultHost));
    muse::settings()->valueChanged(AUDIO_HOST).onReceive(nullptr, [this](const muse::Val& val) {
        const std::vector<DeviceSourceMap>& outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();

        wxString newHost = wxString(val.toString());
        for (const auto& device : outMaps) {
            if (device.hostString == newHost) {
                AudioIOHost.Write(device.hostString);
                m_audioApiChanged.notify();

                updateInputOutputDevices();

                return;
            }
        }
    });

    std::string defaultOutput = defaultOutputDevice();
    muse::settings()->setDefaultValue(PLAYBACK_DEVICE, muse::Val(defaultOutput));
    muse::settings()->valueChanged(PLAYBACK_DEVICE).onReceive(nullptr, [this](const muse::Val& val) {
        AudioIOPlaybackDevice.Write(wxString::FromUTF8(val.toString()));
        Au3AudioDevicesProvider::handleDeviceChange();

        m_audioOutputDeviceChanged.notify();
    });

    std::string defaultInput = defaultInputDevice();
    muse::settings()->setDefaultValue(RECORDING_DEVICE, muse::Val(defaultInput));
    muse::settings()->valueChanged(RECORDING_DEVICE).onReceive(nullptr, [this](const muse::Val& val) {
        setupRecordingDevice(val.toString());

        m_audioInputDeviceChanged.notify();
        m_inputChannelsListChanged.notify();
        m_inputChannelsChanged.notify();
    });

    updateInputOutputDevices();
    initInputChannels();

    std::string defaultInputChannels = !inputChannelsList().empty() ? inputChannelsList().front() : "";
    muse::settings()->setDefaultValue(INPUT_CHANNELS, muse::Val(defaultInputChannels));
    muse::settings()->valueChanged(INPUT_CHANNELS).onReceive(nullptr, [this](const muse::Val& val) {
        AudioIORecordChannels.Write(val.toInt());
        m_inputChannelsChanged.notify();
    });

    muse::settings()->setDefaultValue(LATENCY_DURATION, muse::Val(100.0));
    muse::settings()->valueChanged(LATENCY_DURATION).onReceive(nullptr, [this](const muse::Val& val) {
        AudioIOLatencyDuration.Write(val.toDouble());
        m_bufferLengthChanged.notify();
    });

    muse::settings()->setDefaultValue(LATENCY_COMPENSATION, muse::Val(-130.0));
    muse::settings()->valueChanged(LATENCY_COMPENSATION).onReceive(nullptr, [this](const muse::Val& val) {
        AudioIOLatencyCorrection.Write(val.toDouble());
        m_latencyCompensationChanged.notify();
    });

    muse::settings()->setDefaultValue(DEFAULT_SAMPLE_RATE, muse::Val(AudioIOBase::GetOptimalSupportedSampleRate()));
    muse::settings()->valueChanged(DEFAULT_SAMPLE_RATE).onReceive(nullptr, [this](const muse::Val& val) {
        QualitySettings::DefaultSampleRate.Write(val.toInt());
        auto currentProject = globalContext()->currentProject();
        if (currentProject) {
            Au3Project* project = reinterpret_cast<Au3Project*>(currentProject->au3ProjectPtr());
            ::ProjectRate::Get(*project).SetRate(val.toInt());
        }

        m_defaultSampleRateChanged.notify();
    });

    muse::settings()->setDefaultValue(SAMPLE_FORMAT, muse::Val(QualitySettings::SampleFormatSetting.Default().Internal().ToStdString()));
    muse::settings()->valueChanged(SAMPLE_FORMAT).onReceive(nullptr, [this](const muse::Val& val) {
        QualitySettings::SampleFormatSetting.Write(wxString(val.toString()));
        m_defaultSampleFormatChanged.notify();
    });
}

std::vector<std::string> Au3AudioDevicesProvider::audioOutputDevices() const
{
    std::vector<std::string> outputDevices;
    const std::vector<DeviceSourceMap>& outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();
    auto host = AudioIOHost.Read();

    for (const auto& device : outMaps) {
        if (device.hostString == host) {
            outputDevices.push_back(wxToStdSting(MakeDeviceSourceString(&device)));
        }
    }

    return outputDevices;
}

std::string Au3AudioDevicesProvider::currentAudioOutputDevice() const
{
    return muse::settings()->value(PLAYBACK_DEVICE).toString();
}

void Au3AudioDevicesProvider::setAudioOutputDevice(const std::string& deviceName)
{
    muse::settings()->setSharedValue(PLAYBACK_DEVICE, muse::Val(deviceName));
}

async::Notification Au3AudioDevicesProvider::audioOutputDeviceChanged() const
{
    return m_audioOutputDeviceChanged;
}

std::vector<std::string> Au3AudioDevicesProvider::audioInputDevices() const
{
    std::vector<std::string> inputDevices;
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    auto host = AudioIOHost.Read();

    for (const auto& device : inMaps) {
        if (device.hostString == host) {
            inputDevices.push_back(wxToStdSting(MakeDeviceSourceString(&device)));
        }
    }

    return inputDevices;
}

std::string Au3AudioDevicesProvider::currentAudioInputDevice() const
{
    return muse::settings()->value(RECORDING_DEVICE).toString();
}

void Au3AudioDevicesProvider::setAudioInputDevice(const std::string& deviceName)
{
    muse::settings()->setSharedValue(RECORDING_DEVICE, muse::Val(deviceName));
}

async::Notification Au3AudioDevicesProvider::audioInputDeviceChanged() const
{
    return m_audioInputDeviceChanged;
}

void Au3AudioDevicesProvider::handleDeviceChange()
{
    AudioIO::Get()->HandleDeviceChange();
}

std::vector<std::string> Au3AudioDevicesProvider::audioApiList() const
{
    const std::vector<DeviceSourceMap>& outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();
    std::vector<std::string> hosts;

    for (const auto& device : outMaps) {
        std::string host = wxToStdSting(device.hostString);
        if (std::find(hosts.begin(), hosts.end(), host) == hosts.end()) {
            hosts.push_back(host);
        }
    }
    return hosts;
}

std::string Au3AudioDevicesProvider::currentAudioApi() const
{
    return muse::settings()->value(AUDIO_HOST).toString();
}

void Au3AudioDevicesProvider::setAudioApi(const std::string& audioApi)
{
    muse::settings()->setSharedValue(AUDIO_HOST, muse::Val(audioApi));
}

std::vector<std::string> Au3AudioDevicesProvider::inputChannelsList() const
{
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    auto host = AudioIOHost.Read();
    auto device = AudioIORecordingDevice.Read();
    auto source = AudioIORecordingSource.Read();
    long newChannels = 0;

    auto oldChannels = AudioIORecordChannels.Read();

    std::vector<std::string> names;
    for (auto& dev: inMaps) {
        if (source == dev.sourceString
            && device == dev.deviceString
            && host == dev.hostString) {
            // add one selection for each channel of this source
            for (size_t j = 0; j < (unsigned int)dev.numChannels; j++) {
                wxString name;

                if (j == 0) {
                    name = _("1 (Mono) Recording Channel");
                } else if (j == 1) {
                    name = _("2 (Stereo) Recording Channels");
                } else {
                    name = wxString::Format(wxT("%d"), (int)j + 1);
                }
                names.push_back(name.ToStdString());
            }
        }
    }

    return names;
}

std::string Au3AudioDevicesProvider::currentInputChannels() const
{
    int currentRecordChannels = muse::settings()->value(INPUT_CHANNELS).toInt();
    if (inputChannelsList().empty()) {
        return std::string();
    }

    wxString name;
    if (currentRecordChannels == 1) {
        name = _("1 (Mono) Recording Channel");
    } else if (currentRecordChannels == 2) {
        name = _("2 (Stereo) Recording Channels");
    } else {
        name = wxString::Format(wxT("%d"), currentRecordChannels);
    }

    return name.ToStdString();
}

void Au3AudioDevicesProvider::setInputChannels(const std::string& newChannels)
{
    std::optional<int> channelsToWrite;
    for (const auto& channels : inputChannelsList()) {
        if (channels == newChannels) {
            if (channels == _("1 (Mono) Recording Channel")) {
                channelsToWrite = 1;
            } else if (channels == _("2 (Stereo) Recording Channels")) {
                channelsToWrite = 2;
            } else {
                channelsToWrite = std::stoi(channels);
            }
            break;
        }
    }
    muse::settings()->setSharedValue(INPUT_CHANNELS, muse::Val(channelsToWrite.value()));
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
    muse::settings()->setSharedValue(LATENCY_DURATION, muse::Val(newBufferLength));
}

double Au3AudioDevicesProvider::latencyCompensation() const
{
    return muse::settings()->value(LATENCY_COMPENSATION).toDouble();
}

void Au3AudioDevicesProvider::setLatencyCompensation(double newLatencyCompensation)
{
    settings()->setSharedValue(LATENCY_COMPENSATION, muse::Val(newLatencyCompensation));
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
    return muse::settings()->value(DEFAULT_SAMPLE_RATE).toInt();
}

void Au3AudioDevicesProvider::setDefaultSampleRate(uint64_t newRate)
{
    settings()->setSharedValue(DEFAULT_SAMPLE_RATE, muse::Val(static_cast<int>(newRate)));
}

void Au3AudioDevicesProvider::setDefaultSampleFormat(const std::string& format)
{
    for (const auto& symbol : QualitySettings::SampleFormatSetting.GetSymbols()) {
        if (format == symbol.Msgid().Translation()) {
            settings()->setSharedValue(SAMPLE_FORMAT, muse::Val(symbol.Internal().ToStdString()));
        }
    }
}

async::Notification Au3AudioDevicesProvider::defaultSampleFormatChanged() const
{
    return m_defaultSampleFormatChanged;
}

std::string Au3AudioDevicesProvider::defaultSampleFormat() const
{
    auto currentFormat = muse::settings()->value(SAMPLE_FORMAT).toString();
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

async::Notification Au3AudioDevicesProvider::inputChannelsListChanged() const
{
    return m_inputChannelsListChanged;
}

async::Notification Au3AudioDevicesProvider::inputChannelsChanged() const
{
    return m_inputChannelsChanged;
}

async::Notification Au3AudioDevicesProvider::audioApiChanged() const
{
    return m_audioApiChanged;
}

void Au3AudioDevicesProvider::initHosts()
{
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    const std::vector<DeviceSourceMap>& outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();

    wxArrayString hosts;

    // go over our lists add the host to the list if it isn't there yet

    for (auto& device : inMaps) {
        if (!make_iterator_range(hosts).contains(device.hostString)) {
            hosts.push_back(device.hostString);
        }
    }

    for (auto& device : outMaps) {
        if (!make_iterator_range(hosts).contains(device.hostString)) {
            hosts.push_back(device.hostString);
        }
    }

    mHost.Set(std::move(hosts));
}

void Au3AudioDevicesProvider::initHostDevices()
{
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    const std::vector<DeviceSourceMap>& outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();

    //read what is in the prefs
    auto host = AudioIOHost.Read();
    int foundHostIndex = -1;

    // if the host is not in the hosts combo then we rescanned.
    // set it to blank so we search for another host.
    if (mHost.Find(host) < 0) {
        host = wxT("");
    }

    // Try to find a hostIndex, among either inputs or outputs, assumed to be
    // unique among the union of the set of input and output devices
    for (auto& device : outMaps) {
        if (device.hostString == host) {
            foundHostIndex = device.hostIndex;
            break;
        }
    }

    if (foundHostIndex == -1) {
        for (auto& device : inMaps) {
            if (device.hostString == host) {
                foundHostIndex = device.hostIndex;
                break;
            }
        }
    }

    // If no host was found based on the prefs device host, load the first available one
    if (foundHostIndex == -1) {
        if (outMaps.size()) {
            foundHostIndex = outMaps[0].hostIndex;
        } else if (inMaps.size()) {
            foundHostIndex = inMaps[0].hostIndex;
        }
    }

    // Make sure in/out are clear in case no host was found
    mInput.Clear();
    mOutput.Clear();

    // If we still have no host it means no devices, in which case do nothing.
    if (foundHostIndex == -1) {
        return;
    }

    // Repopulate the Input/Output device list available to the user
    wxArrayStringEx mInputDeviceNames;
    for (size_t i = 0; i < inMaps.size(); ++i) {
        auto& device = inMaps[i];
        if (foundHostIndex == device.hostIndex) {
            mInputDeviceNames.push_back(MakeDeviceSourceString(&device));
            if (host.empty()) {
                host = device.hostString;
                AudioIOHost.Write(host);
                mHost.Set(host);
            }
        }
    }
    mInput.Set(std::move(mInputDeviceNames));

    wxArrayStringEx mOutputDeviceNames;
    for (size_t i = 0; i < outMaps.size(); ++i) {
        auto& device = outMaps[i];
        if (foundHostIndex == device.hostIndex) {
            mOutputDeviceNames.push_back(MakeDeviceSourceString(&device));
            if (host.empty()) {
                host = device.hostString;
                AudioIOHost.Write(host);
                mHost.Set(host);
            }
        }
    }
    mOutput.Set(std::move(mOutputDeviceNames));

    gPrefs->Flush();

    // The setting of the Device is left up to menu handlers
}

void Au3AudioDevicesProvider::initInputChannels()
{
    //!NOTE: Copied from AudioSetupToolBar::FillInputChannels

    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    auto host = AudioIOHost.Read();
    auto device = AudioIORecordingDevice.Read();
    auto source = AudioIORecordingSource.Read();
    long newChannels = 0;

    auto oldChannels = AudioIORecordChannels.Read();

    wxArrayStringEx names;
    for (auto& dev: inMaps) {
        if (source == dev.sourceString
            && device == dev.deviceString
            && host == dev.hostString) {
            // add one selection for each channel of this source
            for (size_t j = 0; j < (unsigned int)dev.numChannels; j++) {
                wxString name;

                if (j == 0) {
                    name = _("1 (Mono) Recording Channel");
                } else if (j == 1) {
                    name = _("2 (Stereo) Recording Channels");
                } else {
                    name = wxString::Format(wxT("%d"), (int)j + 1);
                }
                names.push_back(name);
            }
            newChannels = dev.numChannels;
            if (oldChannels <= newChannels && oldChannels >= 1) {
                newChannels = oldChannels;
            }
            AudioIORecordChannels.Write(newChannels);
            break;
        }
    }
    mInputChannels.Set(std::move(names));
    if (newChannels >= 1) {
        // Correct to 0-based index in choice
        mInputChannels.Set(newChannels - 1);
    }

    m_inputChannelsChanged.notify();
}

void Au3AudioDevicesProvider::updateInputOutputDevices()
{
    std::string input = defaultInputDevice();
    std::string output = defaultOutputDevice();

    if (!input.empty()) {
        setAudioInputDevice(input);
    }

    if (!output.empty()) {
        setAudioOutputDevice(output);
    }
}

void Au3AudioDevicesProvider::setupRecordingDevice(const std::string& newDevice)
{
    std::vector<std::string> inputDevices;
    const std::vector<DeviceSourceMap>& inMaps = DeviceManager::Instance()->GetInputDeviceMaps();
    auto host = AudioIOHost.Read();

    long newChannels = 0;
    auto oldChannels = AudioIORecordChannels.Read();

    wxArrayStringEx names;
    for (const auto& device : inMaps) {
        if (device.hostString == host && wxToStdSting(MakeDeviceSourceString(&device)) == newDevice) {
            AudioIORecordingDevice.Write(wxString::FromUTF8(newDevice));
            AudioIORecordingSourceIndex.Write(device.sourceIndex);
            if (device.totalSources >= 1) {
                AudioIORecordingSource.Write(device.sourceString);
            } else {
                AudioIORecordingSource.Reset();
            }

            for (size_t j = 0; j < (unsigned int)device.numChannels; j++) {
                wxString name;

                if (j == 0) {
                    name = _("1 (Mono) Recording Channel");
                } else if (j == 1) {
                    name = _("2 (Stereo) Recording Channels");
                } else {
                    name = wxString::Format(wxT("%d"), (int)j + 1);
                }
                names.push_back(name);
            }
            newChannels = device.numChannels;
            if (oldChannels <= newChannels && oldChannels >= 1) {
                newChannels = oldChannels;
            }
            AudioIORecordChannels.Write(newChannels);

            Au3AudioDevicesProvider::handleDeviceChange();
        }
    }

    mInputChannels.Set(std::move(names));
    if (newChannels >= 1) {
        // Correct to 0-based index in choice
        mInputChannels.Set(newChannels - 1);
    }
}

std::string Au3AudioDevicesProvider::defaultOutputDevice()
{
    // get api index
    if (audioApiList().size() < 1) {
        return "";
    }

    int index = AudioIO::GetHostIndex(currentAudioApi());

    const auto currentOutputDevice = currentAudioOutputDevice();
    const auto outputDevices = audioOutputDevices();
    if (!muse::contains(outputDevices, currentOutputDevice) && !outputDevices.empty()) {
        // choose the default on this API, as defined by PortAudio
        if (index < 0) {
            return outputDevices[0];
        } else {
            DeviceSourceMap* defaultMap = DeviceManager::Instance()->GetDefaultOutputDevice(index);
            if (defaultMap) {
                return wxToStdSting(MakeDeviceSourceString(defaultMap));
            }
        }
    }

    return "";
}

std::string Au3AudioDevicesProvider::defaultInputDevice()
{
    // get api index
    if (audioApiList().size() < 1) {
        return "";
    }

    int index = AudioIO::GetHostIndex(currentAudioApi());

    const auto currentInputDevice = currentAudioInputDevice();
    const auto inputDevices = audioInputDevices();
    if (!muse::contains(inputDevices, currentInputDevice) && !inputDevices.empty()) {
        // choose the default on this API, as defined by PortAudio
        if (index < 0) {
            return inputDevices[0];
        } else {
            DeviceSourceMap* defaultMap = DeviceManager::Instance()->GetDefaultInputDevice(index);
            if (defaultMap) {
                return wxToStdSting(MakeDeviceSourceString(defaultMap));
            }
        }
    }

    return "";
}
