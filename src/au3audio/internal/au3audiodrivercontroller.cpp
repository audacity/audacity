/*
* Audacity: A Digital Audio Editor
*/

#include "au3audiodrivercontroller.h"

#include <algorithm>
#include <exception>
#include <limits>
#include <stdexcept>

#include "framework/global/containers.h"
#include "framework/global/defer.h"
#include "framework/global/log.h"
#include "framework/global/realfn.h"
#include "framework/global/settings.h"

#include "audio/iaudiostreamsuspender.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-audio-devices/AudioIOBase.h"
#include "au3-audio-devices/DeviceManager.h"
#include "au3-project-rate/ProjectRate.h"
#include "au3-project-rate/QualitySettings.h"

using namespace muse;
using namespace au::audio;
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

std::string preferredAudioHost(const std::vector<std::string>& hosts)
{
    if (hosts.empty()) {
        return {};
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

    for (const auto candidate : preferred) {
        if (muse::contains(hosts, std::string(candidate))) {
            return candidate;
        }
    }
    return hosts.front();
}

std::string displaySampleFormat(const std::string& internal)
{
    for (const auto& symbol : QualitySettings::SampleFormatSetting.GetSymbols()) {
        if (internal == symbol.Internal()) {
            return symbol.Msgid().translated().toStdString();
        }
    }
    return {};
}

std::string internalSampleFormat(const std::string& display)
{
    for (const auto& symbol : QualitySettings::SampleFormatSetting.GetSymbols()) {
        if (display == symbol.Msgid().translated().toStdString()) {
            return symbol.Internal().ToStdString();
        }
    }
    return {};
}

bool restoreSafely(const AudioStreamRestorer& restoreStream) noexcept
{
    if (!restoreStream) {
        return true;
    }
    try {
        return restoreStream();
    } catch (...) {
        return false;
    }
}

bool changesRouting(const AudioConfigurationDelta& delta)
{
    return delta.contains(AudioConfigurationField::Api)
           || delta.contains(AudioConfigurationField::OutputDevice)
           || delta.contains(AudioConfigurationField::InputDevice);
}

std::string systemDefaultOutputDevice(const std::string& api)
{
    const auto& maps = DeviceManager::Instance()->GetOutputDeviceMaps();
    const int hostIndex = DeviceManager::Instance()->GetHostIndex(api);
    return MakeDeviceSourceString(DeviceManager::Instance()->GetDefaultOutputDevice(hostIndex), maps);
}

std::string systemDefaultInputDevice(const std::string& api)
{
    const auto& maps = DeviceManager::Instance()->GetInputDeviceMaps();
    const int hostIndex = DeviceManager::Instance()->GetHostIndex(api);
    return MakeDeviceSourceString(DeviceManager::Instance()->GetDefaultInputDevice(hostIndex), maps);
}

const DeviceSourceMap* findDevice(const std::vector<DeviceSourceMap>& maps,
                                  const std::string& api, const std::string& device)
{
    if (device.empty()) {
        return nullptr;
    }
    const auto it = std::find_if(maps.begin(), maps.end(), [&](const DeviceSourceMap& candidate) {
        return candidate.hostString == api && MakeDeviceSourceString(&candidate, maps) == device;
    });
    return it == maps.end() ? nullptr : &*it;
}

std::string effectiveOutputDevice(const std::string& api, const AudioDeviceSelection& selection)
{
    const auto& maps = DeviceManager::Instance()->GetOutputDeviceMaps();
    if (selection && findDevice(maps, api, selection.value())) {
        return selection.value();
    }
    return systemDefaultOutputDevice(api);
}

std::string effectiveInputDevice(const std::string& api, const AudioDeviceSelection& selection)
{
    const auto& maps = DeviceManager::Instance()->GetInputDeviceMaps();
    if (selection && findDevice(maps, api, selection.value())) {
        return selection.value();
    }
    return systemDefaultInputDevice(api);
}

AudioDeviceSelection selectionFromSetting(const std::string& value)
{
    if (value.empty()) {
        return std::nullopt;
    }
    return value;
}
}

void Au3AudioDriverController::init()
{
    initDefaults();

    const auto availableApis = apis();

    const auto configuredApi = settings()->value(AUDIO_HOST).toString();
    if (!availableApis.empty() && !muse::contains(availableApis, configuredApi)) {
        settings()->setLocalValue(AUDIO_HOST, muse::Val(preferredAudioHost(availableApis)));
    }

    const auto api = settings()->value(AUDIO_HOST).toString();
    const auto inputDevice = selectionFromSetting(settings()->value(RECORDING_DEVICE).toString());
    refreshInputDeviceSettings(api, inputDevice);
    const auto availableChannels = inputChannelsAvailable(api, inputDevice);
    const auto inputChannels
        = availableChannels > 0
          ? std::clamp(settings()->value(INPUT_CHANNELS).toInt(), 1,
                       availableChannels)
          : 0;
    settings()->setLocalValue(INPUT_CHANNELS, muse::Val(inputChannels));
    m_configuration = configurationFromSettings();

    systemAudioDevicesListener()->systemDevicesChanged().onNotify(this, [this]() {
        onSystemDevicesChanged();
    });

    if (audioEngine()) {
        audioEngine()->streamStopped().onNotify(this, [this]() {
            if (m_pendingSystemDevicesChange) {
                m_pendingSystemDevicesChange = false;
                onSystemDevicesChanged();
            }
        });
    }
}

void Au3AudioDriverController::onSystemDevicesChanged()
{
    if (audioEngine() && audioEngine()->isBusy()) {
        m_pendingSystemDevicesChange = true;
        return;
    }

    const auto before = m_configuration;
    const std::string prevOutputDevice = effectiveOutputDevice(before.api, before.outputDevice);
    const std::string prevInputDevice = effectiveInputDevice(before.api, before.inputDevice);

    const auto result = rescan();
    if (!result.succeeded()) {
        LOGW() << "device rescan after a system change failed, status: " << static_cast<int>(result.status);
        m_pendingSystemDevicesChange = true;
        return;
    }

    const auto after = m_configuration;
    const std::string outputDevice = effectiveOutputDevice(after.api, after.outputDevice);
    const std::string inputDevice = effectiveInputDevice(after.api, after.inputDevice);

    if (outputDevice != prevOutputDevice) {
        m_usedOutputDeviceChanged.send(outputDevice);
    }
    if (inputDevice != prevInputDevice) {
        m_usedInputDeviceChanged.send(inputDevice);
    }
}

void Au3AudioDriverController::initDefaults()
{
    const auto availableApis = apis();
    settings()->setDefaultValue(AUDIO_HOST, muse::Val(preferredAudioHost(availableApis)));

    settings()->setDefaultValue(PLAYBACK_DEVICE, muse::Val(std::string()));
    settings()->setDefaultValue(RECORDING_DEVICE, muse::Val(std::string()));
    settings()->setDefaultValue(INPUT_CHANNELS, muse::Val(1));
    settings()->setDefaultValue(LATENCY_DURATION, muse::Val(100.0));
    settings()->setDefaultValue(AUTOMATIC_LATENCY_COMPENSATION, muse::Val(false));
    settings()->setDefaultValue(LATENCY_COMPENSATION, muse::Val(-130.0));
    settings()->setDefaultValue(ASIO_USE_DEVICE_SAMPLE_RATE, muse::Val(true));
    settings()->setDefaultValue(DEFAULT_PROJECT_SAMPLE_RATE, muse::Val(AudioIOBase::GetOptimalSupportedSampleRate()));
    settings()->setDefaultValue(DEFAULT_PROJECT_SAMPLE_FORMAT,
                                muse::Val(QualitySettings::SampleFormatSetting.Default().Internal().ToStdString()));
}

AudioConfiguration Au3AudioDriverController::configurationFromSettings() const
{
    AudioConfiguration result;
    result.api = settings()->value(AUDIO_HOST).toString();
    result.outputDevice = selectionFromSetting(settings()->value(PLAYBACK_DEVICE).toString());
    result.inputDevice = selectionFromSetting(settings()->value(RECORDING_DEVICE).toString());
    result.inputChannels = settings()->value(INPUT_CHANNELS).toInt();
    result.bufferLength = settings()->value(LATENCY_DURATION).toDouble();
    result.automaticLatencyCompensation = settings()->value(AUTOMATIC_LATENCY_COMPENSATION).toBool();
    result.latencyCompensation = settings()->value(LATENCY_COMPENSATION).toDouble();
    result.defaultSampleRate = settings()->value(DEFAULT_PROJECT_SAMPLE_RATE).toInt();
    result.defaultSampleFormat = displaySampleFormat(settings()->value(DEFAULT_PROJECT_SAMPLE_FORMAT).toString());
    result.asioUseDeviceSampleRate = settings()->value(ASIO_USE_DEVICE_SAMPLE_RATE).toBool();
    return result;
}

AudioConfiguration Au3AudioDriverController::configuration() const
{
    return m_configuration;
}

muse::async::Channel<AudioConfigurationDelta> Au3AudioDriverController::configurationChanged() const
{
    return m_configurationChanged;
}

std::vector<std::string> Au3AudioDriverController::apis() const
{
    std::vector<std::string> result;
    const auto appendHosts = [&result](const auto& devices) {
        for (const auto& device : devices) {
            const auto host = wxToStdString(device.hostString);
            if (!muse::contains(result, host)) {
                result.push_back(host);
            }
        }
    };
    appendHosts(DeviceManager::Instance()->GetInputDeviceMaps());
    appendHosts(DeviceManager::Instance()->GetOutputDeviceMaps());
    return result;
}

std::vector<std::string> Au3AudioDriverController::outputDevices() const
{
    return outputDevices(m_configuration.api);
}

std::vector<std::string> Au3AudioDriverController::outputDevices(const std::string& api) const
{
    const auto& maps = DeviceManager::Instance()->GetOutputDeviceMaps();
    std::vector<std::string> result;
    for (const auto& device : maps) {
        if (device.hostString == api) {
            result.push_back(MakeDeviceSourceString(&device, maps));
        }
    }
    return result;
}

std::string Au3AudioDriverController::systemDefaultOutputDevice(const std::string& api) const
{
    return ::systemDefaultOutputDevice(api);
}

std::vector<std::string> Au3AudioDriverController::inputDevices() const
{
    return inputDevices(m_configuration.api);
}

std::vector<std::string> Au3AudioDriverController::inputDevices(const std::string& api) const
{
    const auto& maps = DeviceManager::Instance()->GetInputDeviceMaps();
    std::vector<std::string> result;
    for (const auto& device : maps) {
        if (device.hostString == api) {
            result.push_back(MakeDeviceSourceString(&device, maps));
        }
    }
    return result;
}

std::string Au3AudioDriverController::systemDefaultInputDevice(const std::string& api) const
{
    return ::systemDefaultInputDevice(api);
}

int Au3AudioDriverController::inputChannelsAvailable() const
{
    return inputChannelsAvailable(m_configuration.api,
                                  m_configuration.inputDevice);
}

int Au3AudioDriverController::inputChannelsAvailable(const std::string& api, const AudioDeviceSelection& inputDevice) const
{
    const auto& maps = DeviceManager::Instance()->GetInputDeviceMaps();
    const DeviceSourceMap* device = findDevice(maps, api, effectiveInputDevice(api, inputDevice));
    return device ? device->numChannels : 0;
}

std::vector<uint64_t> Au3AudioDriverController::sampleRates() const
{
    std::vector<uint64_t> rates;
    for (int i = 0; i < AudioIOBase::NumStandardRates; ++i) {
        rates.push_back(AudioIOBase::StandardRates[i]);
    }
    return rates;
}

std::vector<std::string> Au3AudioDriverController::sampleFormats() const
{
    std::vector<std::string> formats;
    for (const auto& format : QualitySettings::SampleFormatSetting.GetSymbols().GetMsgids()) {
        formats.push_back(format.translated().toStdString());
    }
    return formats;
}

std::optional<AudioConfiguration> Au3AudioDriverController::normalizedConfiguration(
    const AudioConfiguration& current, const AudioConfigurationChange& change) const
{
    AudioConfiguration result = current;

    if (change.api) {
        if (!muse::contains(apis(), *change.api)) {
            return std::nullopt;
        }
        result.api = *change.api;
    }

    result.outputDevice = change.outputDevice.value_or(result.outputDevice);
    result.inputDevice = change.inputDevice.value_or(result.inputDevice);

    const int channels = inputChannelsAvailable(result.api, result.inputDevice);
    if (channels > 0) {
        result.inputChannels = std::clamp(change.inputChannels.value_or(result.inputChannels), 1, channels);
    } else {
        result.inputChannels = 0;
    }

    if (change.bufferLength) {
        result.bufferLength = muse::RealIsEqualOrMore(*change.bufferLength, 0.0)
                              ? *change.bufferLength : settings()->defaultValue(LATENCY_DURATION).toDouble();
    }
    if (change.automaticLatencyCompensation) {
        result.automaticLatencyCompensation = *change.automaticLatencyCompensation;
    }
    if (change.latencyCompensation) {
        result.latencyCompensation = *change.latencyCompensation;
    }
    if (change.defaultSampleRate) {
        // Custom rates are valid, but the legacy setting is an int.
        if (*change.defaultSampleRate == 0
            || *change.defaultSampleRate > static_cast<uint64_t>(std::numeric_limits<int>::max())) {
            return std::nullopt;
        }
        result.defaultSampleRate = *change.defaultSampleRate;
    }
    if (change.defaultSampleFormat) {
        if (!muse::contains(sampleFormats(), *change.defaultSampleFormat)) {
            return std::nullopt;
        }
        result.defaultSampleFormat = *change.defaultSampleFormat;
    }
    if (change.asioUseDeviceSampleRate) {
        result.asioUseDeviceSampleRate = *change.asioUseDeviceSampleRate;
    }
    return result;
}

AudioConfigurationDelta Au3AudioDriverController::makeDelta(const AudioConfiguration& before,
                                                            const AudioConfiguration& after) const
{
    AudioConfigurationDelta delta;
    const auto add = [&delta](AudioConfigurationField field) { delta.fields |= fieldMask(field); };
    if (before.api != after.api) {
        add(AudioConfigurationField::Api);
    }
    if (before.outputDevice != after.outputDevice) {
        add(AudioConfigurationField::OutputDevice);
    }
    if (before.inputDevice != after.inputDevice) {
        add(AudioConfigurationField::InputDevice);
    }
    if (before.inputChannels != after.inputChannels) {
        add(AudioConfigurationField::InputChannels);
    }
    if (!muse::RealIsEqual(before.bufferLength, after.bufferLength)) {
        add(AudioConfigurationField::BufferLength);
    }
    if (before.automaticLatencyCompensation != after.automaticLatencyCompensation) {
        add(AudioConfigurationField::AutomaticLatencyCompensation);
    }
    if (!muse::RealIsEqual(before.latencyCompensation, after.latencyCompensation)) {
        add(AudioConfigurationField::LatencyCompensation);
    }
    if (before.defaultSampleRate != after.defaultSampleRate) {
        add(AudioConfigurationField::DefaultSampleRate);
    }
    if (before.defaultSampleFormat != after.defaultSampleFormat) {
        add(AudioConfigurationField::DefaultSampleFormat);
    }
    if (before.asioUseDeviceSampleRate != after.asioUseDeviceSampleRate) {
        add(AudioConfigurationField::AsioUseDeviceSampleRate);
    }
    return delta;
}

bool Au3AudioDriverController::streamNeedsSuspension(
    const AudioConfigurationDelta& delta, uint64_t defaultSampleRate,
    const muse::modularity::ContextPtr& requester,
    const std::optional<AudioStreamDescriptor>& stream) const
{
    if (!stream) {
        return false;
    }

    // Routing and the ASIO flag invalidate the process-wide PortAudio state, so
    // any stream must reopen. A buffer-length change would only take effect on
    // the next stream; reopening even another window's stream is a deliberate
    // policy so the new value is audible immediately.
    const bool routing = changesRouting(delta);
    const bool reopenAll = routing || delta.contains(AudioConfigurationField::BufferLength)
                           || delta.contains(AudioConfigurationField::AsioUseDeviceSampleRate);
    if (reopenAll) {
        return true;
    }

    // Requester-scoped changes must not interrupt another context's stream.
    if (contextForProject(stream->ownerProject) != requester) {
        return false;
    }

    const bool sampleRateChanged
        = delta.contains(AudioConfigurationField::DefaultSampleRate)
          && !muse::RealIsEqual(stream->sampleRate, static_cast<double>(defaultSampleRate));
    switch (stream->kind) {
    case AudioStreamKind::Playback:
        return false;
    case AudioStreamKind::Monitoring:
        return delta.contains(AudioConfigurationField::InputChannels)
               || sampleRateChanged;
    case AudioStreamKind::Recording:
        return delta.contains(AudioConfigurationField::InputChannels)
               || delta.contains(AudioConfigurationField::AutomaticLatencyCompensation)
               || delta.contains(AudioConfigurationField::LatencyCompensation)
               || sampleRateChanged;
    }
    return false;
}

muse::modularity::ContextPtr Au3AudioDriverController::contextForProject(const AudacityProject* project) const
{
    if (!project || !application()) {
        return {};
    }
    for (const auto& context : application()->contexts()) {
        const auto globalContext = muse::modularity::ioc(context)->resolve<au::context::IGlobalContext>("au3audio");
        const auto candidate = globalContext ? globalContext->currentProject() : nullptr;
        if (candidate && reinterpret_cast<AudacityProject*>(candidate->au3ProjectPtr()) == project) {
            return context;
        }
    }
    return {};
}

AudacityProject* Au3AudioDriverController::projectForContext(
    const muse::modularity::ContextPtr& context) const
{
    if (!context) {
        return nullptr;
    }
    const auto globalContext
        = muse::modularity::ioc(context)->resolve<au::context::IGlobalContext>(
              "au3audio");
    const auto project
        = globalContext ? globalContext->currentProject() : nullptr;
    return project ? reinterpret_cast<AudacityProject*>(project->au3ProjectPtr())
           : nullptr;
}

AudioStreamRestorer Au3AudioDriverController::suspend(const AudioStreamDescriptor& stream) const
{
    const auto ownerContext = contextForProject(stream.ownerProject);
    if (!ownerContext) {
        return {};
    }
    const auto suspender = muse::modularity::ioc(ownerContext)->resolve<IAudioStreamSuspender>("au3audio");
    return suspender ? suspender->suspendForAudioConfiguration(stream.kind) : nullptr;
}

AudioStreamRestorer Au3AudioDriverController::suspendOrForceStop(const AudioStreamDescriptor& stream) const
{
    if (auto restoreStream = suspend(stream)) {
        return restoreStream;
    }
    // No context resolves the stream's owner (e.g. its window is closing), so
    // no transport can suspend and later restore it. Force-stop the stream at
    // the engine level instead: an orphaned stream must not make the audio
    // settings unchangeable. There is no transport state to restore afterwards.
    if (!audioEngine()) {
        return {};
    }
    audioEngine()->stopStream();
    if (audioEngine()->currentStream()) {
        return {};
    }
    return [] { return true; };
}

void Au3AudioDriverController::writeConfiguration(const AudioConfiguration& value,
                                                  const AudioConfigurationDelta& delta,
                                                  const muse::modularity::ContextPtr& requester)
{
    if (delta.contains(AudioConfigurationField::Api)) {
        settings()->setLocalValue(AUDIO_HOST, muse::Val(value.api));
    }
    if (delta.contains(AudioConfigurationField::OutputDevice)) {
        settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(value.outputDevice.value_or(std::string())));
    }
    if (delta.contains(AudioConfigurationField::InputDevice)) {
        settings()->setLocalValue(RECORDING_DEVICE, muse::Val(value.inputDevice.value_or(std::string())));
    }
    if (delta.contains(AudioConfigurationField::InputChannels)) {
        settings()->setLocalValue(INPUT_CHANNELS, muse::Val(value.inputChannels));
    }
    if (delta.contains(AudioConfigurationField::BufferLength)) {
        settings()->setLocalValue(LATENCY_DURATION, muse::Val(value.bufferLength));
    }
    if (delta.contains(AudioConfigurationField::AutomaticLatencyCompensation)) {
        settings()->setLocalValue(AUTOMATIC_LATENCY_COMPENSATION, muse::Val(value.automaticLatencyCompensation));
    }
    if (delta.contains(AudioConfigurationField::LatencyCompensation)) {
        settings()->setLocalValue(LATENCY_COMPENSATION, muse::Val(value.latencyCompensation));
    }
    if (delta.contains(AudioConfigurationField::AsioUseDeviceSampleRate)) {
        settings()->setLocalValue(ASIO_USE_DEVICE_SAMPLE_RATE, muse::Val(value.asioUseDeviceSampleRate));
    }
    if (delta.contains(AudioConfigurationField::DefaultSampleFormat)) {
        const auto internal = internalSampleFormat(value.defaultSampleFormat);
        settings()->setLocalValue(DEFAULT_PROJECT_SAMPLE_FORMAT, muse::Val(internal));
        QualitySettings::SampleFormatSetting.Write(wxString(internal));
    }
    if (delta.contains(AudioConfigurationField::DefaultSampleRate)) {
        settings()->setLocalValue(DEFAULT_PROJECT_SAMPLE_RATE, muse::Val(static_cast<int>(value.defaultSampleRate)));
        // Deliberate: beyond being the default for new projects, the new rate
        // retargets the requester's open project (and only that one — other
        // windows keep the rate they were opened with).
        if (auto project = projectForContext(requester)) {
            ::ProjectRate::Get(*project).SetRate(value.defaultSampleRate);
        }
    }
    if (delta.contains(AudioConfigurationField::InputDevice) || delta.contains(AudioConfigurationField::Api)) {
        refreshInputDeviceSettings(value.api, value.inputDevice);
    }
    m_configuration = value;
}

bool Au3AudioDriverController::rollbackAndRestore(const AudioConfiguration& before,
                                                  const AudioConfiguration& attempted,
                                                  const muse::modularity::ContextPtr& requester,
                                                  std::optional<double> requesterProjectRate,
                                                  bool refreshDeviceState,
                                                  bool writeStarted,
                                                  const AudioStreamRestorer& restoreStream) noexcept
{
    bool rollbackSucceeded = true;
    if (writeStarted) {
        try {
            writeConfiguration(before, makeDelta(attempted, before), {});
            if (refreshDeviceState && audioEngine()) {
                audioEngine()->handleDeviceChange();
            }
        } catch (...) {
            rollbackSucceeded = false;
        }
    }

    if (requesterProjectRate) {
        try {
            if (auto project = projectForContext(requester)) {
                ::ProjectRate::Get(*project).SetRate(*requesterProjectRate);
            }
        } catch (...) {
            rollbackSucceeded = false;
        }
    }

    if (!rollbackSucceeded) {
        // Do not resume after a partial rollback.
        return false;
    }
    return restoreSafely(restoreStream);
}

void Au3AudioDriverController::publish(const AudioConfigurationDelta& delta,
                                       bool deviceListChanged) noexcept
{
    // Notification failures cannot roll back committed configuration.
    if (!delta.empty()) {
        try {
            m_configurationChanged.send(delta);
        } catch (...) {
        }
    }
    if (deviceListChanged) {
        try {
            m_audioDeviceListChanged.notify();
        } catch (...) {
        }
    }
}

ApplyResult Au3AudioDriverController::apply(const muse::modularity::ContextPtr& requester,
                                            const AudioConfigurationChange& change)
{
    if (m_applying) {
        return { ApplyStatus::Busy };
    }

    const auto before = m_configuration;
    const auto normalized = normalizedConfiguration(before, change);
    if (!normalized) {
        return { ApplyStatus::InvalidConfiguration };
    }
    const auto& after = *normalized;
    const auto delta = makeDelta(before, after);
    if (delta.empty()) {
        return { ApplyStatus::NoChange };
    }

    m_applying = true;
    DEFER {
        m_applying = false;
    };

    std::optional<AudioStreamDescriptor> stream;
    const bool deviceChange = changesRouting(delta);
    std::optional<double> requesterProjectRate;
    AudioStreamRestorer restoreStream;
    bool writeStarted = false;
    try {
        stream = audioEngine() ? audioEngine()->currentStream() : std::nullopt;
        if (delta.contains(AudioConfigurationField::DefaultSampleRate)) {
            if (const auto project = projectForContext(requester)) {
                requesterProjectRate = ::ProjectRate::Get(*project).GetRate();
            }
        }
        if (streamNeedsSuspension(delta, after.defaultSampleRate, requester, stream)) {
            restoreStream = suspendOrForceStop(*stream);
            if (!restoreStream) {
                return { ApplyStatus::OwnerUnavailable };
            }
        }

        writeStarted = true;
        writeConfiguration(after, delta, requester);
        if (deviceChange && audioEngine()) {
            audioEngine()->handleDeviceChange();
        }
    } catch (const std::exception& exception) {
        const bool restored = rollbackAndRestore(before, after, requester,
                                                 requesterProjectRate, deviceChange, writeStarted,
                                                 restoreStream);
        LOGE() << "Failed to apply audio configuration: " << exception.what();
        return { ApplyStatus::InternalError, !restored };
    } catch (...) {
        const bool restored = rollbackAndRestore(before, after, requester, requesterProjectRate,
                                                 deviceChange, writeStarted,
                                                 restoreStream);
        LOGE() << "Failed to apply audio configuration";
        return { ApplyStatus::InternalError, !restored };
    }

    const bool restored = restoreSafely(restoreStream);
    publish(delta);
    return { ApplyStatus::Applied, !restored };
}

ApplyResult Au3AudioDriverController::rescan()
{
    if (m_applying) {
        return { ApplyStatus::Busy };
    }

    m_applying = true;
    DEFER {
        m_applying = false;
    };

    std::optional<AudioStreamDescriptor> stream;
    AudioStreamRestorer restoreStream;
    const auto before = m_configuration;
    auto attempted = before;
    bool writeStarted = false;
    try {
        stream = audioEngine() ? audioEngine()->currentStream() : std::nullopt;
        if (stream) {
            restoreStream = suspendOrForceStop(*stream);
            if (!restoreStream) {
                return { ApplyStatus::OwnerUnavailable };
            }
        }

        DeviceManager::Instance()->Rescan();

        AudioConfigurationChange availableConfiguration;
        const auto availableApis = apis();
        if (!muse::contains(availableApis, before.api)) {
            availableConfiguration.api = preferredAudioHost(availableApis);
        }
        const auto normalized = normalizedConfiguration(before, availableConfiguration);
        if (!normalized) {
            const bool restored = restoreSafely(restoreStream);
            publish(AudioConfigurationDelta {}, true);
            return { ApplyStatus::NoUsableAudioApi, !restored };
        }
        attempted = *normalized;
        const auto delta = makeDelta(before, *normalized);
        writeStarted = true;
        writeConfiguration(*normalized, delta, {});
        if (audioEngine()) {
            // Rescan restarts PortAudio even if the selected names are unchanged.
            audioEngine()->handleDeviceChange();
        }
        const bool restored = restoreSafely(restoreStream);
        publish(delta, true);
        return { ApplyStatus::Applied, !restored };
    } catch (const std::exception& exception) {
        const bool restored = rollbackAndRestore(before, attempted, {}, {}, true,
                                                 writeStarted, restoreStream);
        LOGE() << "Failed to rescan audio devices: " << exception.what();
        return { ApplyStatus::InternalError, !restored };
    } catch (...) {
        const bool restored = rollbackAndRestore(before, attempted, {}, {}, true,
                                                 writeStarted, restoreStream);
        LOGE() << "Failed to rescan audio devices";
        return { ApplyStatus::InternalError, !restored };
    }
}

ApplyResult Au3AudioDriverController::reload(const muse::modularity::ContextPtr& requester)
{
    const auto fromSettings = configurationFromSettings();
    AudioConfigurationChange change;
    change.api = fromSettings.api;
    change.outputDevice = fromSettings.outputDevice;
    change.inputDevice = fromSettings.inputDevice;
    change.inputChannels = fromSettings.inputChannels;
    change.bufferLength = fromSettings.bufferLength;
    change.automaticLatencyCompensation = fromSettings.automaticLatencyCompensation;
    change.latencyCompensation = fromSettings.latencyCompensation;
    change.defaultSampleRate = fromSettings.defaultSampleRate;
    change.defaultSampleFormat = fromSettings.defaultSampleFormat;
    change.asioUseDeviceSampleRate = fromSettings.asioUseDeviceSampleRate;
    const auto result = apply(requester, change);
    if (!result.succeeded() && result.status != ApplyStatus::Busy) {
        // Restore persisted settings when the live configuration rejects them.
        try {
            const auto currentSettings = configurationFromSettings();
            writeConfiguration(m_configuration,
                               makeDelta(currentSettings, m_configuration), {});
        } catch (...) {
        }
    }
    return result;
}

ApplyResult Au3AudioDriverController::openAsioDriverSettings(const AudioRoutingChange& routing)
{
    if (m_applying) {
        return { ApplyStatus::Busy };
    }
    AudioConfigurationChange change;
    change.api = routing.api;
    change.outputDevice = routing.outputDevice;
    change.inputDevice = routing.inputDevice;
    const auto before = m_configuration;
    const auto normalized = normalizedConfiguration(before, change);
    if (!normalized) {
        return { ApplyStatus::InvalidRouting };
    }
    const auto& after = *normalized;
    const auto routingDelta = makeDelta(before, after);

    int paIndex = DeviceManager::Instance()->GetOutputDevicePaIndex(after.api, effectiveOutputDevice(after.api, after.outputDevice));
    if (paIndex < 0) {
        paIndex = DeviceManager::Instance()->GetInputDevicePaIndex(after.api, effectiveInputDevice(after.api, after.inputDevice));
    }
    if (paIndex < 0) {
        return { ApplyStatus::NoAsioDevice };
    }

    m_applying = true;
    DEFER {
        m_applying = false;
    };

    std::optional<AudioStreamDescriptor> stream;
    AudioStreamRestorer restoreStream;
    bool writeStarted = false;
    auto attempted = after;
    AudioConfigurationDelta totalDelta = routingDelta;
    bool deviceStateChanged = changesRouting(routingDelta);
    try {
        stream = audioEngine() ? audioEngine()->currentStream() : std::nullopt;
        // The ASIO panel requires a closed driver even without a routing change.
        if (stream) {
            restoreStream = suspendOrForceStop(*stream);
            if (!restoreStream) {
                return { ApplyStatus::OwnerUnavailable };
            }
        }

        writeStarted = true;
        writeConfiguration(after, routingDelta, {});
        if (!routingDelta.empty() && audioEngine()) {
            audioEngine()->handleDeviceChange();
        }

        DeviceManager::ShowAsioControlPanel(paIndex);
        refreshInputDeviceSettings(after.api, after.inputDevice);

        const auto afterPanel = normalizedConfiguration(after, {});
        if (!afterPanel) {
            throw std::runtime_error(
                      "The ASIO control panel left no usable audio configuration");
        }
        attempted = *afterPanel;
        const auto capabilityDelta = makeDelta(after, attempted);
        if (!capabilityDelta.empty()) {
            const bool capabilityRoutingChanged = changesRouting(capabilityDelta);
            deviceStateChanged = deviceStateChanged || capabilityRoutingChanged;
            writeConfiguration(attempted, capabilityDelta, {});
            if (capabilityRoutingChanged && audioEngine()) {
                audioEngine()->handleDeviceChange();
            }
        }
        totalDelta = makeDelta(before, attempted);
    } catch (const std::exception& exception) {
        const bool restored = rollbackAndRestore(before, attempted, {}, {},
                                                 deviceStateChanged, writeStarted, restoreStream);
        LOGE() << "Failed to open ASIO settings: " << exception.what();
        return { ApplyStatus::InternalError, !restored };
    } catch (...) {
        const bool restored = rollbackAndRestore(before, attempted, {}, {},
                                                 deviceStateChanged, writeStarted, restoreStream);
        LOGE() << "Failed to open ASIO settings";
        return { ApplyStatus::InternalError, !restored };
    }

    const bool restored = restoreSafely(restoreStream);
    publish(totalDelta, true);
    return { ApplyStatus::Applied, !restored };
}

muse::async::Notification Au3AudioDriverController::audioDeviceListChanged() const
{
    return m_audioDeviceListChanged;
}

muse::async::Channel<std::string> Au3AudioDriverController::usedOutputDeviceChanged() const
{
    return m_usedOutputDeviceChanged;
}

muse::async::Channel<std::string> Au3AudioDriverController::usedInputDeviceChanged() const
{
    return m_usedInputDeviceChanged;
}

void Au3AudioDriverController::refreshInputDeviceSettings(const std::string& api,
                                                          const AudioDeviceSelection& inputDevice)
{
    const auto& maps = DeviceManager::Instance()->GetInputDeviceMaps();
    const DeviceSourceMap* device = findDevice(maps, api, effectiveInputDevice(api, inputDevice));
    if (!device) {
        return;
    }

    DeviceManager::Instance()->UpdateAsioDeviceCaps(device->deviceIndex);
    settings()->setLocalValue(RECORDING_SOURCE_INDEX, muse::Val(device->sourceIndex));
    settings()->setLocalValue(RECORDING_SOURCE,
                              muse::Val(device->totalSources >= 1 ? device->sourceString.ToStdString() : std::string()));
}
