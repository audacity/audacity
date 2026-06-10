/**********************************************************************

Audacity: A Digital Audio Editor

AudioIOBase.cpp

Paul Licameli split from AudioIO.cpp

**********************************************************************/

#include "AudioIOBase.h"

#include <algorithm>
#include <cassert>
#include <optional>

#include <wx/log.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>

#include "au3-utility/IteratorX.h"
#include "au3-preferences/Prefs.h"
#include "DeviceManager.h"
#include "RtAudioBackend.h"

namespace rta = audacity::rta;

std::map<int, std::vector<long> > AudioIOBase::mCachedPlaybackRates;
std::map<int, std::vector<long> > AudioIOBase::mCachedCaptureRates;
std::map<std::pair<int, int>, std::vector<long> > AudioIOBase::mCachedSampleRates;
int AudioIOBase::mCurrentPlaybackIndex { -1 };
int AudioIOBase::mCurrentCaptureIndex { -1 };
double AudioIOBase::mCachedBestRateIn { 0.0 };

const int AudioIOBase::StandardRates[] = {
    8000,
    11025,
    16000,
    22050,
    32000,
    44100,
    48000,
    88200,
    96000,
    176400,
    192000,
    352800,
    384000
};

const int AudioIOBase::NumStandardRates = WXSIZEOF(AudioIOBase::StandardRates);

const int AudioIOBase::RatesToTry[] = {
    8000,
    9600,
    11025,
    12000,
    15000,
    16000,
    22050,
    24000,
    32000,
    44100,
    48000,
    88200,
    96000,
    176400,
    192000,
    352800,
    384000
};
const int AudioIOBase::NumRatesToTry = WXSIZEOF(AudioIOBase::RatesToTry);

std::unique_ptr<AudioIOBase> AudioIOBase::ugAudioIO;

AudioIOExtBase::~AudioIOExtBase() = default;

AudioIOBase* AudioIOBase::Get()
{
    return ugAudioIO.get();
}

AudioIOBase::AudioIOBase() = default;

AudioIOBase::~AudioIOBase() = default;

void AudioIOBase::SetMixer(int /*inputSource*/)
{
    // The audio backend does not expose hardware mixer control; gain is
    // applied in software by AudioIO. Method retained for caller stability.
}

void AudioIOBase::HandleDeviceChange()
{
    wxASSERT(!IsStreamActive());
    if (IsStreamActive()) {
        return;
    }

    const int playDeviceNum = getPlayDevIndex();
    const int recDeviceNum  = getRecordDevIndex();

    if (mCurrentPlaybackIndex == playDeviceNum
        && mCurrentCaptureIndex  == recDeviceNum) {
        return;
    }

    mCurrentPlaybackIndex = playDeviceNum;
    mCurrentCaptureIndex  = recDeviceNum;
    mCachedBestRateIn = 0.0;

    // Input volume is applied in software in the AudioIO callback;
    // mInputMixerWorks is a const false so callers fall through to that path.
}

int AudioIOBase::GetHostIndex(const std::string& hostName)
{
    const auto list = rta::compiledApis();
    for (size_t i = 0; i < list.size(); ++i) {
        if (list[i].displayName == hostName || list[i].name == hostName) {
            return static_cast<int>(i);
        }
    }
    return -1;
}

void AudioIOBase::StartMeters()
{
    if (auto pInputMeter = mInputMeter.lock()) {
        pInputMeter->start(mRate);
    }
    if (auto pOutputMeter = mOutputMeter.lock()) {
        pOutputMeter->start(mRate);
    }
}

void AudioIOBase::StopMeters()
{
    if (auto pInputMeter = mInputMeter.lock()) {
        pInputMeter->stop();
    }
    if (auto pOutputMeter = mOutputMeter.lock()) {
        pOutputMeter->stop();
    }
}

void AudioIOBase::ResetMeters()
{
    mInputMeter.reset();
    mOutputMeter.reset();
}

void AudioIOBase::SetCaptureMeter(
    const std::shared_ptr<AudacityProject>& project, const std::weak_ptr<IMeterSender>& wMeter)
{
    if (auto pOwningProject = mOwningProject.lock();
        (pOwningProject) && (pOwningProject != project)) {
        return;
    }

    auto meter = wMeter.lock();
    if (meter) {
        mInputMeter = meter;
    } else {
        mInputMeter.reset();
    }
}

void AudioIOBase::SetPlaybackMeter(
    const std::shared_ptr<AudacityProject>& project, const std::weak_ptr<IMeterSender>& wMeter)
{
    if (auto pOwningProject = mOwningProject.lock();
        (pOwningProject) && (pOwningProject != project)) {
        return;
    }

    auto meter = wMeter.lock();
    if (meter) {
        mOutputMeter = meter;
    } else {
        mOutputMeter.reset();
    }
}

bool AudioIOBase::IsPaused() const
{
    return mPaused.load(std::memory_order_relaxed);
}

bool AudioIOBase::IsBusy() const
{
    if (mStreamToken != 0) {
        return true;
    }
    return false;
}

bool AudioIOBase::IsStreamActive() const
{
    bool isActive = mStream.isRunning();
    isActive = isActive
               || std::any_of(mAudioIOExt.begin(), mAudioIOExt.end(),
                              [](auto& pExt){ return pExt && pExt->IsOtherStreamActive(); });
    return isActive;
}

bool AudioIOBase::IsStreamActive(int token) const
{
    return this->IsStreamActive() && this->IsAudioTokenActive(token);
}

bool AudioIOBase::IsAudioTokenActive(int token) const
{
    return token > 0 && token == mStreamToken;
}

bool AudioIOBase::IsMonitoring() const
{
    return mStream.isOpen() && mStreamToken == 0;
}

namespace {

// Find an rta::DeviceInfo by encoded device index. Returns nullopt if the
// index doesn't decode to a real device. Cached per call — cheap relative to
// the work the caller's about to do.
std::optional<rta::DeviceInfo> deviceFromEncoded(int encoded)
{
    if (encoded < 0) {
        return std::nullopt;
    }
    const int apiIdx = rta::apiIndexFromEncoded(encoded);
    const unsigned int devId = rta::deviceIdFromEncoded(encoded);
    const RtAudio::Api api = rta::apiFromIndex(apiIdx);
    if (api == RtAudio::UNSPECIFIED) {
        return std::nullopt;
    }
    const auto devices = rta::enumerateDevices(api);
    for (const auto& d : devices) {
        if (d.id == devId) {
            return d;
        }
    }
    return std::nullopt;
}

bool sampleRatesContains(const std::vector<unsigned int>& rates, long rate)
{
    return std::find(rates.begin(), rates.end(),
                     static_cast<unsigned int>(rate)) != rates.end();
}

} // namespace

bool AudioIOBase::IsPlaybackRateSupported(int devIndex, long rate)
{
    if (devIndex == -1) {
        devIndex = getPlayDevIndex();
    }

    if (mCachedPlaybackRates.count(devIndex)
        && make_iterator_range(mCachedPlaybackRates.at(devIndex)).contains(rate)) {
        return true;
    }

    const auto dev = deviceFromEncoded(devIndex);
    if (!dev) {
        wxLogDebug(wxT("IsPlaybackRateSupported() Could not get device info!"));
        return false;
    }

    if (sampleRatesContains(dev->sampleRates, rate)) {
        mCachedPlaybackRates[devIndex].push_back(rate);
        return true;
    }
    return false;
}

bool AudioIOBase::IsCaptureRateSupported(int devIndex, long rate)
{
    if (devIndex == -1) {
        devIndex = getRecordDevIndex();
    }

    if (mCachedCaptureRates.count(devIndex)
        && make_iterator_range(mCachedCaptureRates.at(devIndex)).contains(rate)) {
        return true;
    }

    const auto dev = deviceFromEncoded(devIndex);
    if (!dev) {
        wxLogDebug(wxT("IsCaptureRateSupported() Could not get device info!"));
        return false;
    }

    if (sampleRatesContains(dev->sampleRates, rate)) {
        mCachedCaptureRates[devIndex].push_back(rate);
        return true;
    }
    return false;
}

std::vector<long> AudioIOBase::GetSupportedPlaybackRates(int devIndex)
{
    if (devIndex == -1) {
        devIndex = getPlayDevIndex();
    }

    std::vector<long> supportedRates;
    for (const long rate : RatesToTry) {
        if (IsPlaybackRateSupported(devIndex, rate)) {
            supportedRates.push_back(rate);
        }
    }
    return supportedRates;
}

std::vector<long> AudioIOBase::GetSupportedCaptureRates(int devIndex)
{
    if (devIndex == -1) {
        devIndex = getRecordDevIndex();
    }

    std::vector<long> supportedRates;
    for (const long rate : RatesToTry) {
        if (IsCaptureRateSupported(devIndex, rate)) {
            supportedRates.push_back(rate);
        }
    }
    return supportedRates;
}

long AudioIOBase::GetClosestSupportedPlaybackRate(int devIndex, long rate)
{
    long supportedRate = 0;
    if (devIndex == -1) {
        devIndex = getPlayDevIndex();
    }

    if (rate == 0.0) {
        return 0;
    }

    std::vector<long> rates = { rate };
    auto higherRatesIt = std::upper_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(higherRatesIt, RatesToTry + NumRatesToTry, std::back_inserter(rates));
    auto lowerRatesIt = std::lower_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(std::make_reverse_iterator(lowerRatesIt), std::make_reverse_iterator(RatesToTry),
              std::back_inserter(rates));

    for (const long rateToTry : rates) {
        if (IsPlaybackRateSupported(devIndex, rateToTry)) {
            supportedRate = rateToTry;
            break;
        }
    }
    return supportedRate;
}

long AudioIOBase::GetClosestSupportedCaptureRate(int devIndex, long rate)
{
    long supportedRate = 0;
    if (devIndex == -1) {
        devIndex = getRecordDevIndex();
    }

    if (rate == 0) {
        return supportedRate;
    }

    if (mCachedCaptureRates.count(devIndex)
        && make_iterator_range(mCachedCaptureRates[devIndex]).contains(rate)) {
        return rate;
    }

    std::vector<long> rates = { rate };
    auto higherRatesIt = std::upper_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(higherRatesIt, RatesToTry + NumRatesToTry, std::back_inserter(rates));
    auto lowerRatesIt = std::lower_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(std::make_reverse_iterator(lowerRatesIt), std::make_reverse_iterator(RatesToTry),
              std::back_inserter(rates));

    for (const long rateToTry : rates) {
        if (IsCaptureRateSupported(devIndex, rateToTry)) {
            supportedRate = rateToTry;
            break;
        }
    }
    return supportedRate;
}

long AudioIOBase::GetClosestSupportedSampleRate(int playDevice, int recDevice, long rate)
{
    long supportedRate = 0;

    if (playDevice == -1) {
        playDevice = getPlayDevIndex();
    }
    if (recDevice == -1) {
        recDevice = getRecordDevIndex();
    }

    std::pair<int, int> devicePair { playDevice, recDevice };
    if (mCachedSampleRates.count(devicePair)
        && make_iterator_range(mCachedSampleRates.at(devicePair)).contains(rate)) {
        return rate;
    }

    std::vector<long> rates { rate };
    auto higherRatesIt = std::upper_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(higherRatesIt, RatesToTry + NumRatesToTry, std::back_inserter(rates));
    auto lowerRatesIt = std::lower_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(std::make_reverse_iterator(lowerRatesIt), std::make_reverse_iterator(RatesToTry),
              std::back_inserter(rates));

    for (const long rateToTry : rates) {
        if (IsPlaybackRateSupported(playDevice, rateToTry)
            && IsCaptureRateSupported(recDevice, rateToTry)) {
            supportedRate = rateToTry;
            break;
        }
    }

    mCachedSampleRates[devicePair].push_back(supportedRate);
    return supportedRate;
}

std::vector<long> AudioIOBase::GetSupportedSampleRates(int playDevice, int recDevice)
{
    if (playDevice == -1) {
        playDevice = getPlayDevIndex();
    }
    if (recDevice == -1) {
        recDevice = getRecordDevIndex();
    }

    auto playback = GetSupportedPlaybackRates(playDevice);
    auto capture  = GetSupportedCaptureRates(recDevice);

    std::vector<long> result;
    std::set_intersection(playback.begin(), playback.end(),
                          capture.begin(), capture.end(),
                          std::back_inserter(result));
    return result;
}

int AudioIOBase::GetOptimalSupportedSampleRate()
{
    auto rate = GetClosestSupportedSampleRate(-1, -1, 44100);
    if (rate == 0) {
        return 44100;
    }
    return rate;
}

namespace {

// Encoded-index lookup of the default device under the user's currently
// selected host API, with a fallback to the first compiled API's default if
// the user's choice is unset or unavailable.
int defaultEncodedDevice(bool isInput)
{
    const wxString hostName = AudioIOHost.Read();
    const auto apis = rta::compiledApis();
    if (apis.empty()) {
        return 0;
    }

    int chosenApi = -1;
    if (!hostName.empty()) {
        for (size_t i = 0; i < apis.size(); ++i) {
            if (wxString::FromUTF8(apis[i].displayName.c_str()) == hostName
                || wxString::FromUTF8(apis[i].name.c_str()) == hostName) {
                chosenApi = static_cast<int>(i);
                break;
            }
        }
    }
    if (chosenApi < 0) {
        chosenApi = 0;
    }

    const auto devices = rta::enumerateDevices(apis[chosenApi].api);
    for (const auto& d : devices) {
        if ((isInput && d.isDefaultInput) || (!isInput && d.isDefaultOutput)) {
            return rta::encodeDeviceIndex(chosenApi, d.id);
        }
    }
    if (!devices.empty()) {
        return rta::encodeDeviceIndex(chosenApi, devices.front().id);
    }
    return 0;
}

} // namespace

int AudioIOBase::getPlayDevIndex(const wxString& devNameArg)
{
    wxString devName(devNameArg);
    if (devName.empty()) {
        devName = AudioIOPlaybackDevice.Read();
    }

    if (!devName.empty()) {
        wxString hostName = AudioIOHost.Read();
        int deviceIndex = DeviceManager::Instance()->GetOutputDevicePaIndex(
            hostName.ToStdString(wxConvUTF8), devName.ToStdString(wxConvUTF8));
        if (deviceIndex >= 0) {
            return deviceIndex;
        }
    }

    return defaultEncodedDevice(/*isInput*/ false);
}

int AudioIOBase::getRecordDevIndex(const wxString& devNameArg)
{
    wxString devName(devNameArg);
    if (devName.empty()) {
        devName = AudioIORecordingDevice.Read();
    }

    if (!devName.empty()) {
        wxString hostName = AudioIOHost.Read();
        int deviceIndex = DeviceManager::Instance()->GetInputDevicePaIndex(
            hostName.ToStdString(wxConvUTF8), devName.ToStdString(wxConvUTF8));
        if (deviceIndex >= 0) {
            return deviceIndex;
        }
    }

    return defaultEncodedDevice(/*isInput*/ true);
}

wxString AudioIOBase::GetDeviceInfo() const
{
    wxStringOutputStream o;
    wxTextOutputStream s(o, wxEOL_UNIX);

    if (IsStreamActive()) {
        return XO("Stream is active ... unable to gather information.\n")
               .Translation();
    }

    auto recDevice  = AudioIORecordingDevice.Read();
    auto playDevice = AudioIOPlaybackDevice.Read();

    const auto apis = rta::compiledApis();
    int totalDevices = 0;
    for (const auto& api : apis) {
        totalDevices += static_cast<int>(rta::enumerateDevices(api.api).size());
    }
    wxLogDebug(wxT("Audio backend reports %d devices"), totalDevices);

    s << wxT("==============================\n");
    s << XO("Default playback device: %s\n").Format(playDevice);
    s << XO("Default recording device: %s\n").Format(recDevice);
    s << XO("Total devices across all APIs: %d\n").Format(totalDevices);

    if (totalDevices <= 0) {
        s << XO("No devices found\n");
        return o.GetString();
    }

    for (size_t apiIdx = 0; apiIdx < apis.size(); ++apiIdx) {
        const auto& api = apis[apiIdx];
        s << wxT("==============================\n");
        s << XO("Host API: %s\n").Format(wxString::FromUTF8(api.displayName.c_str()));

        const auto devices = rta::enumerateDevices(api.api);
        for (const auto& d : devices) {
            const int encoded = rta::encodeDeviceIndex(static_cast<int>(apiIdx), d.id);
            s << wxT("------------------------------\n");
            s << XO("Device ID (encoded): %d\n").Format(encoded);
            s << XO("Device name: %s\n").Format(wxString::FromUTF8(d.name.c_str()));
            s << XO("Recording channels: %d\n").Format(d.maxInputChannels);
            s << XO("Playback channels: %d\n").Format(d.maxOutputChannels);
            s << XO("Preferred sample rate: %u\n").Format(d.preferredSampleRate);

            if (d.maxOutputChannels) {
                auto rates = GetSupportedPlaybackRates(encoded);
                s << XO("Supported Playback Rates:\n");
                for (long r : rates) {
                    s << wxT("    ") << static_cast<int>(r) << wxT("\n");
                }
            }
            if (d.maxInputChannels) {
                auto rates = GetSupportedCaptureRates(encoded);
                s << XO("Supported Capture Rates:\n");
                for (long r : rates) {
                    s << wxT("    ") << static_cast<int>(r) << wxT("\n");
                }
            }
        }
    }

    s << wxT("==============================\n");
    s << XO("Mixer control: software gain only.\n");
    return o.GetString();
}

auto AudioIOBase::GetAllDeviceInfo() -> std::vector<AudioIODiagnostics>
{
    std::vector<AudioIODiagnostics> result;
    result.push_back({
        wxT("audiodev.txt"), GetDeviceInfo(), wxT("Audio Device Info") });
    for ( auto& pExt : mAudioIOExt ) {
        if (pExt) {
            result.emplace_back(pExt->Dump());
        }
    }
    return result;
}

StringSetting AudioIOHost{
    L"/AudioIO/Host", L"" };
BoolSetting AudioIOAutomaticLatencyCompensation{
    L"/AudioIO/AutomaticLatencyCompensation", true };
DoubleSetting AudioIOLatencyCompensation{
    L"/AudioIO/LatencyCompensation", -130.0 };
DoubleSetting AudioIOLatencyDuration{
    L"/AudioIO/LatencyDuration", 100.0 };
StringSetting AudioIOPlaybackDevice{
    L"/AudioIO/PlaybackDevice", L"" };
StringSetting AudioIOPlaybackSource{
    L"/AudioIO/PlaybackSource", L"" };
DoubleSetting AudioIOPlaybackVolume {
    L"/AudioIO/PlaybackVolume", 1.0 };
IntSetting AudioIORecordChannels{
    L"/AudioIO/RecordChannels", 2 };
StringSetting AudioIORecordingDevice{
    L"/AudioIO/RecordingDevice", L"" };
StringSetting AudioIORecordingSource{
    L"/AudioIO/RecordingSource", L"" };
IntSetting AudioIORecordingSourceIndex{
    L"/AudioIO/RecordingSourceIndex", -1 };
