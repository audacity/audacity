/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2010 Audacity Team
   Michael Chinen

******************************************************************/

#include "DeviceManager.h"

#include <map>
#include <wx/log.h>
#include <thread>

#include "portaudio.h"
#ifdef __WXMSW__
#include <windows.h>
#include "pa_asio.h"
#include "pa_win_wasapi.h"
#endif

#ifdef USE_PORTMIXER
#include "portmixer.h"
#endif

#include "au3-preferences/Prefs.h"
#include "au3-utility/IteratorX.h"

#include "AudioIOBase.h"

#include "DeviceChange.h" // for HAVE_DEVICE_CHANGE

DeviceManager DeviceManager::dm;

/// Gets the singleton instance
DeviceManager* DeviceManager::Instance()
{
    return &dm;
}

const std::vector<DeviceSourceMap>& DeviceManager::GetInputDeviceMaps()
{
    if (!m_inited) {
        Init();
    }
    return mInputDeviceSourceMaps;
}

const std::vector<DeviceSourceMap>& DeviceManager::GetOutputDeviceMaps()
{
    if (!m_inited) {
        Init();
    }
    return mOutputDeviceSourceMaps;
}

static std::string GetBaseDeviceName(const DeviceSourceMap* map)
{
    if (!map) {
        return {};
    }
    std::string ret = map->deviceString.ToStdString(wxConvUTF8);
    if (map->totalSources > 1) {
        ret += ": " + map->sourceString.ToStdString(wxConvUTF8);
    }
    return ret;
}

std::string MakeDeviceSourceString(const DeviceSourceMap* map,
                                   const std::vector<DeviceSourceMap>& deviceMaps)
{
    if (!map) {
        return {};
    }

    std::string baseName = GetBaseDeviceName(map);

    int occurrence = 0;
    for (const auto& device : deviceMaps) {
        if (&device == map) {
            break;
        }
        if (device.hostString == map->hostString && GetBaseDeviceName(&device) == baseName) {
            occurrence++;
        }
    }

    if (occurrence == 0) {
        return baseName;
    } else {
        return baseName + "#" + std::to_string(occurrence + 1);
    }
}

static std::pair<std::string, int> ParseDeviceId(const std::string& stableId)
{
    size_t hashPos = stableId.rfind('#');
    if (hashPos == std::string::npos) {
        return { stableId, 0 };
    }

    std::string name = stableId.substr(0, hashPos);
    std::string indexStr = stableId.substr(hashPos + 1);

    try {
        int occurrence = std::stoi(indexStr) - 1;
        return { name, occurrence };
    } catch (...) {
        return { stableId, 0 };
    }
}

static int FindDeviceIndexByDeviceId(const std::vector<DeviceSourceMap>& deviceMaps,
                                     const std::string& hostName,
                                     const std::string& deviceId)
{
    auto [targetName, targetOccurrence] = ParseDeviceId(deviceId);

    std::map<std::string, int> nameCount;
    for (const auto& device : deviceMaps) {
        if (device.hostString != hostName) {
            continue;
        }
        std::string deviceName = GetBaseDeviceName(&device);
        int occurrence = nameCount[deviceName]++;

        if (deviceName == targetName && occurrence == targetOccurrence) {
            return device.deviceIndex;
        }
    }
    return -1;
}

int DeviceManager::GetInputDevicePaIndex(const std::string& hostName, const std::string& deviceId)
{
    if (!m_inited) {
        Init();
    }
    return FindDeviceIndexByDeviceId(mInputDeviceSourceMaps, hostName, deviceId);
}

int DeviceManager::GetOutputDevicePaIndex(const std::string& hostName, const std::string& deviceId)
{
    if (!m_inited) {
        Init();
    }
    return FindDeviceIndexByDeviceId(mOutputDeviceSourceMaps, hostName, deviceId);
}

DeviceSourceMap* DeviceManager::GetDefaultDevice(int hostIndex, int isInput)
{
    if (!m_inited) {
        Init();
    }

    if (hostIndex < 0 || hostIndex >= Pa_GetHostApiCount()) {
        return NULL;
    }

    const struct PaHostApiInfo* apiinfo = Pa_GetHostApiInfo(hostIndex);  // get info on API
    std::vector<DeviceSourceMap>& maps = isInput ? mInputDeviceSourceMaps : mOutputDeviceSourceMaps;
    size_t i;
    int targetDevice = isInput ? apiinfo->defaultInputDevice : apiinfo->defaultOutputDevice;

    for (i = 0; i < maps.size(); i++) {
        if (maps[i].deviceIndex == targetDevice) {
            return &maps[i];
        }
    }

    wxLogDebug(wxT("GetDefaultDevice() no default device"));
    return NULL;
}

DeviceSourceMap* DeviceManager::GetDefaultOutputDevice(int hostIndex)
{
    return GetDefaultDevice(hostIndex, 0);
}

DeviceSourceMap* DeviceManager::GetDefaultInputDevice(int hostIndex)
{
    return GetDefaultDevice(hostIndex, 1);
}

int DeviceManager::GetHostIndex(const std::string& hostName)
{
    PaHostApiIndex hostCnt = Pa_GetHostApiCount();
    for (PaHostApiIndex hostNum = 0; hostNum < hostCnt; hostNum++) {
        const PaHostApiInfo* hinfo = Pa_GetHostApiInfo(hostNum);
        if (hinfo && wxString(wxSafeConvertMB2WX(hinfo->name)) == wxString(hostName)) {
            return hostNum;
        }
    }
    return -1;
}

//--------------- Device Enumeration --------------------------

//Port Audio requires we open the stream with a callback or a lot of devices will fail
//as this means open in blocking mode, so we use a dummy one.
static int DummyPaStreamCallback(
    const void* WXUNUSED(input), void* WXUNUSED(output),
    unsigned long WXUNUSED(frameCount),
    const PaStreamCallbackTimeInfo* WXUNUSED(timeInfo),
    PaStreamCallbackFlags WXUNUSED(statusFlags),
    void* WXUNUSED(userData))
{
    return 0;
}

static void FillHostDeviceInfo(DeviceSourceMap* map, const PaDeviceInfo* info, int deviceIndex, int isInput)
{
    wxString hostapiName = wxSafeConvertMB2WX(Pa_GetHostApiInfo(info->hostApi)->name);
    wxString infoName = wxSafeConvertMB2WX(info->name);

    map->deviceIndex  = deviceIndex;
    map->hostIndex    = info->hostApi;
    map->deviceString = infoName;
    map->hostString   = hostapiName;
    map->numChannels  = isInput ? info->maxInputChannels : info->maxOutputChannels;
}

static void AddSourcesFromStream(int deviceIndex, const PaDeviceInfo* info, std::vector<DeviceSourceMap>* maps, PaStream* stream)
{
#ifdef USE_PORTMIXER
    int i;
#endif
    DeviceSourceMap map;

    map.sourceIndex  = -1;
    map.totalSources = 0;
    // Only inputs have sources, so we call FillHostDeviceInfo with a 1 to indicate this
    FillHostDeviceInfo(&map, info, deviceIndex, 1);

#ifdef USE_PORTMIXER
    PxMixer* portMixer = Px_OpenMixer(stream, deviceIndex, -1, 0);
    if (!portMixer) {
        maps->push_back(map);
        return;
    }

    //if there is only one source, we don't need to concatenate the source
    //or enumerate, because it is something meaningless like 'master'
    //(as opposed to 'mic in' or 'line in'), and the user doesn't have any choice.
    //note that some devices have no input sources at all but are still valid.
    //the behavior we do is the same for 0 and 1 source cases.
    map.totalSources = Px_GetNumInputSources(portMixer);
#endif

    if (map.totalSources <= 1) {
        map.sourceIndex = 0;
        maps->push_back(map);
    }
#ifdef USE_PORTMIXER
    else {
        //open up a stream with the device so portmixer can get the info out of it.
        for (i = 0; i < map.totalSources; i++) {
            map.sourceIndex  = i;
            map.sourceString = wxString(wxSafeConvertMB2WX(Px_GetInputSourceName(portMixer, i)));
            maps->push_back(map);
        }
    }
    Px_CloseMixer(portMixer);
#endif
}

static bool IsInputDeviceAMapperDevice(const PaDeviceInfo* info)
{
    // For Windows only, portaudio returns the default mapper object
    // as the first index after a NEW hostApi index is detected (true for MME and DS)
    // this is a bit of a hack, but there's no other way to find out which device is a mapper,
    // I've looked at string comparisons, but if the system is in a different language this breaks.
#ifdef __WXMSW__
    static int lastHostApiTypeId = -1;
    int hostApiTypeId = Pa_GetHostApiInfo(info->hostApi)->type;
    if (hostApiTypeId != lastHostApiTypeId
        && (hostApiTypeId == paMME || hostApiTypeId == paDirectSound)) {
        lastHostApiTypeId = hostApiTypeId;
        return true;
    }
#endif

    return false;
}

static void AddSources(int deviceIndex, int rate, std::vector<DeviceSourceMap>* maps, int isInput)
{
    int error = 0;
    DeviceSourceMap map;
    const PaDeviceInfo* info = Pa_GetDeviceInfo(deviceIndex);

    // This tries to open the device with the samplerate worked out above, which
    // will be the highest available for play and record on the device, or
    // 44.1kHz if the info cannot be fetched.

    PaStream* stream = NULL;

    PaStreamParameters parameters;

    parameters.device = deviceIndex;
    parameters.sampleFormat = paFloat32;
    parameters.hostApiSpecificStreamInfo = NULL;
    parameters.channelCount = 1;

    // If the device is for input, open a stream so we can use portmixer to query
    // the number of inputs.  We skip this for outputs because there are no 'sources'
    // and some platforms (e.g. XP) have the same device for input and output, (while
    // Vista/Win7 separate these into two devices with the same names (but different
    // portaudio indices)
    // Also, for mapper devices we don't want to keep any sources, so check for it here
    // ASIO devices must not be opened during a scan
    if (isInput && !DeviceManager::IsAsioDevice(deviceIndex) && !IsInputDeviceAMapperDevice(info)) {
        if (info) {
            parameters.suggestedLatency = info->defaultLowInputLatency;
        } else {
            parameters.suggestedLatency = 10.0;
        }

        error = Pa_OpenStream(&stream,
                              &parameters,
                              NULL,
                              rate, paFramesPerBufferUnspecified,
                              paClipOff | paDitherOff,
                              DummyPaStreamCallback, NULL);
    }

    if (stream && !error) {
        AddSourcesFromStream(deviceIndex, info, maps, stream);
        Pa_CloseStream(stream);
    } else {
        map.sourceIndex  = -1;
        map.totalSources = 0;
        FillHostDeviceInfo(&map, info, deviceIndex, isInput);
        maps->push_back(map);
    }

    if (error) {
        wxLogDebug(wxT("PortAudio stream error creating device list: %s:%s: %s"),
                   map.hostString, map.deviceString,
                   wxString(wxSafeConvertMB2WX(Pa_GetErrorText((PaError)error))));
    }
}

bool DeviceManager::IsAsioDevice(int paDeviceIndex)
{
    const PaDeviceInfo* info = Pa_GetDeviceInfo(paDeviceIndex);
    if (!info) {
        return false;
    }

    const PaHostApiInfo* hostInfo = Pa_GetHostApiInfo(info->hostApi);
    return hostInfo && hostInfo->type == paASIO;
}

// ASIO devices are enumerated lazily (driver names only)
// to prevent sound interruptions in other running programs
#ifdef __WXMSW__
static wxString AsioCacheKeyRoot(const char* deviceName)
{
    wxString escaped = wxSafeConvertMB2WX(deviceName);
    escaped.Replace(L"/", L"_");
    return L"/AudioIO/ASIODeviceInfoCache/" + escaped + L"/";
}

#endif

static void ApplyCachedAsioDeviceInfo()
{
#ifdef __WXMSW__
    const int deviceCount = Pa_GetDeviceCount();
    for (int i = 0; i < deviceCount; i++) {
        if (!DeviceManager::IsAsioDevice(i)) {
            continue;
        }

        const wxString root = AsioCacheKeyRoot(Pa_GetDeviceInfo(i)->name);

        int maxInputChannels;
        int maxOutputChannels;
        double defaultSampleRate;
        if (!gPrefs->Read(root + L"MaxInputChannels", &maxInputChannels)
            || !gPrefs->Read(root + L"MaxOutputChannels", &maxOutputChannels)
            || !gPrefs->Read(root + L"DefaultSampleRate", &defaultSampleRate)) {
            continue;
        }

        PaAsio_SetCachedDeviceInfo(i, maxInputChannels, maxOutputChannels, defaultSampleRate);
    }
#endif
}

double DeviceManager::GetAsioDeviceCurrentSampleRate(int paDeviceIndex)
{
#ifdef __WXMSW__
    if (!IsAsioDevice(paDeviceIndex)) {
        return 0.0;
    }

    double rate = 0.0;
    if (PaAsio_GetDeviceCurrentSampleRate(paDeviceIndex, &rate) != paNoError) {
        return 0.0;
    }

    const PaDeviceInfo* info = Pa_GetDeviceInfo(paDeviceIndex);
    const wxString root = AsioCacheKeyRoot(info->name);
    gPrefs->Write(root + L"MaxInputChannels", info->maxInputChannels);
    gPrefs->Write(root + L"MaxOutputChannels", info->maxOutputChannels);
    gPrefs->Write(root + L"DefaultSampleRate", info->defaultSampleRate);
    gPrefs->Flush();

    return rate;
#else
    (void)paDeviceIndex;
    return 0.0;
#endif
}

void DeviceManager::ShowAsioControlPanel(int paDeviceIndex)
{
#ifdef __WXMSW__
    if (IsAsioDevice(paDeviceIndex)) {
        PaAsio_ShowControlPanel(paDeviceIndex, GetDesktopWindow());
    }
#else
    (void)paDeviceIndex;
#endif
}

/// Gets a NEW list of devices by terminating and restarting portaudio
/// Assumes that DeviceManager is only used on the main thread.
void DeviceManager::Rescan()
{
    // get rid of the previous scan info
    this->mInputDeviceSourceMaps.clear();
    this->mOutputDeviceSourceMaps.clear();

    // if we are doing a second scan then restart portaudio to get NEW devices
    if (m_inited) {
        // check to see if there is a stream open - can happen if monitoring,
        // but otherwise Rescan() should not be available to the user.
        auto gAudioIO = AudioIOBase::Get();
        gAudioIO->StopMonitoring(); // TODO Inject and use IAudioEngine instead

        // restart portaudio - this updates the device list
        // FIXME: TRAP_ERR restarting PortAudio
        Pa_Terminate();
        Pa_Initialize();
    }

    ApplyCachedAsioDeviceInfo();

    // FIXME: TRAP_ERR PaErrorCode not handled in ReScan()
    int nDevices = Pa_GetDeviceCount();

    //The hierarchy for devices is Host/device/source.
    //Some newer systems aggregate this.
    //So we need to call port mixer for every device to get the sources
    for (int i = 0; i < nDevices; i++) {
        const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
        if (info->maxOutputChannels > 0) {
            AddSources(i, info->defaultSampleRate, &mOutputDeviceSourceMaps, 0);
        }

        if (info->maxInputChannels > 0) {
            AddSources(i, info->defaultSampleRate, &mInputDeviceSourceMaps, 1);
        }
    }

    // If this was not an initial scan update each device toolbar.
    if (m_inited) {
        Publish(DeviceChangeMessage::Rescan);
    }

    m_inited = true;
    mRescanTime = std::chrono::steady_clock::now();
}

bool DeviceManager::UpdateAsioDeviceCaps(int paDeviceIndex)
{
    if (GetAsioDeviceCurrentSampleRate(paDeviceIndex) <= 0) {
        return false;
    }

    const PaDeviceInfo* info = Pa_GetDeviceInfo(paDeviceIndex);
    if (!info) {
        return false;
    }

    for (auto& map : mInputDeviceSourceMaps) {
        if (map.deviceIndex == paDeviceIndex) {
            map.numChannels = info->maxInputChannels;
        }
    }
    for (auto& map : mOutputDeviceSourceMaps) {
        if (map.deviceIndex == paDeviceIndex) {
            map.numChannels = info->maxOutputChannels;
        }
    }

    return true;
}

std::chrono::duration<float> DeviceManager::GetTimeSinceRescan()
{
    auto now = std::chrono::steady_clock::now();
    auto dur = std::chrono::duration_cast<std::chrono::duration<float> >(now - mRescanTime);
    return dur;
}

//private constructor - Singleton.
DeviceManager::DeviceManager()
#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
    :  DeviceChangeHandler()
#endif
#endif
{
    m_inited = false;
    mRescanTime = std::chrono::steady_clock::now();
}

DeviceManager::~DeviceManager()
{
}

void DeviceManager::Init()
{
    Rescan();

#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
    DeviceChangeHandler::Enable(true);
#endif
#endif
}

#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
void DeviceManager::DeviceChangeNotification()
{
    Rescan();
    return;
}

#endif
#endif

std::map<int, std::vector<long> > DeviceManager::mCachedPlaybackRates;
std::map<int, std::vector<long> > DeviceManager::mCachedCaptureRates;
std::map<std::pair<int, int>, std::vector<long> > DeviceManager::mCachedSampleRates;

const int DeviceManager::StandardRates[] = {
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

const int DeviceManager::NumStandardRates = WXSIZEOF(DeviceManager::StandardRates);

const int DeviceManager::RatesToTry[] = {
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
const int DeviceManager::NumRatesToTry = WXSIZEOF(DeviceManager::RatesToTry);

bool DeviceManager::IsPlaybackRateSupported(int devIndex, long rate)
{
    if (devIndex == -1) { // weren't given a device index, get the prefs / default one
        devIndex = getPlayDevIndex();
    }

    // Check if we can use the cached rate
    if (mCachedPlaybackRates.count(devIndex)
        && (make_iterator_range(mCachedPlaybackRates.at(devIndex)).contains(rate))) {
        return true;
    }

    auto devInfo = Pa_GetDeviceInfo(devIndex);

    if (!devInfo) {
        wxLogDebug(wxT("IsPlaybackRateSupported() Could not get device info!"));
        return false;
    }

    // LLL: Remove when a proper method of determining actual supported
    //      DirectSound rate is devised.
    const PaHostApiInfo* hostInfo = Pa_GetHostApiInfo(devInfo->hostApi);
    bool isDirectSound = (hostInfo && hostInfo->type == paDirectSound);

    PaStreamParameters pars;

    pars.device = devIndex;
    pars.channelCount = 1;
    pars.sampleFormat = paFloat32;
    pars.suggestedLatency = devInfo->defaultHighOutputLatency;
    pars.hostApiSpecificStreamInfo = NULL;

    // JKC: PortAudio Errors handled OK here.  No need to report them

    // LLL: Remove when a proper method of determining actual supported
    //      DirectSound rate is devised.
    if (!(isDirectSound && rate > 200000)) {
        if (Pa_IsFormatSupported(NULL, &pars, rate) == 0) {
            mCachedPlaybackRates[devIndex].push_back(rate);
            return true;
        }
    }
    return false;
}

bool DeviceManager::IsCaptureRateSupported(int devIndex, long rate)
{
    if (devIndex == -1) { // not given a device, look up in prefs / default
        devIndex = getRecordDevIndex();
    }

    // Check if we can use the cached rate
    if (mCachedCaptureRates.count(devIndex)
        && (make_iterator_range(mCachedCaptureRates.at(devIndex)).contains(rate))) {
        return true;
    }

    auto devInfo = Pa_GetDeviceInfo(devIndex);

    if (!devInfo) {
        wxLogDebug(wxT("IsCaptureRateSupported() Could not get device info!"));
        return false;
    }

    auto latencyDuration = AudioIOLatencyDuration.Read();
    // Why not defaulting to 2 as elsewhere?
    auto recordChannels = AudioIORecordChannels.ReadWithDefault(1);

    // LLL: Remove when a proper method of determining actual supported
    //      DirectSound rate is devised.
    const PaHostApiInfo* hostInfo = Pa_GetHostApiInfo(devInfo->hostApi);
    bool isDirectSound = (hostInfo && hostInfo->type == paDirectSound);

    PaStreamParameters pars;

    pars.device = devIndex;
    pars.channelCount = recordChannels;
    pars.sampleFormat = paFloat32;
    pars.suggestedLatency = latencyDuration / 1000.0;
    pars.hostApiSpecificStreamInfo = NULL;

    // LLL: Remove when a proper method of determining actual supported
    //      DirectSound rate is devised.
    if (!(isDirectSound && rate > 200000)) {
        if (Pa_IsFormatSupported(&pars, NULL, rate) == 0) {
            mCachedCaptureRates[devIndex].push_back(rate);
            return true;
        }
    }
    return false;
}

std::vector<long> DeviceManager::GetSupportedPlaybackRates(int devIndex)
{
    if (devIndex == -1) { // weren't given a device index, get the prefs / default one
        devIndex = getPlayDevIndex();
    }

    std::vector<long> supportedRates;

    for (const long rate : RatesToTry) {
        if (IsPlaybackRateSupported(devIndex, rate)) {
            supportedRates.push_back(rate);
        }
        Pa_Sleep(10);   // There are ALSA drivers that don't like being probed
                        // too quickly.
    }

    return supportedRates;
}

std::vector<long> DeviceManager::GetSupportedCaptureRates(int devIndex)
{
    if (devIndex == -1) { // weren't given a device index, get the prefs / default one
        devIndex = getRecordDevIndex();
    }

    std::vector<long> supportedRates;

    for (const long rate : RatesToTry) {
        if (IsCaptureRateSupported(devIndex, rate)) {
            supportedRates.push_back(rate);
        }
        Pa_Sleep(10);   // There are ALSA drivers that don't like being probed
                        // too quickly.
    }

    return supportedRates;
}

long DeviceManager::GetClosestSupportedPlaybackRate(int devIndex, long rate)
{
    long supportedRate = 0;
    if (devIndex == -1) { // weren't given a device index, get the prefs / default one
        devIndex = getPlayDevIndex();
    }

    if (rate == 0.0) { // not given a correct rate
        return 0;
    }

    // First we will probe the requested state
    std::vector<long> rates = { rate };
    // Next default rates higher than requested
    auto higherRatesIt = std::upper_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(higherRatesIt, RatesToTry + NumRatesToTry, std::back_inserter(rates));
    // Last default rates lower than requested in reverse order
    auto lowerRatesIt = std::lower_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(std::make_reverse_iterator(lowerRatesIt), std::make_reverse_iterator(RatesToTry),
              std::back_inserter(rates));

    for (const long rateToTry : rates) {
        if (IsPlaybackRateSupported(devIndex, rateToTry)) {
            supportedRate = rateToTry;
            break;
        }
        Pa_Sleep(10);   // There are ALSA drivers that don't like being probed
                        // too quickly.
    }

    return supportedRate;
}

long DeviceManager::GetClosestSupportedCaptureRate(int devIndex, long rate)
{
    long supportedRate = 0;
    if (devIndex == -1) { // not given a device, look up in prefs / default
        devIndex = getRecordDevIndex();
    }

    if (rate == 0) { // not given a correct rate
        return supportedRate;
    }

    // Check if we can use the cached rate
    if (mCachedCaptureRates.count(devIndex)
        && (make_iterator_range(mCachedCaptureRates[devIndex]).contains(rate))) {
        supportedRate = rate;
        return supportedRate;
    }

    // First we will probe the requested state
    std::vector<long> rates = { rate };

    // Next default rates higher than requested
    auto higherRatesIt = std::upper_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(higherRatesIt, RatesToTry + NumRatesToTry, std::back_inserter(rates));

    // Last default rates lower than requested in reverse order
    auto lowerRatesIt = std::lower_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(std::make_reverse_iterator(lowerRatesIt), std::make_reverse_iterator(RatesToTry),
              std::back_inserter(rates));

    for (const long rateToTry : rates) {
        if (IsCaptureRateSupported(devIndex, rateToTry)) {
            supportedRate = rateToTry;
            break;
        }
        Pa_Sleep(10);   // There are ALSA drivers that don't like being probed
                        // too quickly.
    }

    return supportedRate;
}

long DeviceManager::GetClosestSupportedSampleRate(
    int playDevice, int recDevice, long rate)
{
    long supportedRate = 0;

    // Not given device indices, look up prefs
    if (playDevice == -1) {
        playDevice = getPlayDevIndex();
    }
    if (recDevice == -1) {
        recDevice = getRecordDevIndex();
    }

    // Check if we can use the cached rates
    std::pair<int, int> devicePair { playDevice, recDevice };
    if (mCachedSampleRates.count(devicePair)
        && make_iterator_range(mCachedSampleRates.at(devicePair)).contains(rate)) {
        return rate;
    }

    // First we will probe the requested state
    std::vector<long> rates { rate };

    // Next default rates higher than requested
    auto higherRatesIt = std::upper_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(higherRatesIt, RatesToTry + NumRatesToTry, std::back_inserter(rates));

    // Last default rates lower than requested in reverse order
    auto lowerRatesIt = std::lower_bound(RatesToTry, RatesToTry + NumRatesToTry, rate);
    std::copy(std::make_reverse_iterator(lowerRatesIt), std::make_reverse_iterator(RatesToTry),
              std::back_inserter(rates));

    for (const long rateToTry : rates) {
        if (IsPlaybackRateSupported(playDevice, rateToTry)
            && IsCaptureRateSupported(recDevice, rateToTry)) {
            supportedRate = rateToTry;
            break;
        }
        Pa_Sleep(10);   // There are ALSA drivers that don't like being probed
                        // too quickly.
    }

    mCachedSampleRates[devicePair].push_back(supportedRate);

    return supportedRate;
}

std::vector<long> DeviceManager::GetSupportedSampleRates(int playDevice, int recDevice)
{
    // Not given device indices, look up prefs
    if (playDevice == -1) {
        playDevice = getPlayDevIndex();
    }
    if (recDevice == -1) {
        recDevice = getRecordDevIndex();
    }

    auto playback = GetSupportedPlaybackRates(playDevice);
    auto capture = GetSupportedCaptureRates(recDevice);

    // Return only sample rates which are in both arrays
    std::vector<long> result;

    std::set_intersection(playback.begin(), playback.end(),
                          capture.begin(), capture.end(),
                          std::back_inserter(result));

    return result;
}

int DeviceManager::GetOptimalSupportedSampleRate()
{
    // for ASIO use the last known device rate
    const int playDevice = getPlayDevIndex();
    if (DeviceManager::IsAsioDevice(playDevice)) {
        const PaDeviceInfo* info = Pa_GetDeviceInfo(playDevice);
        if (info && info->defaultSampleRate > 0) {
            return static_cast<int>(info->defaultSampleRate);
        }
        return 44100;
    }

    auto rate = GetClosestSupportedSampleRate(-1, -1, 44100);

    // if there are no supported rates, the next bit crashes. So check first,
    // and give them a "sensible" value if there are no valid values. They
    // will still get an error later, but with any luck may have changed
    // something by then. It's no worse than having an invalid default rate
    // stored in the preferences, which we don't check for
    if (rate == 0) {
        return 44100;
    }

    return rate;
}

int DeviceManager::getPlayDevIndex(const wxString& devNameArg)
{
    wxString devName(devNameArg);
    // if we don't get given a device, look up the preferences
    if (devName.empty()) {
        devName = AudioIOPlaybackDevice.Read();
    }

    wxString hostName = AudioIOHost.Read();

    if (!devName.empty()) {
        int deviceIndex = DeviceManager::Instance()->GetOutputDevicePaIndex(hostName.ToStdString(wxConvUTF8), devName.ToStdString(
                                                                                wxConvUTF8));
        if (deviceIndex >= 0) {
            return deviceIndex;
        }
    }

    const int hostIndex = DeviceManager::Instance()->GetHostIndex(hostName.ToStdString(wxConvUTF8));
    if (const DeviceSourceMap* defaultDevice = DeviceManager::Instance()->GetDefaultOutputDevice(hostIndex)) {
        return defaultDevice->deviceIndex;
    }

    // Use the default output device.
    // FIXME: TRAP_ERR PaErrorCode not handled well (this code is similar to input code
    // and the input side has more comments.)

    PaDeviceIndex deviceNum = Pa_GetDefaultOutputDevice();

    // Sometimes PortAudio returns -1 if it cannot find a suitable default
    // device, so we just use the first one available
    //
    // LL:  At this point, preferences and active no longer match
    //
    //      And I can't imagine how far we'll get specifying an "invalid" index later
    //      on...are we certain "0" even exists?
    if (deviceNum < 0) {
        deviceNum = 0;
    }

    return deviceNum;
}

int DeviceManager::getRecordDevIndex(const wxString& devNameArg)
{
    wxString devName(devNameArg);
    // if we don't get given a device, look up the preferences
    if (devName.empty()) {
        devName = AudioIORecordingDevice.Read();
    }

    wxString hostName = AudioIOHost.Read();

    if (!devName.empty()) {
        int deviceIndex = DeviceManager::Instance()->GetInputDevicePaIndex(
            hostName.ToStdString(wxConvUTF8), devName.ToStdString(wxConvUTF8));
        if (deviceIndex >= 0) {
            return deviceIndex;
        }
    }

    const int hostIndex = DeviceManager::Instance()->GetHostIndex(hostName.ToStdString(wxConvUTF8));
    if (const DeviceSourceMap* defaultDevice = DeviceManager::Instance()->GetDefaultInputDevice(hostIndex)) {
        return defaultDevice->deviceIndex;
    }

    // Use the default input device
    // FIXME: TRAP_ERR PaErrorCode not handled well in getRecordDevIndex()
    PaDeviceIndex deviceNum = Pa_GetDefaultInputDevice();

    // Sometimes PortAudio returns -1 if it cannot find a suitable default
    // device, so we just use the first one available
    // PortAudio has an error reporting function.  We should log/report the error?
    //
    // LL:  At this point, preferences and active no longer match
    //
    //      And I can't imagine how far we'll get specifying an "invalid" index later
    //      on...are we certain "0" even exists?
    if (deviceNum < 0) {
        // JKC: This will happen if you run with no config file
        // This happens once.  Config file will exist on the next run.
        // TODO: Look into this a bit more.  Could be relevant to blank Device Toolbar.
        wxLogDebug("PortAudio returns -1, cannot find a suitable default device, so we just use the first one available");
        deviceNum = 0;
    }

    return deviceNum;
}
