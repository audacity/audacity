/**********************************************************************

Audacity: A Digital Audio Editor

RtAudioBackend.cpp

**********************************************************************/

#include "RtAudioBackend.h"

#include <algorithm>
#include <utility>

namespace audacity::rta {

namespace {

// Internal-token names mirror RtAudio::getApiName().
// Display names mirror RtAudio::getApiDisplayName(), captured here so we don't
// depend on RtAudio's exact string at every call site.
const ApiInfo kKnownApis[] = {
    { RtAudio::MACOSX_CORE,   "core",       "Core Audio" },
    { RtAudio::WINDOWS_WASAPI,"wasapi",     "WASAPI" },
    { RtAudio::WINDOWS_ASIO,  "asio",       "ASIO" },
    { RtAudio::WINDOWS_DS,    "ds",         "DirectSound" },
    { RtAudio::LINUX_ALSA,    "alsa",       "ALSA" },
    { RtAudio::LINUX_PULSE,   "pulse",      "PulseAudio" },
    { RtAudio::LINUX_OSS,     "oss",        "OSS" },
    { RtAudio::UNIX_JACK,     "jack",       "JACK" },
    { RtAudio::RTAUDIO_DUMMY, "dummy",      "Dummy" },
    { RtAudio::UNSPECIFIED,   "unspecified","(unspecified)" },
};

DeviceInfo toDeviceInfo(const RtAudio::DeviceInfo& src)
{
    DeviceInfo out;
    out.id                   = src.ID;
    out.name                 = src.name;
    out.maxInputChannels     = static_cast<int>(src.inputChannels);
    out.maxOutputChannels    = static_cast<int>(src.outputChannels);
    out.maxDuplexChannels    = static_cast<int>(src.duplexChannels);
    out.sampleRates          = src.sampleRates;
    out.preferredSampleRate  = src.preferredSampleRate;
    out.isDefaultInput       = src.isDefaultInput;
    out.isDefaultOutput      = src.isDefaultOutput;
    return out;
}

} // namespace

std::vector<ApiInfo> compiledApis()
{
    std::vector<RtAudio::Api> apis;
    RtAudio::getCompiledApi(apis);

    std::vector<ApiInfo> out;
    out.reserve(apis.size());
    for (RtAudio::Api a : apis) {
        if (const ApiInfo* info = apiInfo(a)) {
            out.push_back(*info);
        } else {
            // Unknown API — fall back to RtAudio's own names so we still
            // surface it instead of silently dropping it.
            out.push_back({ a, RtAudio::getApiName(a), RtAudio::getApiDisplayName(a) });
        }
    }
    return out;
}

std::vector<DeviceInfo> enumerateDevices(RtAudio::Api api)
{
    std::vector<DeviceInfo> out;
    try {
        RtAudio rt(api);
        const auto ids = rt.getDeviceIds();
        out.reserve(ids.size());
        for (unsigned int id : ids) {
            out.push_back(toDeviceInfo(rt.getDeviceInfo(id)));
        }
    } catch (const std::exception&) {
        // RtAudio's constructor throws if the requested API is unavailable.
        // Leave `out` empty.
    }
    return out;
}

std::optional<DeviceInfo> deviceByName(RtAudio::Api api, std::string_view name)
{
    const auto devices = enumerateDevices(api);
    auto it = std::find_if(devices.begin(), devices.end(),
                           [&](const DeviceInfo& d) { return d.name == name; });
    if (it == devices.end()) {
        return std::nullopt;
    }
    return *it;
}

const ApiInfo* apiInfo(RtAudio::Api api)
{
    for (const ApiInfo& info : kKnownApis) {
        if (info.api == api) {
            return &info;
        }
    }
    return nullptr;
}

RtAudio::Api apiFromName(std::string_view name)
{
    for (const ApiInfo& info : kKnownApis) {
        if (info.name == name) {
            return info.api;
        }
    }
    return RtAudio::UNSPECIFIED;
}

RtAudio::Api apiFromIndex(int apiIndex)
{
    if (apiIndex < 0) {
        return RtAudio::UNSPECIFIED;
    }
    const auto list = compiledApis();
    if (static_cast<size_t>(apiIndex) >= list.size()) {
        return RtAudio::UNSPECIFIED;
    }
    return list[apiIndex].api;
}

// ---------------------------------------------------------------------------
// DuplexStream

DuplexStream::DuplexStream() = default;

DuplexStream::~DuplexStream()
{
    close();
}

bool DuplexStream::isRunning() const noexcept
{
    return m_open && m_rt && m_rt->isStreamRunning();
}

double DuplexStream::inputLatencySeconds() const noexcept
{
    if (!m_open || !m_rt || m_config.inputChannels == 0 || m_config.sampleRate == 0) {
        return 0.0;
    }
    // RtAudio reports latency in frames; convert to seconds. getStreamLatency
    // returns the larger of input/output for duplex streams, which is the
    // worst case Audacity already uses for sync math, but we expose both
    // sides explicitly to keep parity with PA's separate fields.
    const long frames = m_rt->getStreamLatency();
    return static_cast<double>(frames) / static_cast<double>(m_config.sampleRate);
}

double DuplexStream::outputLatencySeconds() const noexcept
{
    if (!m_open || !m_rt || m_config.outputChannels == 0 || m_config.sampleRate == 0) {
        return 0.0;
    }
    const long frames = m_rt->getStreamLatency();
    return static_cast<double>(frames) / static_cast<double>(m_config.sampleRate);
}

DuplexStream::Result DuplexStream::open(const Config& cfg, Callback cb)
{
    close();

    try {
        m_rt = std::make_unique<RtAudio>(cfg.api);
    } catch (const std::exception& e) {
        m_lastErrorText = e.what();
        m_rt.reset();
        return Result::NoApi;
    }

    RtAudio::StreamParameters inParams{};
    RtAudio::StreamParameters outParams{};
    RtAudio::StreamParameters* inPtr  = nullptr;
    RtAudio::StreamParameters* outPtr = nullptr;

    if (cfg.inputChannels > 0 && cfg.inputDeviceId != kNoDevice) {
        inParams.deviceId  = cfg.inputDeviceId;
        inParams.nChannels = cfg.inputChannels;
        inParams.firstChannel = 0;
        inPtr = &inParams;
    }
    if (cfg.outputChannels > 0 && cfg.outputDeviceId != kNoDevice) {
        outParams.deviceId  = cfg.outputDeviceId;
        outParams.nChannels = cfg.outputChannels;
        outParams.firstChannel = 0;
        outPtr = &outParams;
    }

    if (!inPtr && !outPtr) {
        m_lastErrorText = "open: neither input nor output requested";
        m_rt.reset();
        return Result::OpenFailed;
    }

    unsigned int bufferFrames = cfg.bufferFrames;
    m_config   = cfg;
    m_callback = std::move(cb);
    m_xrunCount.store(0);

    RtAudio::StreamOptions options{};
    options.streamName = "AudacityRtAudio";

    const RtAudioErrorType err = m_rt->openStream(
        outPtr, inPtr, RTAUDIO_FLOAT32, cfg.sampleRate, &bufferFrames,
        &DuplexStream::dispatch, this, &options);

    if (err != RTAUDIO_NO_ERROR) {
        m_lastErrorText = m_rt->getErrorText();
        m_rt.reset();
        m_callback = {};
        return Result::OpenFailed;
    }

    m_actualBufferFrames = bufferFrames;
    m_open = true;
    m_lastErrorText.clear();
    return Result::Ok;
}

DuplexStream::Result DuplexStream::start()
{
    if (!m_open || !m_rt) {
        return Result::NotOpen;
    }
    const RtAudioErrorType err = m_rt->startStream();
    if (err != RTAUDIO_NO_ERROR) {
        m_lastErrorText = m_rt->getErrorText();
        return Result::StartFailed;
    }
    return Result::Ok;
}

void DuplexStream::stop() noexcept
{
    if (m_rt && m_rt->isStreamRunning()) {
        m_rt->stopStream();
    }
}

void DuplexStream::close() noexcept
{
    if (m_rt) {
        if (m_rt->isStreamRunning()) {
            m_rt->stopStream();
        }
        if (m_rt->isStreamOpen()) {
            m_rt->closeStream();
        }
        m_rt.reset();
    }
    m_callback = {};
    m_open = false;
    m_actualBufferFrames = 0;
}

int DuplexStream::dispatch(void* outBuf, void* inBuf, unsigned int frames,
                           double streamTime, RtAudioStreamStatus status,
                           void* userData)
{
    auto* self = static_cast<DuplexStream*>(userData);
    if (status != 0) {
        self->m_xrunCount.fetch_add(1, std::memory_order_relaxed);
    }
    if (self->m_callback) {
        self->m_callback(static_cast<const float*>(inBuf),
                         static_cast<float*>(outBuf),
                         frames, streamTime, status);
    }
    return 0;
}

} // namespace audacity::rta
