/**********************************************************************

Audacity: A Digital Audio Editor

RtAudioBackend.h

Audio backend wrapping the RtAudio library: device enumeration, host-API
discovery, and a duplex callback-driven stream lifecycle.

**********************************************************************/

#ifndef __AUDACITY_RTAUDIO_BACKEND__
#define __AUDACITY_RTAUDIO_BACKEND__

#include <atomic>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#if defined(_WIN32) && !defined(WIN32_LEAN_AND_MEAN)
#define WIN32_LEAN_AND_MEAN
#endif

#include <RtAudio.h>

namespace audacity::rta {

struct DeviceInfo
{
    unsigned int id { 0 };
    std::string name;
    int maxInputChannels { 0 };
    int maxOutputChannels { 0 };
    int maxDuplexChannels { 0 };
    std::vector<unsigned int> sampleRates;
    unsigned int preferredSampleRate { 0 };
    bool isDefaultInput { false };
    bool isDefaultOutput { false };
};

struct ApiInfo
{
    RtAudio::Api api { RtAudio::UNSPECIFIED };
    std::string name;        // internal token, e.g. "core", "wasapi"
    std::string displayName; // human-readable, e.g. "Core Audio", "WASAPI"
};

// APIs RtAudio was compiled with on this platform.
std::vector<ApiInfo> compiledApis();

// All devices visible to the given API. Empty if the API has no available
// devices or is not compiled in. Never throws — RtAudio exceptions are caught
// and surfaced as an empty result with an error logged.
std::vector<DeviceInfo> enumerateDevices(RtAudio::Api api);

// First device matching name under the given API, if any.
std::optional<DeviceInfo> deviceByName(RtAudio::Api api, std::string_view name);

// Convert API enum <-> display string. apiFromName is case-sensitive on the
// internal token (the `name` field of ApiInfo).
const ApiInfo* apiInfo(RtAudio::Api api);
RtAudio::Api apiFromName(std::string_view name);

// Encoded "device index" packing scheme that preserves the legacy `int
// deviceIndex` slot in DeviceSourceMap. High byte = api index in
// compiledApis() order, low 24 bits = rtaudio device id. AudioIO calls these
// to recover (api, deviceId) when opening a stream from a persisted index.
constexpr int kEncodedApiShift = 24;
constexpr int kEncodedDeviceMask = 0x00FFFFFF;

inline int encodeDeviceIndex(int apiIndex, unsigned int rtDeviceId)
{
    return (apiIndex << kEncodedApiShift)
         | static_cast<int>(rtDeviceId & kEncodedDeviceMask);
}

inline int apiIndexFromEncoded(int encoded)
{
    return encoded >> kEncodedApiShift;
}

inline unsigned int deviceIdFromEncoded(int encoded)
{
    return static_cast<unsigned int>(encoded & kEncodedDeviceMask);
}

// Resolve an api index (compiledApis()-relative) back to an RtAudio::Api enum.
RtAudio::Api apiFromIndex(int apiIndex);

// RAII duplex stream over RtAudio. The callback runs on RtAudio's audio
// thread and receives interleaved float32. Either side may have 0 channels
// (for capture-only / playback-only); pass kNoDevice in the device id for the
// unused side.
//
// Lifecycle: open() -> start() -> ... -> stop() -> close(). Destructor stops
// and closes. open() may be called again after close().
class DuplexStream
{
public:
    enum class Result
    {
        Ok,
        NoApi,          // RtAudio instance couldn't be created for the api
        OpenFailed,
        StartFailed,
        NotOpen,
    };

    static constexpr unsigned int kNoDevice = static_cast<unsigned int>(-1);

    struct Config
    {
        RtAudio::Api api { RtAudio::UNSPECIFIED };
        unsigned int inputDeviceId  { kNoDevice };
        unsigned int outputDeviceId { kNoDevice };
        unsigned int inputChannels  { 0 };
        unsigned int outputChannels { 0 };
        unsigned int sampleRate     { 48000 };
        // Hint to RtAudio; the negotiated size is reported via actualBufferFrames().
        unsigned int bufferFrames   { 512 };
    };

    // streamTime is RtAudio's stream-relative clock in seconds. status is a
    // bitmask of RTAUDIO_INPUT_OVERFLOW / RTAUDIO_OUTPUT_UNDERFLOW; the
    // DuplexStream tracks an aggregate xrun count separately so callers don't
    // have to.
    using Callback = std::function<
        void(const float* input, float* output, unsigned int frames,
             double streamTime, RtAudioStreamStatus status)>;

    DuplexStream();
    ~DuplexStream();

    DuplexStream(const DuplexStream&) = delete;
    DuplexStream& operator=(const DuplexStream&) = delete;

    Result open(const Config& cfg, Callback cb);
    Result start();
    void   stop()  noexcept;
    void   close() noexcept;

    bool isOpen()    const noexcept { return m_open; }
    bool isRunning() const noexcept;

    // Post-open negotiated values; zero / nullopt before open.
    unsigned int actualBufferFrames() const noexcept { return m_actualBufferFrames; }
    double inputLatencySeconds()  const noexcept;
    double outputLatencySeconds() const noexcept;

    unsigned long long xrunCount() const noexcept { return m_xrunCount.load(); }

    const std::string& lastErrorText() const noexcept { return m_lastErrorText; }

private:
    static int dispatch(void* outBuf, void* inBuf, unsigned int frames,
                        double streamTime, RtAudioStreamStatus status,
                        void* userData);

    std::unique_ptr<RtAudio> m_rt;
    Callback m_callback;
    Config m_config {};
    unsigned int m_actualBufferFrames { 0 };
    std::atomic<unsigned long long> m_xrunCount { 0 };
    std::string m_lastErrorText;
    bool m_open { false };
};

} // namespace audacity::rta

#endif
