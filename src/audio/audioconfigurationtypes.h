/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstdint>
#include <optional>
#include <string>

class AudacityProject;

namespace au::audio {
using AudioDeviceSelection = std::optional<std::string>;

struct AudioConfiguration {
    std::string api;
    AudioDeviceSelection outputDevice;
    AudioDeviceSelection inputDevice;
    int inputChannels = 1;
    double bufferLength = 0.0;
    bool automaticLatencyCompensation = false;
    double latencyCompensation = 0.0;
    uint64_t defaultSampleRate = 0;
    std::string defaultSampleFormat;
    bool asioUseDeviceSampleRate = true;
};

struct AudioConfigurationChange {
    std::optional<std::string> api;
    std::optional<AudioDeviceSelection> outputDevice;
    std::optional<AudioDeviceSelection> inputDevice;
    std::optional<int> inputChannels;
    std::optional<double> bufferLength;
    std::optional<bool> automaticLatencyCompensation;
    std::optional<double> latencyCompensation;
    std::optional<uint64_t> defaultSampleRate;
    std::optional<std::string> defaultSampleFormat;
    std::optional<bool> asioUseDeviceSampleRate;
};

enum class AudioConfigurationField : uint32_t {
    None = 0,
    Api = 1 << 0,
    OutputDevice = 1 << 1,
    InputDevice = 1 << 2,
    InputChannels = 1 << 3,
    BufferLength = 1 << 4,
    AutomaticLatencyCompensation = 1 << 5,
    LatencyCompensation = 1 << 6,
    DefaultSampleRate = 1 << 7,
    DefaultSampleFormat = 1 << 8,
    AsioUseDeviceSampleRate = 1 << 9,
};

using AudioConfigurationFields = uint32_t;

constexpr AudioConfigurationFields fieldMask(AudioConfigurationField field)
{
    return static_cast<AudioConfigurationFields>(field);
}

struct AudioConfigurationDelta {
    AudioConfigurationFields fields = fieldMask(AudioConfigurationField::None);

    bool contains(AudioConfigurationField field) const
    {
        return (fields & fieldMask(field)) != 0;
    }

    bool empty() const { return fields == 0; }
};

enum class ApplyStatus {
    Applied,
    NoChange,
    InvalidConfiguration,
    InvalidRouting,
    NoUsableAudioApi,
    NoAsioDevice,
    OwnerUnavailable,
    Busy,
    InternalError,
};

struct ApplyResult {
    ApplyStatus status = ApplyStatus::InternalError;
    bool streamRestorationFailed = false;

    bool succeeded() const
    {
        return status == ApplyStatus::Applied
               || status == ApplyStatus::NoChange;
    }
};

struct AudioRoutingChange {
    std::optional<std::string> api;
    std::optional<AudioDeviceSelection> outputDevice;
    std::optional<AudioDeviceSelection> inputDevice;
};

enum class AudioStreamKind {
    Playback,
    Monitoring,
    Recording,
};

struct AudioStreamDescriptor {
    AudioStreamKind kind = AudioStreamKind::Playback;
    // Invariant: AudioIO owns at most one process-wide stream.
    AudacityProject* ownerProject = nullptr;
    double sampleRate = 0.0;
};
}
