/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <array>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace au::audacityplugin {
enum class Status : int32_t {
    Ok = 0,
    InvalidArgument = 1,
    InvalidState = 2,
    ValidationFailed = 3,
    NotReady = 4,
    Cancelled = 5,
    HostError = 6,
    PluginError = 7,
    OutOfMemory = 8,
    BufferTooSmall = 9,
};

enum class PresentationGroup : uint32_t {
    Generate = 1,
    Effect = 2,
    Analyze = 3,
    Tools = 4,
};

inline constexpr uint32_t InputTrackAudio = 1u << 0u;
inline constexpr uint32_t InputTrackLabel = 1u << 1u;

enum class ParameterType : uint32_t {
    Boolean = 1,
    Int64 = 2,
    Double = 3,
    String = 4,
    Enumeration = 5,
    File = 6,
    Directory = 7,
};

using Value = std::variant<bool, int64_t, double, std::string>;

struct EnumChoice {
    std::string token;
    std::string name;
};

struct ParameterDescriptor {
    std::string key;
    std::string name;
    std::string description;
    ParameterType type = ParameterType::Boolean;
    std::string unit;
    Value defaultValue;
    std::optional<Value> minimum;
    std::optional<Value> maximum;
    std::optional<Value> step;
    std::vector<EnumChoice> enumChoices;
};

struct PluginPreference {
    ParameterDescriptor parameter;
    Value value;
};

struct PluginPreferences {
    std::string pluginId;
    std::string pluginName;
    std::string vendor;
    std::string version;
    std::vector<PluginPreference> items;
};

struct EffectDescriptor {
    std::string pluginId;
    std::string effectId;
    std::string name;
    std::string description;
    std::string pluginName;
    std::string vendor;
    std::string version;
    std::string bundlePath;
    PresentationGroup group = PresentationGroup::Effect;
    uint32_t inputTrackTypes = 0;
};

using TrackHandle = uint64_t;
using AudioHandle = uint64_t;
using LabelHandle = uint64_t;

struct AudioFormat {
    uint32_t channelCount = 0;
    uint32_t sampleRate = 0;
};

struct AudioInfo {
    AudioFormat format;
    uint64_t sampleCount = 0;
};

struct AudioChunk {
    std::array<const float*, 2> channels {};
    uint32_t channelCount = 0;
    uint64_t sampleCount = 0;
};

struct AudioTrack {
    std::string name;
    TrackHandle track = 0;
    AudioHandle audio = 0;
    AudioInfo info;
};

struct Label {
    LabelHandle label = 0;
    double startSeconds = 0.0;
    double endSeconds = 0.0;
    std::string text;
};

struct LabelTrack {
    std::string name;
    TrackHandle track = 0;
    std::vector<Label> labels;
};

struct OfflineArgs {
    double selectionDurationSeconds = 0.0;
    double generatorDurationSeconds = 0.0;
    std::vector<AudioTrack> audioTracks;
    std::vector<LabelTrack> labelTracks;
};

class IOfflineHost
{
public:
    virtual ~IOfflineHost() = default;

    virtual Status convertAudio(AudioHandle audio, const AudioFormat& format, AudioHandle& converted, AudioInfo& info) = 0;
    virtual Status readAudio(AudioHandle audio, uint64_t sampleOffset, uint64_t requestedSamples, AudioChunk& chunk) = 0;
    virtual Status createAudio(const AudioFormat& format, AudioHandle& audio) = 0;
    virtual Status writeAudio(AudioHandle audio, const AudioChunk& chunk) = 0;
    virtual Status replaceAudioTrack(AudioHandle audio, TrackHandle track) = 0;
    virtual Status addAudioTrack(AudioHandle audio, const std::string& name) = 0;
    virtual Status releaseAudio(AudioHandle audio) = 0;
    virtual Status createLabelTrack(const std::string& name, TrackHandle& track) = 0;
    virtual Status addLabel(TrackHandle track, double start, double end, const std::string& text, LabelHandle& label) = 0;
    virtual Status updateLabel(LabelHandle label, double start, double end, const std::string& text) = 0;
    virtual Status deleteLabel(LabelHandle label) = 0;
    virtual bool progress(double fraction, const std::string& message) = 0;
    virtual bool cancelled() = 0;

    virtual Status finish() noexcept = 0;
};

struct EffectCreateInfo {
    double selectionDurationSeconds = 0.0;
    uint32_t projectSampleRate = 0;
};

class IEffectInstance
{
public:
    virtual ~IEffectInstance() = default;

    virtual const std::vector<ParameterDescriptor>& parameters() const = 0;
    virtual Value value(uint64_t index) const = 0;
    virtual Status setValue(uint64_t index, const Value& value) = 0;
    virtual Status validate() = 0;
    virtual Status apply(const OfflineArgs& args, IOfflineHost& host) = 0;
};

struct CreateInstanceResult {
    Status status = Status::PluginError;
    std::shared_ptr<IEffectInstance> instance;
};
} // namespace au::audacityplugin
