/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <array>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "audacityplugin/audacityplugintypes.h"

class WaveTrack;
class WaveTrackFactory;

namespace au::effects {
struct AudacityPluginSelectedAudio {
    std::shared_ptr<WaveTrack> selectionTrack;
    uint32_t channelCount = 0;
    uint64_t sampleCount = 0;
    au::audacityplugin::TrackHandle track = 0;
    au::audacityplugin::AudioHandle audio = 0;
};

struct AudacityPluginStagedReplacement {
    uint64_t trackIndex = 0;
    std::shared_ptr<WaveTrack> track;
};

struct AudacityPluginStagedAudioTrack {
    std::string name;
    std::shared_ptr<WaveTrack> track;
};

enum class AudacityPluginLabelChangeType {
    Add,
    Update,
    Delete,
};

struct AudacityPluginLabelChange {
    AudacityPluginLabelChangeType type;
    au::audacityplugin::TrackHandle track = 0;
    au::audacityplugin::LabelHandle label = 0;
    double startSeconds = 0.0;
    double endSeconds = 0.0;
    std::string text;
};

class AudacityPluginOfflineHost final : public au::audacityplugin::IOfflineHost
{
public:
    AudacityPluginOfflineHost(
        WaveTrackFactory& factory, std::vector<AudacityPluginSelectedAudio> selectedAudio,
        const std::vector<au::audacityplugin::LabelTrack>& labelTracks, std::function<bool(double, const std::string&)> progress,
        std::function<bool()> cancelled);

    const std::vector<AudacityPluginSelectedAudio>& selectedAudio() const noexcept;

    au::audacityplugin::Status convertAudio(
        au::audacityplugin::AudioHandle audio, const au::audacityplugin::AudioFormat& format, au::audacityplugin::AudioHandle& converted,
        au::audacityplugin::AudioInfo& info) override;
    au::audacityplugin::Status readAudio(
        au::audacityplugin::AudioHandle audio, uint64_t sampleOffset, uint64_t requestedSamples,
        au::audacityplugin::AudioChunk& chunk) override;
    au::audacityplugin::Status createAudio(
        const au::audacityplugin::AudioFormat& format, au::audacityplugin::AudioHandle& audio) override;
    au::audacityplugin::Status writeAudio(
        au::audacityplugin::AudioHandle audio, const au::audacityplugin::AudioChunk& chunk) override;
    au::audacityplugin::Status replaceAudioTrack(
        au::audacityplugin::AudioHandle audio, au::audacityplugin::TrackHandle track) override;
    au::audacityplugin::Status addAudioTrack(
        au::audacityplugin::AudioHandle audio, const std::string& name) override;
    au::audacityplugin::Status releaseAudio(
        au::audacityplugin::AudioHandle audio) override;

    au::audacityplugin::Status createLabelTrack(
        const std::string& name, au::audacityplugin::TrackHandle& track) override;
    au::audacityplugin::Status addLabel(
        au::audacityplugin::TrackHandle track, double start, double end, const std::string& text,
        au::audacityplugin::LabelHandle& label) override;
    au::audacityplugin::Status updateLabel(
        au::audacityplugin::LabelHandle label, double start, double end, const std::string& text) override;
    au::audacityplugin::Status deleteLabel(
        au::audacityplugin::LabelHandle label) override;

    bool progress(double fraction, const std::string& message) override;
    bool cancelled() override;
    au::audacityplugin::Status finish() noexcept override;

    const std::vector<AudacityPluginStagedReplacement>& replacements() const noexcept;
    const std::vector<AudacityPluginStagedAudioTrack>& audioTracks() const noexcept;
    const std::vector<AudacityPluginLabelChange>& labelChanges() const noexcept;
    const std::vector<au::audacityplugin::LabelTrack>& labelTracks() const noexcept;

private:
    struct Audio {
        std::shared_ptr<WaveTrack> track;
        uint64_t sampleCount = 0;
        bool writable = false;
    };

    void throwIfCancelled() const;
    Audio convertTrack(
        const Audio& audio, const au::audacityplugin::AudioFormat& format);
    WaveTrackFactory& m_factory;
    std::vector<AudacityPluginSelectedAudio> m_selectedAudio;
    std::function<bool(double, const std::string&)> m_progress;
    std::function<bool()> m_cancelled;
    std::unordered_map<au::audacityplugin::AudioHandle, Audio> m_audio;
    std::unordered_map<au::audacityplugin::TrackHandle, size_t> m_trackIndices;
    std::unordered_set<au::audacityplugin::TrackHandle> m_labelTrackHandles;
    std::unordered_map<au::audacityplugin::LabelHandle,
                       au::audacityplugin::TrackHandle> m_existingLabels;
    au::audacityplugin::AudioHandle m_nextAudio = 0;
    au::audacityplugin::TrackHandle m_nextTrack = 0;
    au::audacityplugin::LabelHandle m_nextLabel = 0;
    std::array<std::vector<float>, 2> m_readBuffers;
    std::vector<AudacityPluginStagedReplacement> m_replacements;
    std::vector<AudacityPluginStagedAudioTrack> m_audioTracks;
    std::vector<AudacityPluginLabelChange> m_labelChanges;
    std::vector<au::audacityplugin::LabelTrack> m_labelTracks;
};
} // namespace au::effects
