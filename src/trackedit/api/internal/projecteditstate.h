/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <QObject>
#include <QVariantMap>

#include "trackedit/api/projectedit.h"

class LabelTrack;
class Track;
class TrackList;
class TransactionScope;
class WaveTrack;

namespace au::trackedit::api::detail {
constexpr size_t AUDIO_CHUNK_SAMPLES = 65536;

struct AudioFormat {
    uint32_t channelCount = 0;
    double sampleRate = 0.0;
};

struct Replacement {
    std::shared_ptr<WaveTrack> destination;
    std::shared_ptr<WaveTrack> audio;
    double start = 0.0;
    double end = 0.0;
    std::string name;
    bool wholeTrack = false;
};

struct AddedAudio {
    std::string name;
    double start = 0.0;
    std::shared_ptr<WaveTrack> audio;
};

struct ClipChange {
    size_t trackIndex = 0;
    int64_t nativeId = 0;
    std::string title;
};

struct LabelChange {
    size_t trackIndex = 0;
    std::optional<int64_t> nativeId;
    bool remove = false;
    double start = 0.0;
    double end = 0.0;
    std::string text;
};

struct AddedLabelTrack {
    std::string name;
    std::vector<ProjectLabel> labels;
};

struct PreparedChanges {
    std::shared_ptr<TrackList> audioReplacements;
    std::vector<std::shared_ptr<WaveTrack> > audioDestinations;
    std::shared_ptr<TrackList> labelReplacements;
    std::vector<std::shared_ptr<LabelTrack> > labelDestinations;
    std::shared_ptr<TrackList> additions;
};

struct AppliedChanges {
    struct ReplacedTrack {
        std::shared_ptr<Track> original;
        std::shared_ptr<Track> replacement;
    };

    std::vector<ReplacedTrack> replacements;
    std::vector<std::shared_ptr<Track> > additions;
};

struct EditState {
    ~EditState();

    bool acquire();
    void release();
    bool usable() const;
    bool writable() const;
    bool available() const;
    bool hasSelectedClips() const;
    bool hasSelectedLabels() const;

    std::shared_ptr<AudacityProject> project;
    std::function<bool()> projectAvailable;
    std::function<void(double, double, const std::vector<int64_t>&)> setSelection;
    IProjectHistoryPtr history;
    TrackList* tracks = nullptr;
    WaveTrackFactory* factory = nullptr;
    std::shared_ptr<TrackList> sourceTracks;
    std::vector<ProjectAudioTrack> audioTracks;
    std::vector<ProjectLabelTrack> labelTracks;
    std::shared_ptr<WaveTrack> preferredAudioDestination;
    double selectionStart = 0.0;
    double selectionEnd = 0.0;
    bool deferredCommit = false;
    bool active = true;
    bool editStarted = false;
    bool committed = false;
    bool selectionChanged = false;
    std::vector<int64_t> selectedTrackIds;
    bool lockHeld = false;
    size_t writers = 0;
    size_t routedWriters = 0;
    std::string undoName;
    std::string error;
    std::unique_ptr<TransactionScope> transaction;
    std::vector<Replacement> replacements;
    std::vector<AddedAudio> addedAudio;
    std::vector<ClipChange> clipChanges;
    std::vector<LabelChange> labelChanges;
    std::vector<AddedLabelTrack> addedLabelTracks;
    std::unique_ptr<PreparedChanges> prepared;
};

std::optional<AudioFormat> audioFormat(const QVariantMap& value);
bool validTimes(double start, double end);
bool isCurrentTrack(const EditState& state, const std::shared_ptr<Track>& track);

QObject* makeAudioTrack(std::shared_ptr<EditState> state, size_t index, bool selectionOnly, QObject* parent);
QObject* makeAudioWriter(std::shared_ptr<EditState> state, AudioFormat format, std::shared_ptr<WaveTrack> audio, QObject* parent);
bool transformAudio(const std::shared_ptr<EditState>& state, QObject* track, const QVariantMap& options, QObject* processor, QObject* task,
                    QObject* errorContext);

QObject* makeLabelTrack(std::shared_ptr<EditState> state, size_t index, bool intersectingLabelsOnly, QObject* parent);
std::optional<size_t> audioTrackIndex(const std::shared_ptr<EditState>& state, QObject* object);
std::optional<size_t> labelTrackIndex(const std::shared_ptr<EditState>& state, QObject* object);
QObject* makeAddedLabelTrack(std::shared_ptr<EditState> state, size_t index, QObject* parent);

QObject* makeProjectSelection(std::shared_ptr<EditState> state, QObject* parent);
QObject* makeProjectEdit(std::shared_ptr<EditState> state, QObject* parent);

bool prepareChanges(EditState& state, PreparedChanges& prepared, std::string& error);
bool applyChanges(EditState& state, std::string& error, AppliedChanges* applied = nullptr);
bool rollbackChanges(EditState& state, AppliedChanges& applied, std::string& error);
} // namespace au::trackedit::api::detail
