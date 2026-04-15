/*
* Audacity: A Digital Audio Editor
*/

#include "tempodetection.h"

#include "lteaudioreaderbridge.h"
#include "filenamebpmparser.h"

#include <LoopTempoEstimator/LoopTempoEstimator.h>

#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-stretching-sequence/ClipInterface.h"

#include "au3wrap/internal/domaccessor.h"

#include "log.h"

using namespace au::importexport;
using namespace muse;

static constexpr double MAX_LOOP_DURATION_SECS = 60.0; // loops longer than this are skipped

static constexpr int BTN_DO_NOTHING      = int(IInteractive::Button::CustomButton) + 1;
static constexpr int BTN_MATCH_PROJECT   = int(IInteractive::Button::CustomButton) + 2;
static constexpr int BTN_MATCH_LOOP      = int(IInteractive::Button::CustomButton) + 3;
static constexpr int BTN_STRETCH_LOOP    = int(IInteractive::Button::CustomButton) + 4;

TempoDetection::TempoDetection(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx)
{
}

void TempoDetection::onFilesImported(
    const std::vector<muse::io::path_t>& filePaths,
    const std::vector<WaveTrack*>& waveTracks,
    const std::vector<trackedit::TrackId>& dstTrackIds,
    const std::optional<LibFileFormats::AcidizerTags>& acidTags,
    bool projectWasEmpty)
{
    if (filePaths.empty() || waveTracks.empty()) {
        return;
    }

    const auto pref = configuration()->tempoDetectionPref();
    if (pref == TempoDetectionPref::TempoDetection::NEVER) {
        return;
    }

    if (pref == TempoDetectionPref::TempoDetection::WORKSPACE_DEPENDENT) {
        auto currentWorkspace = workspacesManager()->currentWorkspace();
        if (!currentWorkspace) {
            return;
        }
        const std::string wsName = currentWorkspace->name();
        const auto enabledWorkspaces = configuration()->tempoDetectionWorkspaces();
        if (std::find(enabledWorkspaces.begin(), enabledWorkspaces.end(), wsName) == enabledWorkspaces.end()) {
            return;
        }
    }

    std::optional<TempoDetectionResult> bestResult;
    for (size_t i = 0; i < filePaths.size() && i < waveTracks.size(); ++i) {
        auto result = detectTempo(filePaths[i], *waveTracks[i], acidTags);
        if (result.has_value()) {
            if (!bestResult.has_value() || result->source < bestResult->source) {
                bestResult = result; // prefer higher-priority source (Header < Filename < Signal)
            }
        }
    }

    if (!bestResult.has_value()) {
        return;
    }

    if (projectWasEmpty) {
        showEmptyMusicWorkspaceDialog(bestResult->bpm, waveTracks, dstTrackIds);
    } else {
        showSubsequentImportDialog(bestResult->bpm, waveTracks, dstTrackIds);
    }
}

std::optional<TempoDetectionResult> TempoDetection::detectTempo(
    const muse::io::path_t& filePath,
    const WaveTrack& track,
    const std::optional<LibFileFormats::AcidizerTags>& acidTags)
{
    // If ACID tags mark this as a one-shot, skip detection entirely
    if (acidTags.has_value() && acidTags->isOneShot) {
        return std::nullopt;
    }

    auto clipInterfaces = track.GetClipInterfaces();
    if (clipInterfaces.empty()) {
        return std::nullopt;
    }

    const double duration = static_cast<double>(clipInterfaces[0]->GetVisibleSampleCount().as_long_long())
                            / clipInterfaces[0]->GetRate();
    if (duration > MAX_LOOP_DURATION_SECS || duration <= 0.0) {
        return std::nullopt;
    }

    // Priority 1: ACID/RIFF header tags
    if (acidTags.has_value() && acidTags->bpm.has_value()) {
        return TempoDetectionResult { *acidTags->bpm, TempoDetectionResult::Source::Header };
    }

    // Priority 2: Filename BPM extraction
    std::string fname = io::filename(filePath).toStdString();
    auto filenameBpm = getBpmFromFilename(fname);
    if (filenameBpm.has_value()) {
        return TempoDetectionResult { *filenameBpm, TempoDetectionResult::Source::Filename };
    }

    // Priority 3: Signal analysis via loop-tempo-estimator
    LteAudioReaderBridge reader(clipInterfaces[0]);
    auto signalBpm = LTE::GetBpm(
        reader,
        LTE::FalsePositiveTolerance::Lenient,
        [](double) { /* TODO: progress callback */ });

    if (signalBpm.has_value()) {
        return TempoDetectionResult { *signalBpm, TempoDetectionResult::Source::Signal };
    }

    return std::nullopt;
}

void TempoDetection::showEmptyMusicWorkspaceDialog(double bpm, const std::vector<WaveTrack*>& waveTracks,
                                                   const std::vector<trackedit::TrackId>& dstTrackIds)
{
    std::string message = mtrc("import", "Loop tempo detected at %1 BPM. What would you like to do?")
                          .arg(static_cast<int>(std::round(bpm))).toStdString();

    IInteractive::ButtonDatas buttons = {
        IInteractive::ButtonData(BTN_MATCH_PROJECT, trc("import", "Match project tempo to loop"), true /*accent*/),
        IInteractive::ButtonData(BTN_MATCH_LOOP, trc("import", "Match loop to project tempo")),
        IInteractive::ButtonData(BTN_DO_NOTHING, trc("import", "Do nothing")),
    };

    IInteractive::Result result = interactive()->questionSync(
        trc("import", "Loop tempo detected"),
        message,
        buttons,
        BTN_MATCH_PROJECT);

    if (result.button() == BTN_MATCH_PROJECT) {
        setRawAudioTempoOnClips(waveTracks, bpm);
        setProjectTempo(bpm);
    } else if (result.button() == BTN_MATCH_LOOP) {
        stretchClipsToProjectTempo(waveTracks, dstTrackIds, bpm);
    }
}

void TempoDetection::showSubsequentImportDialog(double bpm, const std::vector<WaveTrack*>& waveTracks,
                                                const std::vector<trackedit::TrackId>& dstTrackIds)
{
    const LoopAction savedAction = configuration()->subsequentImportLoopAction();
    if (savedAction == LoopAction::MatchLoopToProject) {
        stretchClipsToProjectTempo(waveTracks, dstTrackIds, bpm);
        return;
    } else if (savedAction == LoopAction::DoNothing) {
        return;
    }

    std::string message = mtrc("import", "Loop tempo detected at %1 BPM. What would you like to do?")
                          .arg(static_cast<int>(std::round(bpm))).toStdString();

    IInteractive::ButtonDatas buttons = {
        IInteractive::ButtonData(BTN_STRETCH_LOOP, trc("import", "Stretch loop to project tempo"), true /*accent*/),
        IInteractive::ButtonData(BTN_DO_NOTHING, trc("import", "Do nothing")),
    };

    IInteractive::Result result = interactive()->questionSync(
        trc("import", "Loop tempo detected"),
        message,
        buttons,
        BTN_STRETCH_LOOP,
        IInteractive::WithDontShowAgainCheckBox);

    if (result.button() == BTN_STRETCH_LOOP) {
        stretchClipsToProjectTempo(waveTracks, dstTrackIds, bpm);
        if (!result.showAgain()) {
            configuration()->setSubsequentImportLoopAction(LoopAction::MatchLoopToProject);
        }
    } else {
        if (!result.showAgain()) {
            configuration()->setSubsequentImportLoopAction(LoopAction::DoNothing);
        }
    }
}

void TempoDetection::setRawAudioTempoOnClips(const std::vector<WaveTrack*>& waveTracks, double bpm)
{
    // Setting rawAudioTempo on clips before changing the project tempo
    // ensures the stretch ratio stays 1:1 (no stretching).
    // Without this, AU3's ProjectTempoListener would stretch all clips
    // by the ratio oldTempo/newTempo when the project tempo changes.
    for (auto* track : waveTracks) {
        for (const auto& interval : track->SortedIntervalArray()) {
            interval->SetRawAudioTempo(bpm);
        }
    }
}

void TempoDetection::stretchClipsToProjectTempo(const std::vector<WaveTrack*>& waveTracks,
                                                const std::vector<trackedit::TrackId>& dstTrackIds,
                                                double detectedBpm)
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    auto importedClips = collectImportedClipInfos(waveTracks, dstTrackIds);
    applyTempoToClips(waveTracks, detectedBpm, project->timeSignature().tempo);
    makeRoomAndCloseGaps(importedClips);
}

std::vector<ImportedClipInfo> TempoDetection::collectImportedClipInfos(
    const std::vector<WaveTrack*>& waveTracks,
    const std::vector<trackedit::TrackId>& dstTrackIds)
{
    std::vector<ImportedClipInfo> result;
    for (size_t i = 0; i < waveTracks.size() && i < dstTrackIds.size(); ++i) {
        for (const auto& interval : waveTracks[i]->SortedIntervalArray()) {
            result.push_back({ dstTrackIds[i], interval->GetId(), interval->GetPlayEndTime() });
        }
    }
    return result;
}

void TempoDetection::applyTempoToClips(const std::vector<WaveTrack*>& waveTracks,
                                       double detectedBpm, double projectTempo)
{
    // Passing std::nullopt as oldTempo tells OnProjectTempoChange this is an
    // initial tempo assignment — it will set mClipTempo and rescale boundaries.
    for (auto* track : waveTracks) {
        for (const auto& interval : track->SortedIntervalArray()) {
            interval->SetRawAudioTempo(detectedBpm);
            interval->OnProjectTempoChange(std::nullopt, projectTempo);
        }
    }
}

void TempoDetection::makeRoomAndCloseGaps(const std::vector<ImportedClipInfo>& importedClips)
{
    auto* au3Project = reinterpret_cast<AudacityProject*>(
        globalContext()->currentProject()->au3ProjectPtr());

    for (const auto& info : importedClips) {
        // Find the adjacent right neighbour (split/trimmed during import)
        // *before* makeRoomForClip may shift things around.
        WaveTrack* dstTrack = au::au3::DomAccessor::findWaveTrack(
            *au3Project, ::TrackId(info.dstTrackId));
        std::shared_ptr<WaveClip> rightNeighbour;
        if (dstTrack) {
            const double sampleDuration = 1.0 / dstTrack->GetRate();
            for (const auto& candidate : dstTrack->SortedIntervalArray()) {
                if (candidate->GetId() == info.clipId) {
                    continue;
                }
                const double candidateStart = candidate->GetPlayStartTime();
                if (candidateStart >= info.endTimeBeforeStretch - sampleDuration) {
                    const double distance = candidateStart - info.endTimeBeforeStretch;
                    if (distance < sampleDuration && candidate->GetTrimLeft() > 0.0) {
                        rightNeighbour = candidate;
                    }
                    break;
                }
            }
        }

        // If clip grew, push neighbours away.
        trackeditInteraction()->makeRoomForClip(trackedit::ClipKey(info.dstTrackId, info.clipId));

        // If clip shrank, close the gap by untrimming the right neighbour.
        if (!rightNeighbour) {
            continue;
        }
        const trackedit::ClipKey importedKey(info.dstTrackId, info.clipId);
        const double gap = info.endTimeBeforeStretch - trackeditInteraction()->clipEndTime(importedKey);
        if (gap > 0.0 && rightNeighbour->GetTrimLeft() > 0.0) {
            const double untrimAmount = std::min(gap, rightNeighbour->GetTrimLeft());
            trackedit::ClipKey neighbourKey(info.dstTrackId, rightNeighbour->GetId());
            trackeditInteraction()->trimClipsLeft(
                { neighbourKey }, -untrimAmount, 0.0, true, trackedit::UndoPushType::NONE);
        }
    }
}

void TempoDetection::setProjectTempo(double bpm)
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    trackedit::TimeSignature ts = project->timeSignature();
    ts.tempo = bpm;
    project->setTimeSignature(ts);
}
