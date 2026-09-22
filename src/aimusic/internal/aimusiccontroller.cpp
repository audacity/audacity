/*
 * Audacity: A Digital Audio Editor
 */
#include "aimusiccontroller.h"

#include <QCoreApplication>
#include <QDir>
#include <QFile>
#include <QJsonDocument>
#include <QJsonArray>
#include <QJsonObject>
#include <QProcess>
#include <QTemporaryFile>

#include <algorithm>
#include <cmath>
#include <vector>

#include "au3-project/Project.h"
#include "au3-label-track/LabelTrack.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3wrap/au3types.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/interactive/iinteractive.h"
#include "trackedit/itrackeditproject.h"

using namespace au::aimusic;
using namespace muse;
using namespace muse::actions;

static const ActionCode ANALYZE_SELECTION_CODE("ai-music-analyze-selection");
static constexpr double MAX_ANALYSIS_SECONDS = 30.0;
static constexpr double MINIMUM_AUTO_TEMPO_CONFIDENCE = 0.50;

void AiMusicController::init()
{
    dispatcher()->reg(this, ANALYZE_SELECTION_CODE, this, &AiMusicController::analyzeSelection);
}

bool AiMusicController::canReceiveAction(const ActionCode& code) const
{
    return code == ANALYZE_SELECTION_CODE;
}

void AiMusicController::analyzeSelection()
{
    if (globalContext()->isRecording() || globalContext()->playbackState()->isPlaying()) {
        interactive()->errorSync("AI Music Studio", "Stop playback or recording before analyzing the selection.");
        return;
    }
    const auto selectedTracks = selectionController()->selectedTracks();
    const muse::secs_t start = selectionController()->dataSelectedStartTime();
    const muse::secs_t end = selectionController()->dataSelectedEndTime();

    if (selectedTracks.size() != 1 || end <= start) {
        interactive()->errorSync("AI Music Studio", "Select a non-empty range on exactly one audio track and try again.");
        return;
    }

    const auto project = globalContext()->currentProject();
    auto* au3Project = project ? reinterpret_cast<AudacityProject*>(project->au3ProjectPtr()) : nullptr;
    auto* sourceTrack = au3Project ? au3::DomAccessor::findWaveTrack(*au3Project, au3::Au3TrackId(selectedTracks.front())) : nullptr;
    if (!sourceTrack) {
        interactive()->errorSync("AI Music Studio", "The selected track could not be read.");
        return;
    }

    const double selectionStart = std::max(start.raw(), sourceTrack->GetStartTime());
    const double selectionEnd = std::min(end.raw(), sourceTrack->GetEndTime());
    if (selectionEnd <= selectionStart || selectionEnd - selectionStart > MAX_ANALYSIS_SECONDS) {
        interactive()->errorSync("AI Music Studio", "Select up to 30 seconds of audio on one track and try again.");
        return;
    }

    const sampleCount first = sourceTrack->TimeToLongSamples(selectionStart);
    const size_t frameCount = (sourceTrack->TimeToLongSamples(selectionEnd) - first).as_size_t();
    std::vector<float> samples(frameCount);
    float* destination = samples.data();
    if (frameCount == 0 || !sourceTrack->GetFloats(0, 1, &destination, first, frameCount)) {
        interactive()->errorSync("AI Music Studio", "The selected audio could not be decoded.");
        return;
    }

    QTemporaryFile audioInput(QDir::tempPath() + "/audacity-ai-music-XXXXXX.f32");
    if (!audioInput.open() || audioInput.write(reinterpret_cast<const char*>(samples.data()), static_cast<qint64>(samples.size() * sizeof(float)))
        != static_cast<qint64>(samples.size() * sizeof(float))) {
        interactive()->errorSync("AI Music Studio", "A temporary analysis file could not be created.");
        return;
    }
    audioInput.close();

#ifdef Q_OS_WIN
    const QString helperName = "ai_music_analysis_helper.exe";
#else
    const QString helperName = "ai_music_analysis_helper";
#endif
    const QString helperPath = QDir(QCoreApplication::applicationDirPath()).filePath(helperName);

    QProcess helper;
    helper.start(helperPath, {
        "--selection-start", QString::number(selectionStart, 'f', 6),
        "--selection-end", QString::number(selectionEnd, 'f', 6),
        "--track-count", QString::number(selectedTracks.size()),
        "--audio-path", audioInput.fileName(),
        "--sample-rate", QString::number(sourceTrack->GetRate(), 'f', 0)
    });

    if (!helper.waitForStarted(2000)) {
        interactive()->errorSync("AI Music Studio", "The local analysis helper could not be started.");
        return;
    }

    if (!helper.waitForFinished(5000) || helper.exitStatus() != QProcess::NormalExit || helper.exitCode() != 0) {
        interactive()->errorSync("AI Music Studio", "The local analysis helper did not complete successfully.");
        return;
    }

    QJsonParseError parseError;
    const QJsonDocument response = QJsonDocument::fromJson(helper.readAllStandardOutput(), &parseError);
    if (parseError.error != QJsonParseError::NoError || !response.isObject()) {
        interactive()->errorSync("AI Music Studio", "The local analysis helper returned an invalid response.");
        return;
    }

    const QJsonObject result = response.object();
    const double duration = result.value("durationSeconds").toDouble();
    const double rms = result.value("rms").toDouble();
    const double estimatedPitchHz = result.value("estimatedPitchHz").toDouble();
    const double bpm = result.value("bpm").toDouble();
    const double bpmConfidence = result.value("bpmConfidence").toDouble();
    const QString musicalKey = result.value("musicalKey").toString();
    const double keyConfidence = result.value("keyConfidence").toDouble();
    const QString pitchLine = estimatedPitchHz > 0.0
                              ? QString("Estimated monophonic pitch: %1 Hz").arg(estimatedPitchHz, 0, 'f', 1)
                              : QString("Estimated monophonic pitch: unavailable");
    const QString tempoLine = bpm > 0.0
                              ? QString("Tempo: %1 BPM (%2% confidence)").arg(bpm, 0, 'f', 1).arg(bpmConfidence * 100.0, 0, 'f', 0)
                              : QString("Tempo: unavailable");
    const QString keyLine = !musicalKey.isEmpty()
                            ? QString("Musical key: %1 (%2% confidence)").arg(musicalKey).arg(keyConfidence * 100.0, 0, 'f', 0)
                            : QString("Musical key: unavailable");
    const QString engineLine = result.value("status").toString() == "ready"
                               ? QString("Engine: %1 at %2 Hz").arg(result.value("analysisEngine").toString()).arg(result.value("analysisSampleRate").toInt())
                               : QString("Analysis unavailable: %1").arg(result.value("analysisError").toString());
    bool appliedTempo = false;
    if (bpm >= 40.0 && bpm <= 300.0 && bpmConfidence >= MINIMUM_AUTO_TEMPO_CONFIDENCE) {
        // Set the source clip tempo before changing the project tempo. This makes
        // the detected tempo a timing/grid update, not a time-stretch operation.
        for (const auto& interval : sourceTrack->SortedIntervalArray()) {
            interval->SetRawAudioTempo(bpm);
        }

        if (const auto timingProject = globalContext()->currentTrackeditProject()) {
            auto timeSignature = timingProject->timeSignature();
            timeSignature.tempo = bpm;
            timingProject->setTimeSignature(timeSignature);
            appliedTempo = true;
        }
    }
    const QString timingLine = appliedTempo
                               ? QString("Project timing updated to %1 BPM; selected-track audio was not stretched.").arg(bpm, 0, 'f', 1)
                               : QString("Project timing was not changed because the detected tempo was unavailable or below the automatic-confidence threshold.");
    // Helper timestamps are relative to the decoded audio, not project zero.
    // Filter padding/out-of-range events before creating native point labels.
    std::vector<double> beats;
    if (appliedTempo) {
        for (const auto& value : result.value("beatTimesSeconds").toArray()) {
            if (!value.isDouble()) {
                continue;
            }
            const double time = value.toDouble();
            if (std::isfinite(time) && time >= 0.0 && time < selectionEnd - selectionStart) {
                beats.push_back(time);
            }
        }
    }
    std::sort(beats.begin(), beats.end());
    beats.erase(std::unique(beats.begin(), beats.end()), beats.end());
    LabelTrack* beatTrack = nullptr;
    if (!beats.empty()) {
        beatTrack = LabelTrack::Create(TrackList::Get(*au3Project), wxString("AI Music: Tempo & Beats"));
        for (const double beat : beats) {
            const double time = selectionStart + beat;
            SelectedRegion point;
            point.setTimes(time, time);
            // This invisible token identifies generated beat markers to the
            // QML timeline, which renders them as compact vertical ticks.
            beatTrack->AddLabel(point, wxString::FromUTF8("\xE2\x81\xA3"));
        }
        globalContext()->currentTrackeditProject()->notifyAboutTrackAdded(au3::DomConverter::labelTrack(beatTrack));
    }

    int keyRegionCount = 0;
    LabelTrack* keyMapTrack = nullptr;
    const auto keyRegions = result.value("keyRegions").toArray();
    if (!keyRegions.empty()) {
        keyMapTrack = LabelTrack::Create(TrackList::Get(*au3Project), wxString("AI Music: Key Map"));
        for (int index = 0; index < keyRegions.size(); ++index) {
            const auto region = keyRegions.at(index).toObject();
            const double regionStart = region.value("startSeconds").toDouble();
            const double regionEnd = index + 1 < keyRegions.size()
                                     ? keyRegions.at(index + 1).toObject().value("startSeconds").toDouble()
                                     : selectionEnd - selectionStart;
            const QString key = region.value("key").toString();
            if (!std::isfinite(regionStart) || !std::isfinite(regionEnd) || regionEnd <= regionStart || key.isEmpty()) {
                continue;
            }
            SelectedRegion span;
            span.setTimes(selectionStart + regionStart, selectionStart + regionEnd);
            keyMapTrack->AddLabel(span, wxString::FromUTF8(key.toUtf8().constData()));
            ++keyRegionCount;
        }
        if (keyRegionCount > 0) {
            globalContext()->currentTrackeditProject()->notifyAboutTrackAdded(au3::DomConverter::labelTrack(keyMapTrack));
        } else {
            TrackList::Get(*au3Project).Remove(*keyMapTrack);
            keyMapTrack = nullptr;
        }
    }

    int chordRegionCount = 0;
    LabelTrack* chordTrack = nullptr;
    const auto chordRegions = result.value("chordRegions").toArray();
    if (!chordRegions.empty()) {
        chordTrack = LabelTrack::Create(TrackList::Get(*au3Project), wxString("AI Music: Chords"));
        for (int index = 0; index < chordRegions.size(); ++index) {
            const auto region = chordRegions.at(index).toObject();
            const double regionStart = region.value("startSeconds").toDouble();
            const double regionEnd = index + 1 < chordRegions.size()
                                     ? chordRegions.at(index + 1).toObject().value("startSeconds").toDouble()
                                     : selectionEnd - selectionStart;
            const QString chord = region.value("chord").toString();
            if (!std::isfinite(regionStart) || !std::isfinite(regionEnd) || regionEnd <= regionStart || chord.isEmpty()) {
                continue;
            }
            SelectedRegion span;
            span.setTimes(selectionStart + regionStart, selectionStart + regionEnd);
            chordTrack->AddLabel(span, wxString::FromUTF8(chord.toUtf8().constData()));
            ++chordRegionCount;
        }
        if (chordRegionCount > 0) {
            globalContext()->currentTrackeditProject()->notifyAboutTrackAdded(au3::DomConverter::labelTrack(chordTrack));
        } else {
            TrackList::Get(*au3Project).Remove(*chordTrack);
            chordTrack = nullptr;
        }
    }
    int arrangementRegionCount = 0;
    LabelTrack* arrangementTrack = nullptr;
    const auto arrangementRegions = result.value("arrangementRegions").toArray();
    if (!arrangementRegions.empty()) {
        arrangementTrack = LabelTrack::Create(TrackList::Get(*au3Project), wxString("AI Music: Arrangement"));
        for (int index = 0; index < arrangementRegions.size(); ++index) {
            const auto region = arrangementRegions.at(index).toObject();
            const double regionStart = region.value("startSeconds").toDouble();
            const double regionEnd = index + 1 < arrangementRegions.size()
                                     ? arrangementRegions.at(index + 1).toObject().value("startSeconds").toDouble()
                                     : selectionEnd - selectionStart;
            const QString section = region.value("section").toString();
            if (!std::isfinite(regionStart) || !std::isfinite(regionEnd) || regionEnd <= regionStart || section.isEmpty()) {
                continue;
            }
            SelectedRegion span;
            span.setTimes(selectionStart + regionStart, selectionStart + regionEnd);
            arrangementTrack->AddLabel(span, wxString::FromUTF8(section.toUtf8().constData()));
            ++arrangementRegionCount;
        }
        if (arrangementRegionCount > 0) {
            globalContext()->currentTrackeditProject()->notifyAboutTrackAdded(au3::DomConverter::labelTrack(arrangementTrack));
        } else {
            TrackList::Get(*au3Project).Remove(*arrangementTrack);
            arrangementTrack = nullptr;
        }
    }
    // Generated analysis lanes always occupy the top of the timeline. Move in
    // reverse display order so the result is Tempo & Beats, Chords, Key Map,
    // Arrangement.
    const auto analysisTracks = { arrangementTrack, keyMapTrack, chordTrack, beatTrack };
    auto& trackList = TrackList::Get(*au3Project);
    const auto trackEditProject = globalContext()->currentTrackeditProject();
    for (auto* track : analysisTracks) {
        if (!track) {
            continue;
        }
        int position = std::distance(trackList.begin(), trackList.Find(track));
        while (trackList.CanMoveUp(*track)) {
            trackList.MoveUp(*track);
            --position;
        }
        if (position >= 0 && trackEditProject) {
            trackEditProject->notifyAboutTrackMoved(au3::DomConverter::labelTrack(track), position);
        }
    }
    if (!beats.empty() || keyRegionCount > 0 || chordRegionCount > 0 || arrangementRegionCount > 0) {
        projectHistory()->pushHistoryState("Added AI music timing, key, chords, and arrangement", "AI Music analysis map");
    }
    const QString markerLine = beats.empty()
                               ? QString("No beat markers added.")
                               : QString("Added %1 beat markers on the AI Music: Tempo & Beats lane.").arg(beats.size());
    const QString keyMapLine = keyRegionCount == 0
                               ? QString("No key-map regions added.")
                               : QString("Added %1 key-map regions on a new AI Music: Key Map label track.").arg(keyRegionCount);
    const QString chordLine = chordRegionCount == 0
                              ? QString("No chord regions added (%1).").arg(result.value("chordStatus").toString())
                              : QString("Added %1 chord regions on a new AI Music: Chords label track.").arg(chordRegionCount);
    const QString arrangementLine = arrangementRegionCount == 0
                                    ? QString("No arrangement regions added (%1).").arg(result.value("arrangementStatus").toString())
                                    : QString("Added %1 neutral arrangement regions on a new AI Music: Arrangement label track.").arg(arrangementRegionCount);
    const QString message = QString("Local analysis helper completed.\n\nSelection: %1 seconds on the selected track.\nDecoded mono frames: %2\nRMS level: %3\n%4\n%5\n%6\n%7\n%8\n%9\n%10\n%11\n%12\nProtocol: %13")
                            .arg(duration, 0, 'f', 3)
                            .arg(result.value("audioFrames").toInt())
                            .arg(rms, 0, 'f', 4)
                            .arg(pitchLine)
                            .arg(tempoLine)
                            .arg(keyLine)
                            .arg(engineLine)
                            .arg(timingLine)
                            .arg(markerLine)
                            .arg(keyMapLine)
                            .arg(chordLine)
                            .arg(arrangementLine)
                            .arg(result.value("protocolVersion").toInt());
    interactive()->infoSync("AI Music Studio", message.toStdString());
}
