/*
* Audacity: A Digital Audio Editor
*/
#include "au3player.h"

#include "global/types/number.h"
#include "global/defer.h"

#include "libraries/lib-time-frequency-selection/SelectedRegion.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-stretching-sequence/StretchingSequence.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"
#include "libraries/lib-audio-io/AudioIO.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include "log.h"
#include <algorithm>

#include "ProjectRate.h"

using namespace au::playback;
using namespace au::au3;

Au3Player::Au3Player()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        auto project = globalContext()->currentTrackeditProject();
        if (!project) {
            return;
        }

        static double oldTempo = project->timeSignature().tempo;
        project->timeSignatureChanged().onReceive(this, [this](trackedit::TimeSignature ts){
            auto tempoChange = oldTempo / ts.tempo;

            Au3Project& project = projectRef();
            auto& playRegion = ViewInfo::Get(project).playRegion;

            playRegion.SetAllTimes(playRegion.GetStart() * tempoChange, playRegion.GetEnd() * tempoChange);

            oldTempo = ts.tempo;
            m_loopRegionChanged.notify();
        });
    });
}

bool Au3Player::isBusy() const
{
    return audioEngine()->isBusy();
}

void Au3Player::play()
{
    if (m_playbackStatus.val == PlaybackStatus::Paused) {
        audioEngine()->pauseStream(false);
        m_playbackStatus.set(PlaybackStatus::Running);
        return;
    }

    //! NOTE: copied from ProjectAudioManager::PlayPlayRegion

    Au3Project& project = projectRef();

    const bool newDefault = true; //(mode == PlayMode::loopedPlay);
    auto options = ProjectAudioIO::GetDefaultOptions(project, newDefault);
    bool backwards = false;

    auto& tracks = Au3TrackList::Get(project);

    auto& playRegion = ViewInfo::Get(project).playRegion;

    SelectedRegion selectedRegion(playRegion.GetStart(), playRegion.GetEnd());

    if (!canStopAudioStream()) {
        return /*-1*/;
    }

    auto& pStartTime = options.pStartTime;

    bool nonWaveToo = options.playNonWaveTracks;

    double t0 = selectedRegion.t0();
    double t1 = selectedRegion.t1();

    if (backwards) {
        std::swap(t0, t1);
    }

    if (audioEngine()->isBusy()) {
        return /*-1*/;
    }

    const bool cutpreview = false;//mode == PlayMode::cutPreviewPlay;
    if (cutpreview && t0 == t1) {
        return /*-1*/; /* msmeyer: makes no sense */
    }

    // mLastPlayMode = mode;

    bool hasaudio;
    if (nonWaveToo) {
        hasaudio = !tracks.Any<PlayableTrack>().empty();
    } else {
        hasaudio = !tracks.Any<Au3WaveTrack>().empty();
    }

    double latestEnd = tracks.GetEndTime();
    if (playRegion.Active()) {
        latestEnd = std::max(tracks.GetEndTime(), playRegion.GetEnd());
    }

    if (!hasaudio) {
        return /*-1*/;  // No need to continue without audio tracks
    }

    if (t1 == t0) {
        // move t0 to valid range
        t0 = std::clamp(t0, tracks.GetStartTime(), tracks.GetEndTime());
        t1 = tracks.GetEndTime();
    } else {
        // maybe t1 < t0, with backwards scrubbing for instance
        if (backwards) {
            std::swap(t0, t1);
        }

        t0 = std::max(0.0, std::min(t0, latestEnd));
        t1 = std::max(0.0, std::min(t1, latestEnd));

        if (backwards) {
            std::swap(t0, t1);
        }
    }

    muse::Ret ret;
    PlayTracksOptions opts;
    m_startOffset = 0.0;
    if (!muse::is_equal(t1, t0)) {
        if (cutpreview) {
            const double tless = std::min(t0, t1);
            const double tgreater = std::max(t0, t1);
            double beforeLen, afterLen;
            gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
            gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);
            double tcp0 = tless - beforeLen;
            double tcp1 = tgreater + afterLen;
            if (backwards) {
                std::swap(tcp0, tcp1);
            }

            ret = doPlayTracks(Au3TrackList::Get(project), tcp0, tcp1, opts);
        } else {
            double mixerEndTime = t1;
            if (newDefault) {
                mixerEndTime = latestEnd;
                if (pStartTime && *pStartTime >= t1) {
                    t1 = latestEnd;
                }
            }
            opts.mixerEndTime = mixerEndTime;
            ret = doPlayTracks(TrackList::Get(project), t0, t1, opts);
        }
    }

    m_playbackStatus.set(PlaybackStatus::Running);
}

muse::Ret Au3Player::playTracks(TrackList& trackList, double startTime, double endTime, const PlayTracksOptions& options)
{
    muse::Ret ret = doPlayTracks(trackList, startTime, endTime, options);
    if (ret) {
        m_playbackStatus.set(PlaybackStatus::Running);
    }
    return ret;
}

muse::Ret Au3Player::doPlayTracks(TrackList& trackList, double startTime, double endTime, const PlayTracksOptions& options)
{
    TransportSequences seqs = makeTransportTracks(trackList, options.selectedOnly);

    double mixerEndTime = options.mixerEndTime;
    if (mixerEndTime < 0.0) {
        mixerEndTime = endTime;
    }

    m_startOffset = options.startOffset;

    AudacityProject& project = projectRef();
    const double projectRate = ProjectRate::Get(project).GetRate();
    int token = audioEngine()->startStream(seqs, startTime, endTime, mixerEndTime, project, options.isDefaultPolicy, projectRate);
    bool success = token != 0;
    if (success) {
        ProjectAudioIO::Get(project).SetAudioIOToken(token);
    }

    return success ? muse::make_ok() : muse::make_ret(muse::Ret::Code::InternalError);
}

void Au3Player::seek(const muse::secs_t newPosition, bool applyIfPlaying)
{
    LOGD() << "newPosition: " << newPosition;
    auto pos = std::max(0.0, newPosition.raw());

    Au3Project& project = projectRef();

    auto& playRegion = ViewInfo::Get(project).playRegion;
    if (!playRegion.Active()) {
        playRegion.SetStart(pos);
    }

    if (applyIfPlaying && m_playbackStatus.val == PlaybackStatus::Running) {
        auto gAudioIO = AudioIO::Get();
        gAudioIO->SeekStream(pos - gAudioIO->GetStreamTime());
    }

    m_playbackPosition.set(pos);
}

void Au3Player::rewind()
{
    seek(0.0);
}

void Au3Player::stop()
{
    if (m_playbackStatus.val == PlaybackStatus::Stopped) {
        return;
    }

    m_playbackStatus.set(PlaybackStatus::Stopped);

    //! NOTE: copied from ProjectAudioManager::Stop
    if (!canStopAudioStream()) {
        return;
    }

    audioEngine()->stopStream();
    //Make sure to unpause
    audioEngine()->pauseStream(false);

    // So that we continue monitoring after playing or recording.
    // also clean the MeterQueues
    Au3Project& project = projectRef();
    auto& projectAudioIO = ProjectAudioIO::Get(project);
    auto playbackMeter = projectAudioIO.GetPlaybackMeter();
    if (playbackMeter) {
        playbackMeter->stop();
    }

    auto captureMeter= projectAudioIO.GetCaptureMeter();
    if (captureMeter) {
        captureMeter->stop();
    }
}

void Au3Player::pause()
{
    if (!canStopAudioStream()) {
        return;
    }

    auto gAudioIO = AudioIO::Get();

    gAudioIO->SetPaused(true);

    m_playbackStatus.set(PlaybackStatus::Paused);
}

void Au3Player::resume()
{
    if (!canStopAudioStream()) {
        return;
    }

    audioEngine()->pauseStream(false);

    m_playbackStatus.set(PlaybackStatus::Running);
}

bool Au3Player::isRunning() const
{
    return playbackStatus() == PlaybackStatus::Running;
}

PlaybackStatus Au3Player::playbackStatus() const
{
    return m_playbackStatus.val;
}

muse::async::Channel<PlaybackStatus> Au3Player::playbackStatusChanged() const
{
    return m_playbackStatus.ch;
}

muse::ValNt<bool> Au3Player::reachedEnd() const
{
    return m_reachedEnd;
}

PlaybackRegion Au3Player::playbackRegion() const
{
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;

    double start = playRegion.GetStart();
    double end = playRegion.GetEnd();

    return { std::isinf(start) ? 0 : start, std::isinf(end) ? 0 : end };
}

void Au3Player::setPlaybackRegion(const PlaybackRegion& region)
{
    m_playbackPosition.set(std::max(0.0, region.start.raw()));

    Au3Project& project = projectRef();

    auto& playRegion = ViewInfo::Get(project).playRegion;

    if (playRegion.Active()) {
        return;
    }

    playRegion.SetStart(region.start);

    if (region.start == region.end) {
        auto& tracks = Au3TrackList::Get(project);
        playRegion.SetEnd(tracks.GetEndTime());
    } else {
        playRegion.SetEnd(region.end);
    }
}

PlaybackRegion Au3Player::loopRegion() const
{
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;

    double start = playRegion.GetLastActiveStart();
    double end = playRegion.GetLastActiveEnd();

    return { std::isinf(start) ? 0 : start, std::isinf(end) ? 0 : end };
}

void Au3Player::loopEditingBegin()
{
}

void Au3Player::loopEditingEnd()
{
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;
    playRegion.Order();

    m_playbackPosition.set(std::max(loopRegion().start.raw(), 0.0));
}

void Au3Player::setLoopRegion(const PlaybackRegion& region)
{
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;

    playRegion.SetActive(true);
    playRegion.SetAllTimes(region.start, region.end);

    m_loopRegionChanged.notify();
}

void Au3Player::setLoopRegionStart(const muse::secs_t time)
{
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;

    playRegion.SetStart(time);

    m_loopRegionChanged.notify();
}

void Au3Player::setLoopRegionEnd(const muse::secs_t time)
{
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;

    playRegion.SetEnd(time);

    m_loopRegionChanged.notify();
}

void Au3Player::clearLoopRegion()
{
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;
    playRegion.SetActive(false);
    playRegion.Clear();

    m_loopRegionChanged.notify();
}

bool Au3Player::isLoopRegionClear() const
{
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;

    return playRegion.IsLastActiveRegionClear();
}

bool Au3Player::isLoopRegionActive() const
{
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;

    return playRegion.Active();
}

void Au3Player::setLoopRegionActive(const bool active)
{
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;

    if (playRegion.IsLastActiveRegionClear()) {
        double start = 0;
        double end = 0;

        if (selectionController()->timeSelectionIsNotEmpty()) {
            start = selectionController()->dataSelectedStartTime();
            end = selectionController()->dataSelectedEndTime();
        } else if (selectionController()->hasSelectedClips()) {
            start = selectionController()->leftMostSelectedClipStartTime();
            end = selectionController()->rightMostSelectedClipEndTime();
        } else {
            // Default length is 4 bars
            au::trackedit::TimeSignature ts = globalContext()->currentTrackeditProject()->timeSignature();
            end = 4 * ts.upper * (4.0 / ts.lower) * (60.0 / ts.tempo);
        }

        playRegion.SetAllTimes(start, end);
    }

    playRegion.SetActive(active);

    m_loopRegionChanged.notify();
}

muse::async::Notification Au3Player::loopRegionChanged() const
{
    return m_loopRegionChanged;
}

void Au3Player::updatePlaybackStateTimeCritical()
{
    if (m_playbackStatus.val != PlaybackStatus::Running) {
        return;
    }

    IF_ASSERT_FAILED(globalContext()->currentProject()) {
        return;
    }

    int token = ProjectAudioIO::Get(projectRef()).GetAudioIOToken();
    bool isActive = AudioIO::Get()->IsStreamActive(token);
    double time = AudioIO::Get()->GetStreamTime() + m_startOffset;

    if (isActive) {
        m_reachedEnd.val = false;
        const auto newTime = std::max(0.0, time);
        if (!muse::is_equal(newTime, m_playbackPosition.val.raw())) {
            m_playbackPosition.set(newTime);
        }
    } else {
        if (playbackStatus() == PlaybackStatus::Running && !m_reachedEnd.val) {
            m_reachedEnd.val = true;
            m_reachedEnd.notification.notify();
        }
    }
}

muse::secs_t Au3Player::playbackPosition() const
{
    return m_playbackPosition.val;
}

void Au3Player::updatePlaybackPosition()
{
    auto& audioIO = *AudioIO::Get();
    const std::optional<std::chrono::steady_clock::time_point> startTime = audioIO.UserStartsHearingAudioTimePoint();

    using namespace std::chrono;
    const auto now = steady_clock::now();

    if (!startTime.has_value() || now < *startTime) {
        // Too early: we only want to start when the user starts hearing audio.
        m_elapsedSamplesAtLastReport = 0;
        return;
    }

    muse::Defer defer = [this] {
        updatePlaybackStateTimeCritical();
    };

    const double sampleRate = AudioIO::Get()->GetPlaybackSampleRate();

    // Make 100% sure we do not introduce drift: quantize the elapsed time to an integral sample value,
    // advance by that amount and don't lose track of the amount of samples advanced by so far.
    const auto elapsedMs = duration_cast<milliseconds>(now - *startTime).count();
    const auto elapsedSamples = static_cast<unsigned long long>(elapsedMs / 1000.0 * sampleRate);
    IF_ASSERT_FAILED(elapsedSamples >= m_elapsedSamplesAtLastReport) {
        return;
    }
    const unsigned long long elapsed { elapsedSamples - m_elapsedSamplesAtLastReport };
    m_elapsedSamplesAtLastReport = elapsedSamples;

    audioIO.UpdateTimePosition(elapsed);
}

muse::async::Channel<muse::secs_t> Au3Player::playbackPositionChanged() const
{
    return m_playbackPosition.ch;
}

Au3Project& Au3Player::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

bool Au3Player::canStopAudioStream() const
{
    auto gAudioIO = AudioIO::Get();
    Au3Project& project = projectRef();
    return !gAudioIO->IsStreamActive()
           || gAudioIO->IsMonitoring()
           || gAudioIO->GetOwningProject().get() == &project;
}

TransportSequences Au3Player::makeTransportTracks(Au3TrackList& trackList, bool selectedOnly)
{
    TransportSequences result;
    {
        const auto range = trackList.Any<Au3WaveTrack>()
                           + (selectedOnly ? &Au3Track::IsSelected : &Au3Track::Any);
        for (auto pTrack : range) {
            result.playbackSequences.push_back(
                StretchingSequence::Create(*pTrack, pTrack->GetClipInterfaces()));
        }
    }
    return result;
}
