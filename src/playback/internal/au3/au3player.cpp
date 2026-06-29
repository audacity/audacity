/*
* Audacity: A Digital Audio Editor
*/
#include "au3player.h"

#include "framework/global/types/number.h"
#include "framework/global/realfn.h"
#include "framework/global/translation.h"
#include "framework/global/defer.h"
#include "framework/global/log.h"

#include "au3-time-frequency-selection/SelectedRegion.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-stretching-sequence/StretchingSequence.h"
#include "au3-audio-io/ProjectAudioIO.h"
#include "au3-time-frequency-selection/ViewInfo.h"
#include "au3-audio-io/AudioIO.h"
#include "au3-project-rate/ProjectRate.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include <QVariant>

#include <algorithm>

using namespace au::playback;
using namespace au::au3;

static const muse::secs_t TIME_EPS = muse::secs_t(1 / 1000.0);

Au3Player::Au3Player(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx)
{
    m_playbackStatus.ch.onReceive(this, [this](PlaybackStatus st) {
        m_isPlayingChanged.notify();

        if (st == PlaybackStatus::Running) {
            m_currentTarget.reset();
            m_consumedSamplesSoFar = 0;
            m_reachedEnd.val = false;
        }

        if (st != PlaybackStatus::Stopped) {
            m_timer.start();
        } else {
            int token = ProjectAudioIO::Get(projectRef()).GetAudioIOToken();
            if (!AudioIO::Get()->IsStreamActive(token)) {
                m_timer.stop();
            }
        }
    });

    // Start position tracking timer when recording begins (the timer normally
    // starts on PlaybackStatus change, but recording doesn't change that status).
    recordController()->isRecordingChanged().onNotify(this, [this]() {
        if (recordController()->isRecording() && !m_timer.isActive()) {
            m_currentTarget.reset();
            m_consumedSamplesSoFar = 0;
            m_timer.start();
        }
        m_isPlayAllowedChanged.notify();
    });

    // While recording, the playcursor position is driven by the recording
    // unless this is lead-in
    record()->recordPositionChanged().onReceive(this, [this](const muse::secs_t& pos) {
        if (recordController()->isRecording() && !recordController()->isLeadInRecording()) {
            m_playbackPosition.set(std::max(0.0, pos.raw()));
        }
    });

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        onProjectChanged();

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

    playbackPositionChanged().onReceive(this, [this](const muse::secs_t&) {
        onPlaybackPositionChanged();
    });

    m_loopRegionChanged.onNotify(this, [this]() {
        if (playbackConfiguration()->selectionFollowsLoopRegion()) {
            setSelectionToLoop();
        }
    });

    m_timer.setInterval(16);
    m_timer.setTimerType(Qt::PreciseTimer);
    m_timer.callOnTimeout([this]() { updatePlaybackPosition(); });

    selectionController()->clipsSelected().onReceive(this, [this](const trackedit::ClipKeyList& clipKeyList) {
        if (clipKeyList.empty()) {
            return;
        }
        if (!isPlaying()) {
            stop();
            PlaybackRegion selectionRegion = selectionPlaybackRegion();
            if (selectionRegion.isValid()) {
                doChangePlaybackRegion(selectionRegion);
            }
            doSeek(lastPlaybackSeekTime(), false);
        }
    });

    selectionController()->labelsSelected().onReceive(this, [this](const trackedit::LabelKeyList& labelKeyList) {
        if (labelKeyList.empty()) {
            return;
        }
        if (!isPlaying()) {
            stop();
            PlaybackRegion selectionRegion = selectionPlaybackRegion();
            if (selectionRegion.isValid()) {
                doChangePlaybackRegion(selectionRegion);
            }
            auto labelStartTime = selectionController()->selectedLabelStartTime();
            auto labelEndTime = selectionController()->selectedLabelEndTime();
            if (labelStartTime.has_value()) {
                doSeek(labelStartTime.value(), false);
                m_lastPlaybackRegion = { labelStartTime.value_or(0.0), labelEndTime.value_or(0.0) };
            }
        }
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
        if (audioEngine()->isCapturing()) {
            LOGW() << "Cannot start playback: another project is recording";
            return;
        }
        audioEngine()->stopStream();
    }

    auto& pStartTime = options.pStartTime;

    bool nonWaveToo = options.playNonWaveTracks;

    double t0 = selectedRegion.t0();
    double t1 = selectedRegion.t1();

    if (backwards) {
        std::swap(t0, t1);
    }

    if (audioEngine()->isBusy()) {
        LOGW() << "Audio engine still busy after stopping other stream";
        return;
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

        //! NOTE: only flip to Running when a stream was actually started.
        //! Doing it unconditionally (as before) left the UI in a "playing"
        //! state when t1 == t0 short-circuits doPlayTracks, with no audible
        //! playback and no playhead movement.
        if (ret) {
            m_playbackStatus.set(PlaybackStatus::Running);
        }
    }
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
        playRegion.SetEnd(pos);
    }

    if (applyIfPlaying && m_playbackStatus.val == PlaybackStatus::Running) {
        audioEngine()->seekStream(pos);
    }

    m_playbackPosition.set(pos);

    // Start position tracking if this project's audio stream is active during recording
    // (e.g., lead-in recording). The timer reads GetStreamTime() and
    // updatePlaybackState() will stop it when the stream ends.
    int token = ProjectAudioIO::Get(projectRef()).GetAudioIOToken();
    bool isStreamActive = AudioIO::Get()->IsStreamActive(token);
    if (isStreamActive && m_playbackStatus.val == PlaybackStatus::Stopped
        && !m_timer.isActive()) {
        m_currentTarget.reset();
        m_consumedSamplesSoFar = 0;
        m_timer.start();
    }
}

void Au3Player::rewind()
{
    seek(0.0);
}

void Au3Player::stop()
{
    m_pauseShouldStopPlayback = false;

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

    // Stop the timer once the stream is down; otherwise it ticks past project
    // close, causing a use-after-free in updatePlaybackPosition().
    m_timer.stop();

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
    if (m_pauseShouldStopPlayback && isPlaying()) {
        m_pauseShouldStopPlayback = false;
        stopSeekAndUpdatePlaybackRegion();
        return;
    }

    doPauseStream();
}

void Au3Player::doPauseStream()
{
    if (!canStopAudioStream()) {
        return;
    }
    audioEngine()->pauseStream(true);

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
    Au3Project& project = projectRef();

    auto& playRegion = ViewInfo::Get(project).playRegion;

    if (playRegion.Active()) {
        return;
    }

    double end = region.end;
    if (region.start == region.end) {
        const auto& tracks = Au3TrackList::Get(project);
        end = tracks.GetEndTime();
    }
    playRegion.SetStart(region.start);
    playRegion.SetEnd(end);
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
}

void Au3Player::setLoopRegion(const PlaybackRegion& region)
{
    if (!region.isValid()) {
        return;
    }

    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;

    playRegion.SetAllTimes(region.start, region.end);
    playRegion.SetActive(true);

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
    if (!globalContext()->currentProject()) {
        return false;
    }

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

        if (!selectionController()->timeSelectionIsEmpty()) {
            start = selectionController()->dataSelectedStartTime();
            end = selectionController()->dataSelectedEndTime();
        } else if (selectionController()->leftMostSelectedItemStartTime().has_value()) {
            start = selectionController()->leftMostSelectedItemStartTime().value();
            end = selectionController()->rightMostSelectedItemEndTime().value_or(0.0);
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

void Au3Player::updateStreamState()
{
    int token = ProjectAudioIO::Get(projectRef()).GetAudioIOToken();
    bool isActive = AudioIO::Get()->IsStreamActive(token);

    if (!isActive) {
        if (playbackStatus() == PlaybackStatus::Running
            || playbackStatus() == PlaybackStatus::Paused) {
            m_playbackStatus.set(PlaybackStatus::Stopped);
        } else if (m_timer.isActive()) {
            // Stream ended (playback or recording finished)
            m_timer.stop();
        }
    }
}

void Au3Player::updatePlaybackState()
{
    // Capture drives the cursor via recordPositionChanged — skip here.
    const bool captureDrivingCursor = recordController()->isRecording()
                                      && !recordController()->isLeadInRecording();
    if (!captureDrivingCursor) {
        const double time = std::max(0.0, AudioIO::Get()->GetStreamTime() + m_startOffset);
        if (!muse::is_equal(time, m_playbackPosition.val.raw())) {
            m_playbackPosition.set(time);
        }
    }

    updateStreamState();
}

muse::secs_t Au3Player::playbackPosition() const
{
    return m_playbackPosition.val;
}

void Au3Player::updatePlaybackPosition()
{
    using namespace std::chrono;

    if (!globalContext()->currentProject()) {
        // Project closed but timer still firing; projectRef() would deref null.
        m_timer.stop();
        return;
    }

    const double sampleRate = audioEngine()->getPlaybackSampleRate();

    while (const auto callbackInfo = audioEngine()->consumeNextCallbackInfo()) {
        const auto targetConsumedSamples = static_cast<unsigned long long>(callbackInfo->numSamples)
                                           + (m_currentTarget ? m_currentTarget->consumedSamples : 0);
        const nanoseconds payloadDuration{ static_cast<long>(callbackInfo->numSamples * 1e9 / sampleRate + .5) };

        auto dacTime = callbackInfo->dacTime;
        if (m_currentTarget && m_currentTarget->time > callbackInfo->dacTime) {
            // Jitter was observed in the hardware-thread callbacks on a macbook device: 4096 samples at 44.1kHz is 93ms, yet it
            // was called 10 times with 85ms intervals, then one time with 170ms, over and over again.
            dacTime = m_currentTarget->time;
        }

        const auto targetTime = dacTime + payloadDuration;
        m_currentTarget.emplace(targetTime, targetConsumedSamples);
    }

    const int token = ProjectAudioIO::Get(projectRef()).GetAudioIOToken();
    if (!m_currentTarget.has_value() || !AudioIO::Get()->IsStreamActive(token)) {
        // No DAC callbacks available (e.g. recording-only without overdub playback).
        // GetStreamTime() does not advance here, so we deliberately skip the
        // position write that updatePlaybackState() would do — the playhead is
        // driven externally via setPlaybackPosition() in this case. Only the
        // stream-state / timer cleanup is still needed.
        updateStreamState();
        return;
    }

    const auto timeDiff = duration_cast<milliseconds>(steady_clock::now() - m_currentTarget->time).count() / 1000.0;
    const auto extrapolation = static_cast<long long>(m_currentTarget->consumedSamples + timeDiff * sampleRate);
    // Never progress by more samples than really have been output.
    const auto expectedConsumedNow = std::min(static_cast<long long>(m_currentTarget->consumedSamples), extrapolation);

    if (static_cast<long long>(m_consumedSamplesSoFar) >= expectedConsumedNow) {
        // User isn't hearing audio yet - wait some more. (`expectedConsumedNow` can be negative.)
        return;
    }

    audioEngine()->updateTimePosition(expectedConsumedNow - m_consumedSamplesSoFar);
    m_consumedSamplesSoFar = expectedConsumedNow;
    updatePlaybackState();
}

muse::async::Channel<muse::secs_t> Au3Player::playbackPositionChanged() const
{
    return m_playbackPosition.ch;
}

Au3Project& Au3Player::projectRef() const
{
    const auto project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

bool Au3Player::canStopAudioStream() const
{
    return audioEngine()->canStopAudioStream(projectRef());
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

bool Au3Player::isPlaying() const
{
    return playbackStatus() == PlaybackStatus::Running;
}

bool Au3Player::isPaused() const
{
    return playbackStatus() == PlaybackStatus::Paused;
}

bool Au3Player::isStopped() const
{
    return playbackStatus() == PlaybackStatus::Stopped;
}

muse::async::Notification Au3Player::isPlayingChanged() const
{
    return m_isPlayingChanged;
}

bool Au3Player::isPlayAllowed() const
{
    return !recordController()->isRecording();
}

muse::async::Notification Au3Player::isPlayAllowedChanged() const
{
    return m_isPlayAllowedChanged;
}

muse::secs_t Au3Player::lastPlaybackSeekTime() const
{
    return m_lastPlaybackSeekTime;
}

void Au3Player::setLastPlaybackSeekTime(muse::secs_t secs)
{
    if (muse::RealIsEqual(lastPlaybackSeekTime(), secs)) {
        return;
    }

    m_lastPlaybackSeekTime = secs;
    m_pauseShouldStopPlayback = isPlaying();
    m_lastPlaybackSeekTimeChanged.notify();
}

muse::async::Notification Au3Player::lastPlaybackSeekTimeChanged() const
{
    return m_lastPlaybackSeekTimeChanged;
}

muse::secs_t Au3Player::totalPlayTime() const
{
    au::project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return 0;
    }

    return project->trackeditProject()->totalTime();
}

void Au3Player::togglePlay(bool ignoreSelection)
{
    if (!isPlayAllowed()) {
        LOGW() << "playback not allowed";
        return;
    }

    if (isPlaying()) {
        if (ignoreSelection) {
            stopSeekAndUpdatePlaybackRegion();
        } else {
            pause();
        }
    } else if (isPaused()) {
        if (isSelectionPlaybackRegionChanged()) {
            //! NOTE: just stop, without seek
            stop();
            doPlay(false);
        } else if (ignoreSelection) {
            //! NOTE: set the current position as start position
            doSeek(playbackPosition(), false);
            doPlay(true /* ignoreSelection */);
        } else {
            resume();
        }
    } else {
        if (isPlaybackPositionOnTheEndOfProject() || isPlaybackPositionOnTheEndOfPlaybackRegion()) {
            doSeek(0.0, false);
        }

        doPlay(ignoreSelection);
    }
}

void Au3Player::doPlay(bool ignoreSelection)
{
    if (!ignoreSelection) {
        PlaybackRegion selectionRegion = selectionPlaybackRegion();
        if (selectionRegion.isValid()) {
            doChangePlaybackRegion(selectionRegion);
        } else {
            //! NOTE: no selection — fall back to the user's cursor (lastPlaybackSeekTime)
            //! and the project end.
            const muse::secs_t end = totalPlayTime();
            const muse::secs_t start = lastPlaybackSeekTime();
            if (end > start) {
                doChangePlaybackRegion({ start, end });
            } else {
                LOGW() << "playback region is not valid";
                updatePlaybackRegion();
            }
        }
    } else {
        doChangePlaybackRegion({});
        doSeek(lastPlaybackSeekTime(), false);
    }

    if (!isPlaybackStartPositionValid()) {
        return;
    }

    play();
}

void Au3Player::rewindToStart()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    stopSeekAndUpdatePlaybackRegion();

    doSeek(0.0, false);

    selectionController()->resetTimeSelection();
}

void Au3Player::rewindToEnd()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    setLastPlaybackSeekTime(totalPlayTime());
    m_lastPlaybackRegion = { totalPlayTime(), totalPlayTime() };
    stopSeekAndUpdatePlaybackRegion();

    selectionController()->resetTimeSelection();
}

void Au3Player::seekTo(const muse::secs_t secs, bool triggerPlay)
{
    if (recordController()->isRecording()) {
        return;
    }

    const bool isSeekStartPositionValid = isSeekPositionValid(secs);

    if (isPaused() || (!isSeekStartPositionValid)) {
        stop();
    }

    doSeek(secs, triggerPlay);

    if (triggerPlay) {
        if (isPlaying()) {
            return;
        }

        if (!isSeekStartPositionValid) {
            return;
        }

        play();
    }
}

void Au3Player::doSeek(const muse::secs_t secs, bool applyIfPlaying)
{
    seek(secs, applyIfPlaying);
    setLastPlaybackSeekTime(secs);
    m_lastPlaybackRegion = { secs, secs };
    m_pauseShouldStopPlayback = false;
}

void Au3Player::changePlaybackRegion(const muse::secs_t start, const muse::secs_t end)
{
    doChangePlaybackRegion({ start, end });
}

void Au3Player::doChangePlaybackRegion(const PlaybackRegion& region)
{
    m_lastPlaybackRegion = region;

    if (isStopped()) {
        updatePlaybackRegion();
    }

    if (region.isValid()) {
        setLastPlaybackSeekTime(m_lastPlaybackRegion.start);
    }
}

void Au3Player::stopSeekAndUpdatePlaybackRegion()
{
    stop();

    seek(lastPlaybackSeekTime(), false);
    updatePlaybackRegion();
}

void Au3Player::toggleLoopPlayback()
{
    setLoopRegionActive(!isLoopRegionActive());
}

void Au3Player::setLoopRegionToSelection()
{
    double start = 0;
    double end = 0;

    if (!selectionController()->timeSelectionIsEmpty()) {
        start = selectionController()->dataSelectedStartTime();
        end = selectionController()->dataSelectedEndTime();
    } else {
        auto itemStart = selectionController()->leftMostSelectedItemStartTime();
        auto itemEnd = selectionController()->rightMostSelectedItemEndTime();
        if (itemStart.has_value() && itemEnd.has_value()) {
            start = itemStart.value();
            end = itemEnd.value();
        } else {
            clearLoopRegion();
            return;
        }
    }

    setLoopRegion({ start, end });
}

void Au3Player::setSelectionToLoop()
{
    const PlaybackRegion region = loopRegion();

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    trackedit::TrackIdList tracks = prj->trackIdList();

    selectionController()->setSelectedTracks(tracks, false);
    selectionController()->setDataSelectedStartTime(region.start, false);
    selectionController()->setDataSelectedEndTime(region.end, true);
}

void Au3Player::setLoopRegionInOut()
{
    const PlaybackRegion region = loopRegion();

    muse::UriQuery loopRegionInOutUri("audacity://playback/loop_region_in_out");
    loopRegionInOutUri.addParam("title", muse::Val(muse::trc("trackedit", "Set looping region in/out")));
    loopRegionInOutUri.addParam("start", muse::Val(static_cast<double>(region.start)));
    loopRegionInOutUri.addParam("end", muse::Val(static_cast<double>(region.end)));

    const muse::RetVal<muse::Val> rv = interactive()->openSync(loopRegionInOutUri);
    if (!rv.ret.success()) {
        return;
    }

    const QVariantMap vals = rv.val.toQVariant().toMap();

    setLoopRegion({ vals["start"].toDouble(), vals["end"].toDouble() });
}

void Au3Player::setSelectionFollowsLoopRegion()
{
    playbackConfiguration()->setSelectionFollowsLoopRegion(!playbackConfiguration()->selectionFollowsLoopRegion());
}

void Au3Player::setAudioApi(const std::string& api)
{
    withStreamRestart([this, api]() {
        audioDevicesProvider()->setApi(api);
    });
}

void Au3Player::setAudioOutputDevice(const std::string& device)
{
    withStreamRestart([this, device]() {
        audioDevicesProvider()->setOutputDevice(device);
    });
}

void Au3Player::setAudioInputDevice(const std::string& device)
{
    withStreamRestart([this, device]() {
        audioDevicesProvider()->setInputDevice(device);
    });
}

void Au3Player::withStreamRestart(const std::function<void()>& change)
{
    const bool isRecording = recordController()->isRecording();
    const bool wasPlaying = !isRecording && isPlaying();
    const bool wasPaused = !isRecording && isPaused();
    const muse::secs_t resumePos = playbackPosition();

    if (isRecording || wasPlaying || wasPaused) {
        stop();
    }

    change();

    // Only resume when the user was actively playing. A paused or recording
    // transport is torn down for the switch and then left stopped (the teardown
    // flushes its queues, so the paused state can't be preserved).
    if (wasPlaying) {
        playFrom(resumePos);
    }
}

void Au3Player::playFrom(muse::secs_t pos)
{
    // Resume plain playback from `pos` after the audio stream was torn down for a
    // device change. Keep m_lastPlaybackRegion aligned with the player's
    // resulting zero-length region so a following pause→play resumes from the
    // pause position instead of being mistaken for a selection change. The user's
    // seek bookmark (m_lastPlaybackSeekTime) is intentionally left as-is: the
    // stream restart is our workaround, not a user gesture.
    m_lastPlaybackRegion = { pos, pos };
    seek(pos, false);
    play();
}

PlaybackRegion Au3Player::selectionPlaybackRegion() const
{
    // item selection has priority over time selection
    auto itemStart = selectionController()->leftMostSelectedItemStartTime();
    auto itemEnd = selectionController()->rightMostSelectedItemEndTime();
    if (itemStart.has_value() && itemEnd.has_value()) {
        return { itemStart.value(), itemEnd.value() };
    }

    if (!selectionController()->timeSelectionIsEmpty()) {
        return { selectionController()->dataSelectedStartTime(),
                 selectionController()->dataSelectedEndTime() };
    }

    return PlaybackRegion();
}

bool Au3Player::isSelectionPlaybackRegionChanged() const
{
    return m_lastPlaybackRegion.isValid() && m_lastPlaybackRegion != playbackRegion();
}

void Au3Player::updatePlaybackRegion()
{
    setPlaybackRegion(m_lastPlaybackRegion);
}

void Au3Player::onProjectChanged()
{
    au::project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (prj) {
        prj->aboutCloseBegin().onNotify(this, [this]() {
            stopSeekAndUpdatePlaybackRegion();
        });

        seek(0.0, false); // TODO: get the previous position from the project data
        setLastPlaybackSeekTime(playbackPosition());
    }
}

void Au3Player::onPlaybackPositionChanged()
{
    if (isPlaybackPositionOnTheEndOfProject() || isPlaybackPositionOnTheEndOfPlaybackRegion()) {
        //! NOTE: just stop, without seek
        stop();
        if (playbackRegion() != m_lastPlaybackRegion && !isEqualToPlaybackPosition(m_lastPlaybackRegion.end)) {
            // we want to update the playback region in case user made new selection during playback
            updatePlaybackRegion();
        }
    }
}

bool Au3Player::isEqualToPlaybackPosition(const muse::secs_t position) const
{
    const muse::secs_t playbackPos = playbackPosition();
    return playbackPos - TIME_EPS <= position && position <= playbackPos + TIME_EPS;
}

bool Au3Player::isPlaybackPositionOnTheEndOfProject() const
{
    return isEqualToPlaybackPosition(totalPlayTime());
}

bool Au3Player::isPlaybackPositionOnTheEndOfPlaybackRegion() const
{
    PlaybackRegion region = playbackRegion();
    return region.isValid() && isEqualToPlaybackPosition(region.end) && !isLoopRegionActive();
}

bool Au3Player::isPlaybackStartPositionValid() const
{
    muse::secs_t total = totalPlayTime();

    if (lastPlaybackSeekTime() >= total) {
        return false;
    }

    if (m_lastPlaybackRegion.start >= total) {
        return false;
    }

    return true;
}

bool Au3Player::isSeekPositionValid(const muse::secs_t& seekTime) const
{
    const auto region = playbackRegion();
    return region.isValid() ? (seekTime <= region.end) : (seekTime <= totalPlayTime());
}
