/*
* Audacity: A Digital Audio Editor
*/
#include "au3player.h"

#include "framework/global/types/number.h"
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

#include <algorithm>

using namespace au::playback;
using namespace au::au3;

Au3Player::Au3Player()
{
    m_playbackStatus.ch.onReceive(this, [this](PlaybackStatus st) {
        if (st == PlaybackStatus::Running) {
            m_currentTarget.reset();
            m_consumedSamplesSoFar = 0;
            m_reachedEnd.val = false;
        }

        if (st != PlaybackStatus::Stopped) {
            m_timer.start();
        } else {
            m_timer.stop();
        }
    });

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

    m_timer.setInterval(16);
    m_timer.setTimerType(Qt::PreciseTimer);
    m_timer.callOnTimeout([this]() { updatePlaybackPosition(); });
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
        playRegion.SetEnd(pos);
    }

    if (applyIfPlaying && m_playbackStatus.val == PlaybackStatus::Running) {
        audioEngine()->seekStream(pos);
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
    Au3Project& project = projectRef();
    auto& playRegion = ViewInfo::Get(project).playRegion;

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

void Au3Player::updatePlaybackState()
{
    int token = ProjectAudioIO::Get(projectRef()).GetAudioIOToken();
    bool isActive = AudioIO::Get()->IsStreamActive(token);
    const double time = std::max(0.0, AudioIO::Get()->GetStreamTime() + m_startOffset);

    if (!muse::is_equal(time, m_playbackPosition.val.raw())) {
        m_playbackPosition.set(time);
    }

    if (isActive) {
        m_reachedEnd.val = false;
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
    using namespace std::chrono;

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

    if (!m_currentTarget.has_value()) {
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
