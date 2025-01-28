/*
* Audacity: A Digital Audio Editor
*/
#include "au3player.h"

#include "global/types/number.h"

#include "libraries/lib-time-frequency-selection/SelectedRegion.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-stretching-sequence/StretchingSequence.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"
#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-devices/Meter.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include "log.h"

using namespace au::playback;
using namespace au::au3;

Au3Player::Au3Player()
    : m_positionUpdateTimer(std::chrono::milliseconds(16))
{
    m_positionUpdateTimer.onTimeout(this, [this]() {
        updatePlaybackState();
    });

    m_playbackStatus.ch.onReceive(this, [this](PlaybackStatus st) {
        if (st == PlaybackStatus::Running) {
            m_positionUpdateTimer.start();
        } else {
            m_positionUpdateTimer.stop();
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
        auto gAudioIO = AudioIO::Get();
        gAudioIO->SetPaused(false);

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

    auto gAudioIO = AudioIO::Get();
    if (gAudioIO->IsBusy()) {
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
    AudioIOStartStreamOptions sopts = ProjectAudioIO::GetDefaultOptions(project, options.isDefaultPolicy);

    int token = audioEngine()->startStream(seqs, startTime, endTime, mixerEndTime, sopts);
    bool success = token != 0;
    if (success) {
        ProjectAudioIO::Get(project).SetAudioIOToken(token);
    }

    return success ? muse::make_ok() : muse::make_ret(muse::Ret::Code::InternalError);
}

void Au3Player::seek(const muse::secs_t newPosition, bool applyIfPlaying)
{
    LOGD() << "newPosition: " << newPosition;

    Au3Project& project = projectRef();

    auto& playRegion = ViewInfo::Get(project).playRegion;
    playRegion.Clear();
    playRegion.SetStart(newPosition);

    if (applyIfPlaying && m_playbackStatus.val == PlaybackStatus::Running) {
        auto gAudioIO = AudioIO::Get();
        gAudioIO->SeekStream(newPosition - gAudioIO->GetStreamTime());
    }

    m_playbackPosition.set(newPosition);
}

void Au3Player::rewind()
{
    seek(0.0);
}

void Au3Player::stop()
{
    m_playbackStatus.set(PlaybackStatus::Stopped);

    //! NOTE: copied from ProjectAudioManager::Stop
    bool stopStream = true;

    if (!canStopAudioStream()) {
        return;
    }

    auto gAudioIO = AudioIO::Get();

    if (stopStream) {
        gAudioIO->StopStream();
    }

    //Make sure you tell gAudioIO to unpause
    gAudioIO->SetPaused(false);

    // So that we continue monitoring after playing or recording.
    // also clean the MeterQueues
    Au3Project& project = projectRef();
    auto& projectAudioIO = ProjectAudioIO::Get(project);
    auto meter = projectAudioIO.GetPlaybackMeter();
    if (meter) {
        meter->Clear();
    }

    meter = projectAudioIO.GetCaptureMeter();
    if (meter) {
        meter->Clear();
    }

    while (isBusy()) {
        using namespace std::chrono;
        std::this_thread::sleep_for(100ms);
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

    auto gAudioIO = AudioIO::Get();

    gAudioIO->SetPaused(false);

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

    return { playRegion.GetStart(), playRegion.GetEnd() };
}

void Au3Player::setPlaybackRegion(const PlaybackRegion& region)
{
    Au3Project& project = projectRef();

    auto& playRegion = ViewInfo::Get(project).playRegion;
    playRegion.SetStart(region.start);

    if (region.start == region.end) {
        auto& tracks = Au3TrackList::Get(project);
        playRegion.SetEnd(tracks.GetEndTime());
    } else {
        playRegion.SetEnd(region.end);
    }

    m_playbackPosition.set(region.start);
}

muse::async::Promise<bool> Au3Player::setLoop(const muse::secs_t from, const muse::secs_t to)
{
    UNUSED(from);
    UNUSED(to);
    NOT_IMPLEMENTED;

    return muse::async::Promise<bool>([](auto, auto reject) {
        muse::Ret ret = make_ret(muse::Ret::Code::NotImplemented);
        return reject(ret.code(), ret.text());
    });
}

void Au3Player::resetLoop()
{
    NOT_IMPLEMENTED;
}

void Au3Player::updatePlaybackState()
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

    //LOGDA() << "token: " << token << ", isActive: " << isActive << ", time: " << time;

    if (isActive) {
        m_reachedEnd.val = false;
        m_playbackPosition.set(std::max(0.0, time));
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
