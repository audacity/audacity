/*
* Audacity: A Digital Audio Editor
*/
#include "au3player.h"

#include "libraries/lib-time-frequency-selection/SelectedRegion.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-stretching-sequence/StretchingSequence.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"
#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-devices/Meter.h"

#include "wxtypes_convert.h"

#include "log.h"

using namespace au::au3;

Au3Player::Au3Player()
    : m_positionUpdateTimer(std::chrono::microseconds(1000))
{
    m_positionUpdateTimer.onTimeout(this, [this]() {
        updatePlaybackPosition();
    });

    m_playbackStatus.ch.onReceive(this, [this](audio::PlaybackStatus st) {
        if (st == audio::PlaybackStatus::Running) {
            m_positionUpdateTimer.start();
        } else {
            m_positionUpdateTimer.stop();
        }
    });
}

void Au3Player::play()
{
    if (m_playbackStatus.val == audio::PlaybackStatus::Paused) {
        auto gAudioIO = AudioIO::Get();
        gAudioIO->SetPaused(false);

        m_playbackStatus.set(audio::PlaybackStatus::Running);
        return;
    }

    //! NOTE: copied from ProjectAudioManager::PlayPlayRegion

    AudacityProject& project = projectRef();

    auto options = ProjectAudioIO::GetDefaultOptions(project, true /*newDefault*/);
    bool backwards = false;

    auto& tracks = TrackList::Get(project);

    auto& playRegion = ViewInfo::Get(project).playRegion;

    SelectedRegion selectedRegion(playRegion.GetStart(), playRegion.GetEnd());

    if (!canStopAudioStream()) {
        return /*-1*/;
    }

    auto& pStartTime = options.pStartTime;

    bool nonWaveToo = options.playNonWaveTracks;

    double t0 = selectedRegion.t0();
    double t1 = selectedRegion.t1();
    // SelectedRegion guarantees t0 <= t1, so we need another boolean argument
    // to indicate backwards play.
    const bool newDefault = false; //(mode == PlayMode::loopedPlay);

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
        hasaudio = !tracks.Any<WaveTrack>().empty();
    }

    double latestEnd = tracks.GetEndTime();

    if (!hasaudio) {
        return /*-1*/;  // No need to continue without audio tracks
    }
#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
    double initSeek = 0.0;
#endif
    double loopOffset = 0.0;

    if (t1 == t0) {
        if (newDefault) {
            const auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
            // play selection if there is one, otherwise
            // set start of play region to project start,
            // and loop the project from current play position.

            if ((t0 > selectedRegion.t0()) && (t0 < selectedRegion.t1())) {
                t0 = selectedRegion.t0();
                t1 = selectedRegion.t1();
            } else {
                // loop the entire project
                // Bug2347, loop playback from cursor position instead of project start
                loopOffset = t0 - tracks.GetStartTime();
                if (!pStartTime) {
                    // TODO move this reassignment elsewhere so we don't need an
                    // ugly mutable member
                    pStartTime.emplace(loopOffset);
                }
                t0 = tracks.GetStartTime();
                t1 = tracks.GetEndTime();
            }
        } else {
            // move t0 to valid range
            if (t0 < 0) {
                t0 = tracks.GetStartTime();
            } else if (t0 > tracks.GetEndTime()) {
                t0 = tracks.GetEndTime();
            }
#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
            else {
                initSeek = t0;         //AC: initSeek is where playback will 'start'
                if (!pStartTime) {
                    pStartTime.emplace(initSeek);
                }
                t0 = tracks.GetStartTime();
            }
#endif
        }
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

    int token = -1;

    if (t1 != t0) {
        if (cutpreview) {
            const double tless = std::min(t0, t1);
            const double tgreater = std::max(t0, t1);
            double beforeLen, afterLen;
            gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
            gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);
            double tcp0 = tless - beforeLen;
            const double diff = tgreater - tless;
            double tcp1 = tgreater + afterLen;
            if (backwards) {
                std::swap(tcp0, tcp1);
            }
            AudioIOStartStreamOptions myOptions = options;
            // myOptions.policyFactory
            //     =[tless, diff](auto&) -> std::unique_ptr<PlaybackPolicy> {
            //     return std::make_unique<CutPreviewPlaybackPolicy>(tless, diff);
            // };
            token = gAudioIO->StartStream(
                makeTransportTracks(TrackList::Get(project), false, nonWaveToo),
                tcp0, tcp1, tcp1, myOptions);
        } else {
            double mixerLimit = t1;
            if (newDefault) {
                mixerLimit = latestEnd;
                if (pStartTime && *pStartTime >= t1) {
                    t1 = latestEnd;
                }
            }
            token = gAudioIO->StartStream(
                makeTransportTracks(tracks, false, nonWaveToo),
                t0, t1, mixerLimit, options);
        }
        if (token != 0) {
            ProjectAudioIO::Get(project).SetAudioIOToken(token);
        } else {
            // XO("Error opening sound device.\nTry changing the audio host, playback device and the project sample rate."),
        }
    }

    m_playbackStatus.set(audio::PlaybackStatus::Running);
}

void Au3Player::seek(const audio::secs_t newPosition)
{
    LOGD() << "newPosition: " << newPosition;

    //! TODO At the moment not work
    //! there probably should be a different implementation

    AudacityProject& project = projectRef();

    auto& playRegion = ViewInfo::Get(project).playRegion;
    playRegion.SetStart(newPosition);

    m_playbackPosition.set(newPosition);
}

void Au3Player::stop()
{
    m_playbackStatus.set(audio::PlaybackStatus::Stopped);

    //! NOTE: copied from ProjectAudioManager::Stop
    bool stopStream = true;

    if (!canStopAudioStream()) {
        return;
    }

    auto gAudioIO = AudioIO::Get();

    if (stopStream) {
        gAudioIO->StopStream();
    }

#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
    gAudioIO->AILADisable();
#endif

    //Make sure you tell gAudioIO to unpause
    gAudioIO->SetPaused(false);

    // So that we continue monitoring after playing or recording.
    // also clean the MeterQueues
    AudacityProject& project = projectRef();
    auto& projectAudioIO = ProjectAudioIO::Get(project);
    auto meter = projectAudioIO.GetPlaybackMeter();
    if (meter) {
        meter->Clear();
    }

    meter = projectAudioIO.GetCaptureMeter();
    if (meter) {
        meter->Clear();
    }
}

void Au3Player::pause()
{
    if (!canStopAudioStream()) {
        return;
    }

    auto gAudioIO = AudioIO::Get();

    gAudioIO->SetPaused(true);

    m_playbackStatus.set(audio::PlaybackStatus::Paused);
}

void Au3Player::resume()
{
    if (!canStopAudioStream()) {
        return;
    }

    auto gAudioIO = AudioIO::Get();

    gAudioIO->SetPaused(false);

    m_playbackStatus.set(audio::PlaybackStatus::Running);
}

au::audio::PlaybackStatus Au3Player::playbackStatus() const
{
    return m_playbackStatus.val;
}

muse::async::Channel<au::audio::PlaybackStatus> Au3Player::playbackStatusChanged() const
{
    return m_playbackStatus.ch;
}

muse::async::Promise<bool> Au3Player::setLoop(const audio::secs_t from, const audio::secs_t to)
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

void Au3Player::updatePlaybackPosition()
{
    m_playbackPosition.set(std::max(0.0, AudioIO::Get()->GetStreamTime()));
}

au::audio::secs_t Au3Player::playbackPosition() const
{
    return m_playbackPosition.val;
}

muse::async::Channel<au::audio::secs_t> Au3Player::playbackPositionChanged() const
{
    return m_playbackPosition.ch;
}

AudacityProject& Au3Player::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

bool Au3Player::canStopAudioStream() const
{
    auto gAudioIO = AudioIO::Get();
    AudacityProject& project = projectRef();
    return !gAudioIO->IsStreamActive()
           || gAudioIO->IsMonitoring()
           || gAudioIO->GetOwningProject().get() == &project;
}

TransportSequences Au3Player::makeTransportTracks(TrackList& trackList, bool selectedOnly, bool nonWaveToo)
{
    TransportSequences result;
    {
        const auto range = trackList.Any<WaveTrack>()
                           + (selectedOnly ? &Track::IsSelected : &Track::Any);
        for (auto pTrack : range) {
            result.playbackSequences.push_back(
                StretchingSequence::Create(*pTrack, pTrack->GetClipInterfaces()));
        }
    }
#ifdef EXPERIMENTAL_MIDI_OUT
    if (nonWaveToo) {
        const auto range = trackList.Any<const PlayableTrack>()
                           + (selectedOnly ? &Track::IsSelected : &Track::Any);
        for (auto pTrack : range) {
            if (!track_cast<const SampleTrack*>(pTrack)) {
                if (auto pSequence
                        =std::dynamic_pointer_cast<const OtherPlayableSequence>(
                              pTrack->shared_from_this())
                        ) {
                    result.otherPlayableSequences.push_back(pSequence);
                }
            }
        }
    }
#else
    UNUSED(nonWaveToo);
#endif
    return result;
}
