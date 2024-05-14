/*
* Audacity: A Digital Audio Editor
*/
#include "au3playback.h"

#include "libraries/lib-time-frequency-selection/SelectedRegion.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-stretching-sequence/StretchingSequence.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"

#include "libraries/lib-audio-io/AudioIO.h"

#include "wxtypes_convert.h"

#include "internal/au3audiooutput.h"

#include "log.h"

using namespace au::au3;

void Au3Playback::init()
{
    m_audioOutputPtr = std::make_shared<Au3AudioOutput>();
}

void Au3Playback::play()
{
    //! NOTE: copied from ProjectAudioManager::PlayPlayRegion

    AudacityProject& project = projectRef();

    auto options = ProjectAudioIO::GetDefaultOptions(project, true /*newDefault*/);
    bool backwards = false;

    auto& tracks = TrackList::Get(project);

    auto& playRegion = ViewInfo::Get(project).playRegion;
    playRegion.SetStart(tracks.GetStartTime());
    playRegion.SetEnd(tracks.GetEndTime());

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
}

void Au3Playback::seek(const audio::msecs_t newPositionMsecs)
{
    AudacityProject& project = projectRef();

    auto& playRegion = ViewInfo::Get(project).playRegion;
    playRegion.SetStart(newPositionMsecs);
}

void Au3Playback::stop()
{
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
}

void Au3Playback::pause()
{
    if (!canStopAudioStream()) {
        return;
    }

    auto gAudioIO = AudioIO::Get();

    gAudioIO->SetPaused(true);
}

void Au3Playback::resume()
{
    if (!canStopAudioStream()) {
        return;
    }

    auto gAudioIO = AudioIO::Get();

    gAudioIO->SetPaused(false);
}

void Au3Playback::setDuration(const audio::msecs_t durationMsec)
{
    UNUSED(durationMsec);
    NOT_IMPLEMENTED;
}

muse::async::Promise<bool> Au3Playback::setLoop(const audio::msecs_t fromMsec, const audio::msecs_t toMsec)
{
    UNUSED(fromMsec);
    UNUSED(toMsec);
    NOT_IMPLEMENTED;

    return muse::async::Promise<bool>([](auto, auto reject) {
        muse::Ret ret = make_ret(muse::Ret::Code::NotImplemented);
        return reject(ret.code(), ret.text());
    });
}

void Au3Playback::resetLoop()
{
}

muse::async::Channel<au::audio::msecs_t> Au3Playback::playbackPositionMsecs() const
{
    return m_playbackPositionMsecsChanged;
}

muse::async::Channel<au::audio::PlaybackStatus> Au3Playback::playbackStatusChanged() const
{
    return m_playbackStatusChanged;
}

IAu3AudioOutputPtr Au3Playback::audioOutput() const
{
    return m_audioOutputPtr;
}

AudacityProject& Au3Playback::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

bool Au3Playback::canStopAudioStream() const
{
    auto gAudioIO = AudioIO::Get();
    AudacityProject& project = projectRef();
    return !gAudioIO->IsStreamActive()
           || gAudioIO->IsMonitoring()
           || gAudioIO->GetOwningProject().get() == &project;
}

TransportSequences Au3Playback::makeTransportTracks(TrackList& trackList, bool selectedOnly, bool nonWaveToo)
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
