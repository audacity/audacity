/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../iplayer.h"

#include "global/async/asyncable.h"
#include "global/types/retval.h"
#include "global/timer.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

class AudacityProject;
class TrackList;
struct TransportSequences;

namespace au::playback {
class Au3Player : public IPlayer, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:

    Au3Player();

    void play() override;
    void seek(const audio::secs_t newPosition) override;
    void stop() override;
    void pause() override;
    void resume() override;

    audio::PlaybackStatus playbackStatus() const override;
    muse::async::Channel<audio::PlaybackStatus> playbackStatusChanged() const override;

    muse::async::Promise<bool> setLoop(const audio::secs_t from, const audio::secs_t toM) override;
    void resetLoop() override;

    audio::secs_t playbackPosition() const override;
    muse::async::Channel<audio::secs_t> playbackPositionChanged() const override;

private:
    AudacityProject& projectRef() const;

    bool canStopAudioStream() const;
    TransportSequences makeTransportTracks(TrackList& trackList, bool selectedOnly, bool nonWaveToo);

    void updatePlaybackPosition();

    muse::ValCh<audio::PlaybackStatus> m_playbackStatus;

    muse::Timer m_positionUpdateTimer;
    muse::ValCh<audio::secs_t> m_playbackPosition;
};
}
