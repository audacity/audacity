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
#include "au3audio/iaudioengine.h"

#include "au3wrap/au3types.h"

struct TransportSequences;
namespace au::playback {
class Au3Player : public IPlayer, public muse::async::Asyncable
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<audio::IAudioEngine> audioEngine;

public:

    Au3Player();

    void play() override;
    void seek(const muse::secs_t newPosition) override;
    void stop() override;
    void pause() override;
    void resume() override;

    PlaybackStatus playbackStatus() const override;
    muse::async::Channel<PlaybackStatus> playbackStatusChanged() const override;

    muse::async::Promise<bool> setLoop(const muse::secs_t from, const muse::secs_t toM) override;
    void resetLoop() override;

    muse::secs_t playbackPosition() const override;
    muse::async::Channel<muse::secs_t> playbackPositionChanged() const override;

    int playTracks(TrackList& trackList, double t0, double t1, const PlayTracksOptions& options = {}) override;

private:
    au3::Au3Project& projectRef() const;

    bool canStopAudioStream() const;
    TransportSequences makeTransportTracks(au3::Au3TrackList& trackList, bool selectedOnly, bool nonWaveToo);

    void updatePlaybackPosition();

    muse::ValCh<PlaybackStatus> m_playbackStatus;

    muse::Timer m_positionUpdateTimer;
    muse::ValCh<muse::secs_t> m_playbackPosition;
};
}
