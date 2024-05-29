/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_AU3WRAP_AU3PLAYBACK_H
#define AU_AU3WRAP_AU3PLAYBACK_H

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "playback/iplayer.h"

class AudacityProject;
class TrackList;
struct TransportSequences;

namespace au::au3 {
class Au3Player : public playback::IPlayer, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    void play() override;
    void seek(const audio::secs_t newPosition) override;
    void stop() override;
    void pause() override;
    void resume() override;
    muse::async::Channel<audio::PlaybackStatus> playbackStatusChanged() const override;

    muse::async::Promise<bool> setLoop(const audio::secs_t from, const audio::secs_t toM) override;
    void resetLoop() override;

    muse::async::Promise<audio::secs_t> playbackPosition() const override;
    muse::async::Channel<audio::secs_t> playbackPositionChanged() const override;

private:
    AudacityProject& projectRef() const;

    bool canStopAudioStream() const;
    TransportSequences makeTransportTracks(TrackList& trackList, bool selectedOnly, bool nonWaveToo);

    mutable muse::async::Channel<audio::secs_t> m_playbackPositionChanged;
    mutable muse::async::Channel<audio::PlaybackStatus> m_playbackStatusChanged;
};
}

#endif // AU_AU3WRAP_AU3PLAYBACK_H
