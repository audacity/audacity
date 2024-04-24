/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_AU3WRAP_AUDACITY3PLAYBACK_H
#define AU_AU3WRAP_AUDACITY3PLAYBACK_H

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"

#include "iaudacity3playback.h"

class AudacityProject;
class TrackList;
struct TransportSequences;

namespace au::au3 {
class Audacity3Playback : public IAudacity3Playback, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    void play() override;
    void seek(const audio::msecs_t newPositionMsecs) override;
    void stop() override;
    void pause() override;
    void resume() override;

    void setDuration(const audio::msecs_t durationMsec) override;
    muse::async::Promise<bool> setLoop(const audio::msecs_t fromMsec, const audio::msecs_t toMsec) override;
    void resetLoop() override;

    muse::async::Channel<audio::msecs_t> playbackPositionMsecs() const override;
    muse::async::Channel<audio::PlaybackStatus> playbackStatusChanged() const override;

private:
    AudacityProject& projectRef() const;

    bool canStopAudioStream() const;
    TransportSequences makeTransportTracks(TrackList& trackList, bool selectedOnly, bool nonWaveToo);

    mutable muse::async::Channel<audio::msecs_t> m_playbackPositionMsecsChanged;
    mutable muse::async::Channel<audio::PlaybackStatus> m_playbackStatusChanged;
};
}

#endif // AU_AU3WRAP_AUDACITY3PLAYBACK_H
