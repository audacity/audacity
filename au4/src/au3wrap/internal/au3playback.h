/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_AU3WRAP_AU3PLAYBACK_H
#define AU_AU3WRAP_AU3PLAYBACK_H

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"

#include "iau3playback.h"
#include "iau3audiooutput.h"

class AudacityProject;
class TrackList;
struct TransportSequences;

namespace au::au3 {
class Au3Playback : public IAu3Playback, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    void init();

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

    IAu3AudioOutputPtr audioOutput() const override;

private:
    AudacityProject& projectRef() const;

    bool canStopAudioStream() const;
    TransportSequences makeTransportTracks(TrackList& trackList, bool selectedOnly, bool nonWaveToo);

    mutable muse::async::Channel<audio::msecs_t> m_playbackPositionMsecsChanged;
    mutable muse::async::Channel<audio::PlaybackStatus> m_playbackStatusChanged;

    IAu3AudioOutputPtr m_audioOutputPtr;
};
}

#endif // AU_AU3WRAP_AU3PLAYBACK_H
