/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_AU3WRAP_IAUDACITY3PLAYBACK_H
#define AU_AU3WRAP_IAUDACITY3PLAYBACK_H

#include "global/async/promise.h"
#include "global/async/channel.h"

#include "modularity/imoduleinterface.h"

#include "playback/audiotypes.h"

class AudacityProject;

namespace au::au3 {
class IAudacity3Playback : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAudacity3Playback)
public:
    virtual ~IAudacity3Playback() = default;

    virtual void play() = 0;
    virtual void seek(const audio::msecs_t newPositionMsecs) = 0;
    virtual void stop() = 0;
    virtual void pause() = 0;
    virtual void resume() = 0;

    virtual void setDuration(const audio::msecs_t durationMsec) = 0;
    virtual muse::async::Promise<bool> setLoop(const audio::msecs_t fromMsec, const audio::msecs_t toMsec) = 0;
    virtual void resetLoop() = 0;

    virtual muse::async::Channel<audio::msecs_t> playbackPositionMsecs() const = 0;
    virtual muse::async::Channel<audio::PlaybackStatus> playbackStatusChanged() const = 0;
};
using IAudacity3PlaybackPtr = std::shared_ptr<IAudacity3Playback>;
}

#endif // AU_AU3WRAP_IAUDACITY3PLAYBACK_H
