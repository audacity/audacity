/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_AU3WRAP_IAU3PLAYBACK_H
#define AU_AU3WRAP_IAU3PLAYBACK_H

#include "global/async/promise.h"
#include "global/async/channel.h"

#include "modularity/imoduleinterface.h"

#include "playback/audiotypes.h"

#include "iau3audiooutput.h"

namespace au::au3 {
class IAu3Playback : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAu3Playback)
public:
    virtual ~IAu3Playback() = default;

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

    virtual std::shared_ptr<IAu3AudioOutput> audioOutput() const = 0;
};
using IAu3PlaybackPtr = std::shared_ptr<IAu3Playback>;
}

#endif // AU_AU3WRAP_IAU3PLAYBACK_H
