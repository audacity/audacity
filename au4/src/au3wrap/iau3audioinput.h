/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_IAU3AUDIOINPUT_H
#define AU_AU3WRAP_IAU3AUDIOINPUT_H

#include <memory>

#include "global/async/promise.h"
#include "global/async/channel.h"

#include "playback/audiotypes.h"

namespace au::au3 {
class IAu3AudioInput
{
public:
    virtual ~IAu3AudioInput() = default;

    virtual muse::async::Promise<float> recordVolume() const = 0;
    virtual void setRecordVolume(float volume) = 0;
    virtual muse::async::Channel<float> recordVolumeChanged() const = 0;

    virtual muse::async::Promise<muse::async::Channel<audio::audioch_t, audio::AudioSignalVal>> recordSignalChanges() const = 0;
};

using IAu3AudioInputPtr = std::shared_ptr<IAu3AudioInput>;
}

#endif // AU_AU3WRAP_IAU3AUDIOINPUT_H
