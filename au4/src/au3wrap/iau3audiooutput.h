/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_IAUDACITYAUDIOOUTPUT_H
#define AU_AU3WRAP_IAUDACITYAUDIOOUTPUT_H

#include <memory>

#include "global/async/promise.h"
#include "global/async/channel.h"

#include "playback/audiotypes.h"

namespace au::au3 {
class IAu3AudioOutput
{
public:
    virtual ~IAu3AudioOutput() = default;

    virtual muse::async::Promise<float> playbackVolume() const = 0;
    virtual void setPlaybackVolume(float volume) = 0;
    virtual muse::async::Channel<float> playbackVolumeChanged() const = 0;

    virtual muse::async::Promise<au::audio::AudioSignalChanges> playbackSignalChanges() const = 0;
};

using IAu3AudioOutputPtr = std::shared_ptr<IAu3AudioOutput>;
}

#endif // AU_AU3WRAP_IAUDACITYAUDIOOUTPUT_H
