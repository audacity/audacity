/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iaudioengine.h"

namespace au::audio {
class AudioEngine : public IAudioEngine
{
public:
    AudioEngine() = default;

    bool isBusy() const override;

    int startStream(const TransportSequences& sequences, double t0, double t1, double mixerLimit,
                    const AudioIOStartStreamOptions& options) override;
};
}
