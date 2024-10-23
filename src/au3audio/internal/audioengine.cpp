#include "audioengine.h"

#include "libraries/lib-audio-io/AudioIO.h"

using namespace au::audio;

bool AudioEngine::isBusy() const
{
    return AudioIO::Get()->IsBusy();
}

int AudioEngine::startStream(const TransportSequences& sequences, double t0, double t1, double mixerLimit,
                             const AudioIOStartStreamOptions& options)
{
    return AudioIO::Get()->StartStream(sequences, t0, t1, mixerLimit, options);
}
