#include "audioengine.h"

#include "libraries/lib-audio-io/AudioIO.h"

using namespace au::audio;

bool AudioEngine::isBusy() const
{
    return AudioIO::Get()->IsBusy();
}

int AudioEngine::startStream(const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime,
                             const AudioIOStartStreamOptions& options)
{
    return AudioIO::Get()->StartStream(sequences, startTime, endTime, mixerEndTime, options);
}
