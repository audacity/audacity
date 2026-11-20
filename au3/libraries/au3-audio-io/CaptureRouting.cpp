/*
* Audacity: A Digital Audio Editor
*/
#include "CaptureRouting.h"

#include <algorithm>

CaptureRouting MakeCaptureRouting(size_t inputChannels, size_t trackChannels)
{
    CaptureRouting routing;
    if (inputChannels == 0 || trackChannels == 0) {
        return routing;
    }

    routing.trackChannelSources.resize(trackChannels);

    const bool canMix = inputChannels <= 2 && trackChannels <= 2;

    if (canMix && inputChannels == 2 && trackChannels == 1) {
        routing.needsMixdown = true;
        routing.trackChannelSources[0] = { 0, 1 };
    } else if (canMix && inputChannels == 1 && trackChannels == 2) {
        routing.trackChannelSources[0] = { 0 };
        routing.trackChannelSources[1] = { 0 };
    } else {
        for (size_t channel = 0; channel < std::min(trackChannels, inputChannels); ++channel) {
            routing.trackChannelSources[channel] = { channel };
        }
    }

    return routing;
}
