/*
* Audacity: A Digital Audio Editor
*/
#include "CaptureRouting.h"

CaptureChannelSources GetCaptureChannelSources(size_t inputChannels, size_t trackChannels, size_t trackChannel)
{
    CaptureChannelSources result;
    if (inputChannels == 0 || trackChannel >= trackChannels) {
        return result;
    }

    const bool canMix = inputChannels <= 2 && trackChannels <= 2;

    if (canMix && inputChannels == 2 && trackChannels == 1) {
        result.sources = { 0, 1 };
        result.count = 2;
    } else if (canMix && inputChannels == 1 && trackChannels == 2) {
        result.sources[0] = 0;
        result.count = 1;
    } else if (trackChannel < inputChannels) {
        result.sources[0] = trackChannel;
        result.count = 1;
    }

    return result;
}

CaptureRouting MakeCaptureRouting(size_t inputChannels, size_t trackChannels)
{
    CaptureRouting routing;
    if (inputChannels == 0 || trackChannels == 0) {
        return routing;
    }

    routing.trackChannels.reserve(trackChannels);
    for (size_t channel = 0; channel < trackChannels; ++channel) {
        routing.trackChannels.push_back(GetCaptureChannelSources(inputChannels, trackChannels, channel));
    }
    routing.needsMixdown = inputChannels == 2 && trackChannels == 1;

    return routing;
}
