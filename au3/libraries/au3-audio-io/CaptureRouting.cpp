/*
* Audacity: A Digital Audio Editor
*/
#include "CaptureRouting.h"

namespace {
bool isStereoToMonoMixdown(size_t inputChannels, size_t trackChannels)
{
    return inputChannels == 2 && trackChannels == 1;
}

bool isMonoToStereoDuplication(size_t inputChannels, size_t trackChannels)
{
    return inputChannels == 1 && trackChannels == 2;
}
}

CaptureChannelSources GetCaptureChannelSources(size_t inputChannels, size_t trackChannels, size_t trackChannel)
{
    CaptureChannelSources result;
    if (inputChannels == 0 || trackChannel >= trackChannels) {
        return result;
    }

    if (isStereoToMonoMixdown(inputChannels, trackChannels)) {
        result.sources = { 0, 1 };
        result.count = 2;
    } else if (isMonoToStereoDuplication(inputChannels, trackChannels)) {
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
    routing.needsMixdown = isStereoToMonoMixdown(inputChannels, trackChannels);

    return routing;
}
