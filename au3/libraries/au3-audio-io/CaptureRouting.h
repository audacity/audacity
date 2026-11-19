/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstddef>
#include <vector>

//! For each recorded track channel, the input channels it takes; several inputs are averaged
struct AUDIO_IO_API CaptureRouting
{
    std::vector<std::vector<size_t> > trackChannelSources;
    bool needsMixdown{ false };
};

AUDIO_IO_API CaptureRouting MakeCaptureRouting(size_t inputChannels, size_t trackChannels);
