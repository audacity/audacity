/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <array>
#include <cstddef>
#include <span>
#include <vector>

//! For each recorded track channel, the input channels it takes; several inputs are averaged
struct AUDIO_IO_API CaptureRouting
{
    std::vector<std::vector<size_t> > trackChannelSources;
    bool needsMixdown{ false };
};

AUDIO_IO_API CaptureRouting MakeCaptureRouting(size_t inputChannels, size_t trackChannels);

//! The input channels one track channel takes, without allocating: safe to use in the audio callback
struct AUDIO_IO_API CaptureChannelSources
{
    std::array<size_t, 2> sources{};
    size_t count{ 0 };

    std::span<const size_t> span() const { return { sources.data(), count }; }
};

AUDIO_IO_API CaptureChannelSources GetCaptureChannelSources(size_t inputChannels, size_t trackChannels, size_t trackChannel);
