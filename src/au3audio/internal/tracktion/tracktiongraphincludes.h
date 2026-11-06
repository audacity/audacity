/*
* Audacity: A Digital Audio Editor
*/
#pragma once

// Qt's keyword macros clash with identifiers in tracktion_graph; the includers don't use them
#include "../juce/juceincludes.h"
#include <tracktion_graph/tracktion_graph.h>

namespace au::au4tracktion {
inline float* channelPointer(const choc::buffer::ChannelArrayView<float>& view, int channel)
{
    return view.data.channels[channel] + view.data.offset;
}
}
