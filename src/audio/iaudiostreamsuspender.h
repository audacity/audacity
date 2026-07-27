/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>

#include "framework/global/modularity/imoduleinterface.h"

#include "audioconfigurationtypes.h"

namespace au::audio {
using AudioStreamRestorer = std::function<bool ()>;

class IAudioStreamSuspender : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAudioStreamSuspender)

public:
    virtual ~IAudioStreamSuspender() = default;

    // A non-empty result guarantees that the physical stream is stopped.
    // Invoking it restores transport state when appropriate.
    virtual AudioStreamRestorer suspendForAudioConfiguration(AudioStreamKind streamKind) = 0;
};
}
