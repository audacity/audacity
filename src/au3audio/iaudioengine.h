/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/modularity/imoduleinterface.h"

struct TransportSequences;
struct AudioIOStartStreamOptions;
namespace au::audio {
class IAudioEngine : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAudioEngine);
public:
    virtual ~IAudioEngine() = default;

    virtual bool isBusy() const = 0;

    virtual int startStream(const TransportSequences& sequences, double startTime, double endTime,
                            double mixerEndTime, // Time at which mixer stops producing, maybe > endTime
                            const AudioIOStartStreamOptions& options) = 0;
};
}
