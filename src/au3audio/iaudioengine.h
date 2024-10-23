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

    virtual int startStream(const TransportSequences& sequences, double t0, double t1, double mixerLimit, //!< Time at which mixer stops producing, maybe > t1
                            const AudioIOStartStreamOptions& options);
};
}
