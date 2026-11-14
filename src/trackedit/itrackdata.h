/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "trackedittypes.h"

namespace au::trackedit {
class ITrackData
{
public:
    virtual ~ITrackData() = default;

    virtual secs_t endTime() const = 0;
};

using ITrackDataPtr = std::shared_ptr<ITrackData>;
}
