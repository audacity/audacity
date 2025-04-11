/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "trackedittypes.h"

#include <memory>
#include <variant>

namespace au::trackedit {
class ITrackData
{
public:
    virtual ~ITrackData() = default;
};

using ITrackDataPtr = std::shared_ptr<ITrackData>;
}
