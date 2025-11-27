/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

namespace au::trackedit {
class ITrackData
{
public:
    virtual ~ITrackData() = default;
};

using ITrackDataPtr = std::shared_ptr<ITrackData>;
}
