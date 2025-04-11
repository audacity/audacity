/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "trackedit/itrackdata.h"
#include "au3wrap/au3types.h"

namespace au::trackedit {
class Au3TrackData : public ITrackData
{
public:
    explicit Au3TrackData(std::shared_ptr<au3::Au3Track> track);

    ~Au3TrackData() override = default;

    const std::shared_ptr<au3::Au3Track>& track() const { return m_track; }

private:
    std::shared_ptr<au3::Au3Track> m_track;
};
}
