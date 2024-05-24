#pragma once

#include <map>

#include "../itracksgeometrycontroller.h"

namespace au::projectscene {
class TracksGeometryController : public ITracksGeometryController
{
public:
    TracksGeometryController() = default;

    muse::ValCh<int> trackHeight(const processing::TrackId& trackId) const override;

    void changeTrackHeight(const processing::TrackId& trackId, int deltaY) override;

private:

    mutable std::map<processing::TrackId, muse::ValCh<int /*height*/>> m_data;
};
}
