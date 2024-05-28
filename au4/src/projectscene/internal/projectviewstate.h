#pragma once

#include "../iprojectviewstate.h"

namespace au::projectscene {
class ProjectViewState : public IProjectViewState
{
public:
    ProjectViewState() = default;

    // context of all tracks
    muse::ValCh<int> tracksVericalY() const override;
    void changeTracksVericalY(int deltaY) override;

    // context of track
    muse::ValCh<int> trackHeight(const processing::TrackId& trackId) const override;
    void changeTrackHeight(const processing::TrackId& trackId, int deltaY) override;

private:

    muse::ValCh<int> m_tracksVericalY;
    mutable std::map<processing::TrackId, muse::ValCh<int /*height*/>> m_tracks;
};
}
