/*
* Audacity: A Digital Audio Editor
*/
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
    muse::ValCh<bool> isTrackCollapsed(const processing::TrackId& trackId) const override;
    void changeTrackHeight(const processing::TrackId& trackId, int deltaY) override;

private:

    struct TrackData {
        muse::ValCh<int> height;
        muse::ValCh<bool> collapsed;
    };

    TrackData& makeTrackData(const processing::TrackId& trackId) const;

    muse::ValCh<int> m_tracksVericalY;
    mutable std::map<processing::TrackId, TrackData> m_tracks;
};
}
