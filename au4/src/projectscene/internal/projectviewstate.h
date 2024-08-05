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
    muse::ValCh<int> trackHeight(const trackedit::TrackId& trackId) const override;
    muse::ValCh<bool> isTrackCollapsed(const trackedit::TrackId& trackId) const override;
    void changeTrackHeight(const trackedit::TrackId& trackId, int deltaY) override;

    muse::ValCh<bool> isSnapEnabled() const override;
    void setIsSnapEnabled(bool enabled) override;

    muse::ValCh<SnapType> snapType() const override;
    void setSnapType(SnapType type) override;

    muse::ValCh<bool> isSnapTripletsEnabled() const override;
    void setIsSnapTripletsEnabled(bool enabled) override;

private:

    struct TrackData {
        muse::ValCh<int> height;
        muse::ValCh<bool> collapsed;
    };

    TrackData& makeTrackData(const trackedit::TrackId& trackId) const;

    muse::ValCh<int> m_tracksVericalY;
    mutable std::map<trackedit::TrackId, TrackData> m_tracks;

    muse::ValCh<bool> m_isSnapEnabled;
    muse::ValCh<SnapType> m_snapType;
    muse::ValCh<bool> m_isSnapTripletsEnabled;
};
}
