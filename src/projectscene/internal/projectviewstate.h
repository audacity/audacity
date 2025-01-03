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

    virtual muse::ValCh<bool> tracksVerticalScrollLocked() const override;
    virtual void setTracksVerticalScrollLocked(bool lock) override;

    // context of track
    muse::ValCh<int> trackHeight(const trackedit::TrackId& trackId) const override;
    muse::ValCh<bool> isTrackCollapsed(const trackedit::TrackId& trackId) const override;
    void changeTrackHeight(const trackedit::TrackId& trackId, int deltaY) override;

    bool isSnapEnabled() const override;
    void setIsSnapEnabled(bool enabled) override;

    SnapType snapType() const override;
    void setSnapType(SnapType type) override;

    bool isSnapTripletsEnabled() const override;
    void setIsSnapTripletsEnabled(bool enabled) override;

    void setSnap(const Snap& s) override;
    muse::ValCh<Snap> snap() const override;

private:
    struct TrackData {
        muse::ValCh<int> height;
        muse::ValCh<bool> collapsed;
    };

    TrackData& makeTrackData(const trackedit::TrackId& trackId) const;

    muse::ValCh<int> m_tracksVericalY;
    muse::ValCh<bool> m_tracksVerticalScrollLocked;

    mutable std::map<trackedit::TrackId, TrackData> m_tracks;
    muse::ValCh<Snap> m_snap;
};
}  // namespace au::projectscene
